{-# LANGUAGE MultiWayIf
  , NegativeLiterals
  #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-| Time-stamp: <2018-04-08 17:24:22 CDT>

Module      : Lib
Copyright   : (c) Robert Lee
License     : All Rights Reserved

Maintainers : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Attempt to control xkeys devices from PI Engineering
Element     : Xenon

-}
{-
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6  +, -
infixr 6  <>
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixl 4  <$, <*>, <*, *>, <**>
infixr 3  &&
infixr 2  ||
infixl 1  ?, >>, >>=
infixr 1  =<<, <=<, >=>
infixr 0  $, $!, ‘seq‘

⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ Omega Symbol Key ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
                   early or abnormal termination ⋅⋅⋅ Ω
                            termination (normal) ⋅⋅⋅ ω
                                    a new thread ⋅⋅⋅ ⋔
          code that can throw an error exception ⋅⋅⋅ ⏈
                                  loop-like code ⋅⋅⋅ ➿
                              a loop-like repeat ⋅⋅⋅ ↺
                           end of loop-like code ⋅⋅⋅ 🔚
               an uninterruptible exception mask ⋅⋅⋅ ☔
                code that can emit IO exceptions ⋅⋅⋅ ☢
                a warning about troublesome code ⋅⋅⋅ ⚠
  an imperative concerning imprudent code change ⋅⋅⋅ ⚡
                  a forbidden/nonsense condition ⋅⋅⋅ ⛞
                          a timed race condition ⋅⋅⋅ 🏁
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}

module Lib
where

-- from base:
import Control.Exception
import Control.Concurrent.MVar
import System.IO
import System.Environment
import System.Exit
import Data.Functor
import Data.List
import Control.Monad
import Text.Printf

-- from bytestring:
import qualified Data.ByteString as B
import Data.Word (Word8)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent (threadDelay)
-- from vector:
import           Data.Vector      ( (!) )
import qualified Data.Vector as V ( toList )

-- from usb:
import System.USB

(?) :: Bool -> a -> a -> a
(?) True t _ = t
(?) False _ f = f

infixl 1 ?

libmain :: IO ()
libmain = do
  [vendorIdStr, productIdStr] <- getArgs
  let vendorId  = read vendorIdStr
      productId = read productIdStr

  -- Initialization:
  ctx <- newCtx
  setDebug ctx PrintWarnings -- PrintDebug
  putStrLn . show $ ("HasHotplug",ctx `hasCapability` HasHotplug)
  -- Device retrieval:
  dev <- if ctx `hasCapability` HasHotplug
         then waitForMyDevice ctx vendorId productId
         else findMyDevice ctx vendorId productId
  -- Device usage:
  putStrLn $ show dev
  catchUSBException (doSomethingWithDevice dev) (putStrLn . ("Caught " ++) . show)

waitForMyDevice :: Ctx -> VendorId -> ProductId -> IO Device
waitForMyDevice ctx vendorId productId = do
  putStrLn "Waiting for device attachment..."
  mv <- newEmptyMVar
  mask_ $ do
    h <- registerHotplugCallback ctx
                                 deviceArrived
                                 enumerate
                                 (Just vendorId)
                                 (Just productId)
                                 Nothing
                                 (\dev event ->
                                    tryPutMVar mv (dev, event) $>
                                      DeregisterThisCallback)
    void $ mkWeakMVar mv $ deregisterHotplugCallback h
  (dev, _event) <- takeMVar mv
  -- desc <- getDeviceDesc dev
  -- cDesc <- getConfigDesc dev 0
  putStrLn $ replicate 80 '-'
  -- putStrLn $ show desc
  -- putStrLn $ show cDesc
  return dev

-- Enumeratie all devices and find the right one.
findMyDevice :: Ctx -> VendorId -> ProductId -> IO Device
findMyDevice ctx vendorId productId = do
    devs <- V.toList <$> getDevices ctx
    deviceDescs <- mapM getDeviceDesc devs
    case fmap fst $ find (match . snd) $ zip devs deviceDescs of
      Nothing  -> hPutStrLn stderr "Mouse not found" >> exitFailure
      Just dev -> return dev
  where
    match :: DeviceDesc -> Bool
    match devDesc =  deviceVendorId  devDesc == vendorId
                  && deviceProductId devDesc == productId

-- DEVICE_MAP_ENTRY(vid   , pid   , interface, usage_page, usage , readlength, writelength)
-- DEVICE_MAP_ENTRY(PI_VID, 0x046A, 0        , 0x000c    , 0x0001, 33        , 36         ) /* XK-8 Stick splat read and write*/
-- DEVICE_MAP_ENTRY(PI_VID, 0x046A, 1        , 0x0001    , 0x0006, 9         , 2          ) /* XK-8 Stick keyboard*/
-- DEVICE_MAP_ENTRY(PI_VID, 0x046A, 2        , 0x0001    , 0x0002, 6         , 0          ) /* XK-8 Stick mouse*/


doSomethingWithDevice :: Device -> IO ()
doSomethingWithDevice dev = do
  putStrLn $ unlines $ deviceInfo dev

  putStrLn $ replicate 80 '-'
  putStrLn "Opening device..."
  withDeviceHandle dev $ \devHndl -> do

    putStrLn $ replicate 80 '-'
    putStrLn "Detaching kernel driver..."
    withDetachedKernelDriver devHndl 0 $ do

      putStrLn $ replicate 80 '-'
      putStrLn "Claiming interface..."
      withClaimedInterface devHndl 0 $ do

        -- Inspecting descriptors:
        config0 <- getConfigDesc dev 0
        let interface0 = configInterfaces config0 ! 0
            alternate0 = interface0 ! 0
            endpoint1  = interfaceEndpoints alternate0 ! 0
            endpoint2  = interfaceEndpoints alternate0 ! 1
            mps        = maxPacketSize $ endpointMaxPacketSize endpoint1
            mps2       = maxPacketSize $ endpointMaxPacketSize endpoint2

        printf "RX maxPacketSize = %i\n" mps
        printf "TX maxPacketSize = %i\n" mps2

        putStrLn $ replicate 80 '-'
        putStrLn "Creating transfers..."
        readTrans <- newReadTransfer
                       InterruptTransfer
                       devHndl
                       (endpointAddress endpoint1)
                       mps
                       noTimeout
        writeTrans <- newWriteTransfer
                        InterruptTransfer
                        devHndl
                        (endpointAddress endpoint2)
                        B.empty
                        noTimeout

        setWriteTransferInput writeTrans (B.pack . take mps2 $ [181, 4, 2] ++ repeat 0x00)
        (szWrite, statusWrite) <- performWriteTransfer writeTrans
        -- ((bs, status), (szWrite, statusWrite)) <- concurrently (do rx <- performReadTransfer readTrans
        --                                                            putStrLn "Initial RX completed."
        --                                                            pure rx
        --                                                        )
        --                                                        (do threadDelay 100000
        --                                                            tx <- performWriteTransfer writeTrans
        --                                                            -- threadDelay 100000
        --                                                            -- setWriteTransferInput writeTrans (B.pack . take mps2 $ [0x0, 214] ++ repeat 0x00)
        --                                                            -- performWriteTransfer writeTrans
        --                                                            -- threadDelay 100000
        --                                                            -- setWriteTransferInput writeTrans (B.pack . take mps2 $ [0x0, 193] ++ repeat 0x00)
        --                                                            -- performWriteTransfer writeTrans
        --                                                            putStrLn "Initial TX completed."
        --                                                            pure tx
        --                                                        )
        printf "Write %i bytes with status: %s\n" szWrite (show statusWrite)
        -- printf "Read %i bytes with status: %s\n" (B.length bs) (show status)
        -- printBytes bs

        -- Performing I/O:
        forever $ do
          (bs, status) <- performReadTransfer readTrans
          printBytes bs
          printf "Read %i bytes with status: %s\n" (B.length bs) (show status)
          let keys = filter (/= 0)                             -- :: [] Int
                   . join                                      -- :: [] Int
                   . map (\(z, (a,b)) -> [z * a, (z + 4) * b]) -- :: L² Int
                   . zip [1..]                                 -- :: [] (Int, (Int, Int))
                   . map mat                                   -- :: [] (Int, Int)
                   . B.unpack                                  -- :: [] Word8
                   . B.take 4                                  -- :: B.ByteString
                   . B.drop 2                                  -- :: B.ByteString
                   $ bs                                        -- :: B.ByteString
          putStrLn $ show $ sort keys

-- ---------------------------------------------------------------------------
--
-- https://www.kernel.org/doc/html/v4.12/input/uinput.html
-- https://www.freedesktop.org/software/libevdev/doc/latest/
--
-- ---------------------------------------------------------------------------

          -- setWriteTransferInput writeTrans (B.pack $ [leftLEDs, green, modeFlash] ++ replicate 32 0x00)
          -- (szWrite,statusWrite) <- performWriteTransfer writeTrans
          -- printf "Write %i bytes with status: %s\n" szWrite (show statusWrite)
          setWriteTransferInput writeTrans (B.pack $ [flashFreq, 50] ++ replicate 33 0x00) -- second number is in milliseconds (both flash same rate).
          (szWrite,statusWrite) <- performWriteTransfer writeTrans

          printf "Write %i bytes with status: %s\n" szWrite (show statusWrite)
          -- setWriteTransferInput writeTrans (B.pack $ [toggleBacklight, 0] ++ replicate 32 0x00)
          -- (szWrite,statusWrite) <- performWriteTransfer writeTrans
          -- printf "Write %i bytes with status: %s\n" szWrite (show statusWrite)
  where
    red, green, modeOff, modeOn, modeFlash, leftLEDs, flashFreq :: Word8 -- Red and Gree Indicator LEDs at far left of device.
    red = 0x07
    green = 0x06
    modeOff = 0x00
    modeOn = 0x01
    modeFlash = 0x02
    leftLEDs = 0xb3 -- 179
    flashFreq = 0xb4 -- 180
    ledClicked = 0xb5 -- 181
    timeStamp = 210
    setIntensity = 187
    setBacklight = 182
    toggleBacklight = 184
    scrollLockBackLight = 183

    mat :: Word8 -> (Int,Int)
    mat 0 = (0 , 0)
    mat 1 = (1 , 0)
    mat 2 = (0 , 1)
    mat 3 = (1 , 1)
    mat _ = (0 , 0)
deviceInfo :: Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ deviceSpeed dev)
  , printf "busNumber:     %s" (show $ busNumber dev)
  , printf "portNumber:    %s" (show $ portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . V.toList) $ portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ deviceAddress dev)
  ]

printBytes :: B.ByteString -> IO ()
printBytes = putStrLn . intercalate " " . map (printf "0x%02x") . B.unpack

catchUSBException :: IO α -> (USBException -> IO α) -> IO α
catchUSBException = catch


{-
DeviceDesc { deviceUSBSpecReleaseNumber = (0,1,1,0)
           , deviceClass = 0
           , deviceSubClass = 0
           , deviceProtocol = 0
           , deviceMaxPacketSize0 = 8
           , deviceVendorId = 1523   -- 05f3 PI Engineering, Inc.
           , deviceProductId = 1130  -- 046a XK-8 Stick
           , deviceReleaseNumber = (0,3,0,0)
           , deviceManufacturerStrIx = Just 1
           , deviceProductStrIx = Just 2
           , deviceSerialNumberStrIx = Nothing
           , deviceNumConfigs = 1
           }

ConfigDesc { configValue = 1
           , configStrIx = Nothing
           , configAttribs = DeviceStatus { remoteWakeup = False
                                          , selfPowered = False
                                          }
           , configMaxPower = 150
           , configInterfaces = [ [InterfaceDesc { interfaceNumber = 0
                                                 , interfaceAltSetting = 0
                                                 , interfaceClass = 3
                                                 , interfaceSubClass = 0
                                                 , interfaceProtocol = 0
                                                 , interfaceStrIx = Just 2
                                                 , interfaceEndpoints =
                                                   [ EndpointDesc {endpointAddress = EndpointAddress { endpointNumber = 3
                                                                                                     , transferDirection = In
                                                                                                     }
                                                                  , endpointAttribs = Interrupt
                                                                  , endpointMaxPacketSize = MaxPacketSize { maxPacketSize = 32
                                                                                                          , transactionOpportunities = Zero
                                                                                                          }
                                                                  , endpointInterval = 10
                                                                  , endpointRefresh = 0
                                                                  , endpointSynchAddress = 0
                                                                  , endpointExtra = ""}
                                                   , EndpointDesc { endpointAddress = EndpointAddress { endpointNumber = 4
                                                                                                      , transferDirection = Out
                                                                                                      }
                                                                  , endpointAttribs = Interrupt
                                                                  , endpointMaxPacketSize = MaxPacketSize { maxPacketSize = 35
                                                                                                          , transactionOpportunities = Zero
                                                                                                          }
                                                                  , endpointInterval = 1
                                                                  , endpointRefresh = 0
                                                                  , endpointSynchAddress = 0
                                                                  , endpointExtra = ""
                                                                  }
                                                   ]
                                                 , interfaceExtra = "\t!\DC1\SOH\NUL\SOH\"!\NUL"
                                                 }
                                  ]
                                , [ InterfaceDesc { interfaceNumber = 1
                                                  , interfaceAltSetting = 0
                                                  , interfaceClass = 3
                                                  , interfaceSubClass = 1
                                                  , interfaceProtocol = 1
                                                  , interfaceStrIx = Just 2
                                                  , interfaceEndpoints =
                                                    [ EndpointDesc { endpointAddress = EndpointAddress { endpointNumber = 1
                                                                                                       , transferDirection = In
                                                                                                       }
                                                                   , endpointAttribs = Interrupt
                                                                   , endpointMaxPacketSize = MaxPacketSize { maxPacketSize = 8
                                                                                                           , transactionOpportunities = Zero
                                                                                                           }
                                                                   , endpointInterval = 1
                                                                   , endpointRefresh = 0
                                                                   , endpointSynchAddress = 0
                                                                   , endpointExtra = ""
                                                                   }
                                                    ]
                                                  , interfaceExtra = "\t!\DC1\SOH\NUL\SOH\"?\NUL"
                                                  }
                                  ]
                                , [ InterfaceDesc { interfaceNumber = 2
                                                  , interfaceAltSetting = 0
                                                  , interfaceClass = 3
                                                  , interfaceSubClass = 0
                                                  , interfaceProtocol = 0
                                                  , interfaceStrIx = Just 2
                                                  , interfaceEndpoints =
                                                    [ EndpointDesc { endpointAddress = EndpointAddress { endpointNumber = 2
                                                                                                       , transferDirection = In
                                                                                                       }
                                                                   , endpointAttribs = Interrupt
                                                                   , endpointMaxPacketSize = MaxPacketSize { maxPacketSize = 5
                                                                                                           , transactionOpportunities = Zero
                                                                                                           }
                                                                   , endpointInterval = 1
                                                                   , endpointRefresh = 0
                                                                   , endpointSynchAddress = 0
                                                                   , endpointExtra = ""
                                                                   }
                                                    ]
                                                  , interfaceExtra = "\t!\DC1\SOH\NUL\SOH\"8\NUL"
                                                  }
                                  ]
                                ]
           , configExtra = ""
           }
-}



















-- -- Local Imports

-- -- Explicit Imports

-- -- Qualified Imports

-- import qualified Data.Text as T

-- -- Undisciplined Imports

-- import Prelude
-- import System.USB

-- -- End of Imports
-- -- --------------------------------------------------------------------------------------------------------------------------------------------


