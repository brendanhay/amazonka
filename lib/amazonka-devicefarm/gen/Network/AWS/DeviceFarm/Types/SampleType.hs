{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.SampleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.SampleType where

import Network.AWS.Prelude

data SampleType
  = CPU
  | Memory
  | NativeAvgDrawtime
  | NativeFps
  | NativeFrames
  | NativeMaxDrawtime
  | NativeMinDrawtime
  | OpenglAvgDrawtime
  | OpenglFps
  | OpenglFrames
  | OpenglMaxDrawtime
  | OpenglMinDrawtime
  | RX
  | RxRate
  | TX
  | Threads
  | TxRate
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText SampleType where
  parser =
    takeLowerText >>= \case
      "cpu" -> pure CPU
      "memory" -> pure Memory
      "native_avg_drawtime" -> pure NativeAvgDrawtime
      "native_fps" -> pure NativeFps
      "native_frames" -> pure NativeFrames
      "native_max_drawtime" -> pure NativeMaxDrawtime
      "native_min_drawtime" -> pure NativeMinDrawtime
      "opengl_avg_drawtime" -> pure OpenglAvgDrawtime
      "opengl_fps" -> pure OpenglFps
      "opengl_frames" -> pure OpenglFrames
      "opengl_max_drawtime" -> pure OpenglMaxDrawtime
      "opengl_min_drawtime" -> pure OpenglMinDrawtime
      "rx" -> pure RX
      "rx_rate" -> pure RxRate
      "tx" -> pure TX
      "threads" -> pure Threads
      "tx_rate" -> pure TxRate
      e ->
        fromTextError $
          "Failure parsing SampleType from value: '" <> e
            <> "'. Accepted values: cpu, memory, native_avg_drawtime, native_fps, native_frames, native_max_drawtime, native_min_drawtime, opengl_avg_drawtime, opengl_fps, opengl_frames, opengl_max_drawtime, opengl_min_drawtime, rx, rx_rate, tx, threads, tx_rate"

instance ToText SampleType where
  toText = \case
    CPU -> "CPU"
    Memory -> "MEMORY"
    NativeAvgDrawtime -> "NATIVE_AVG_DRAWTIME"
    NativeFps -> "NATIVE_FPS"
    NativeFrames -> "NATIVE_FRAMES"
    NativeMaxDrawtime -> "NATIVE_MAX_DRAWTIME"
    NativeMinDrawtime -> "NATIVE_MIN_DRAWTIME"
    OpenglAvgDrawtime -> "OPENGL_AVG_DRAWTIME"
    OpenglFps -> "OPENGL_FPS"
    OpenglFrames -> "OPENGL_FRAMES"
    OpenglMaxDrawtime -> "OPENGL_MAX_DRAWTIME"
    OpenglMinDrawtime -> "OPENGL_MIN_DRAWTIME"
    RX -> "RX"
    RxRate -> "RX_RATE"
    TX -> "TX"
    Threads -> "THREADS"
    TxRate -> "TX_RATE"

instance Hashable SampleType

instance NFData SampleType

instance ToByteString SampleType

instance ToQuery SampleType

instance ToHeader SampleType

instance FromJSON SampleType where
  parseJSON = parseJSONText "SampleType"
