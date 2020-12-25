{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.SampleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.SampleType
  ( SampleType
      ( SampleType',
        SampleTypeCpu,
        SampleTypeMemory,
        SampleTypeThreads,
        SampleTypeRxRate,
        SampleTypeTxRate,
        SampleTypeRX,
        SampleTypeTX,
        SampleTypeNativeFrames,
        SampleTypeNativeFps,
        SampleTypeNativeMinDrawtime,
        SampleTypeNativeAvgDrawtime,
        SampleTypeNativeMaxDrawtime,
        SampleTypeOpenglFrames,
        SampleTypeOpenglFps,
        SampleTypeOpenglMinDrawtime,
        SampleTypeOpenglAvgDrawtime,
        SampleTypeOpenglMaxDrawtime,
        fromSampleType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SampleType = SampleType' {fromSampleType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SampleTypeCpu :: SampleType
pattern SampleTypeCpu = SampleType' "CPU"

pattern SampleTypeMemory :: SampleType
pattern SampleTypeMemory = SampleType' "MEMORY"

pattern SampleTypeThreads :: SampleType
pattern SampleTypeThreads = SampleType' "THREADS"

pattern SampleTypeRxRate :: SampleType
pattern SampleTypeRxRate = SampleType' "RX_RATE"

pattern SampleTypeTxRate :: SampleType
pattern SampleTypeTxRate = SampleType' "TX_RATE"

pattern SampleTypeRX :: SampleType
pattern SampleTypeRX = SampleType' "RX"

pattern SampleTypeTX :: SampleType
pattern SampleTypeTX = SampleType' "TX"

pattern SampleTypeNativeFrames :: SampleType
pattern SampleTypeNativeFrames = SampleType' "NATIVE_FRAMES"

pattern SampleTypeNativeFps :: SampleType
pattern SampleTypeNativeFps = SampleType' "NATIVE_FPS"

pattern SampleTypeNativeMinDrawtime :: SampleType
pattern SampleTypeNativeMinDrawtime = SampleType' "NATIVE_MIN_DRAWTIME"

pattern SampleTypeNativeAvgDrawtime :: SampleType
pattern SampleTypeNativeAvgDrawtime = SampleType' "NATIVE_AVG_DRAWTIME"

pattern SampleTypeNativeMaxDrawtime :: SampleType
pattern SampleTypeNativeMaxDrawtime = SampleType' "NATIVE_MAX_DRAWTIME"

pattern SampleTypeOpenglFrames :: SampleType
pattern SampleTypeOpenglFrames = SampleType' "OPENGL_FRAMES"

pattern SampleTypeOpenglFps :: SampleType
pattern SampleTypeOpenglFps = SampleType' "OPENGL_FPS"

pattern SampleTypeOpenglMinDrawtime :: SampleType
pattern SampleTypeOpenglMinDrawtime = SampleType' "OPENGL_MIN_DRAWTIME"

pattern SampleTypeOpenglAvgDrawtime :: SampleType
pattern SampleTypeOpenglAvgDrawtime = SampleType' "OPENGL_AVG_DRAWTIME"

pattern SampleTypeOpenglMaxDrawtime :: SampleType
pattern SampleTypeOpenglMaxDrawtime = SampleType' "OPENGL_MAX_DRAWTIME"

{-# COMPLETE
  SampleTypeCpu,
  SampleTypeMemory,
  SampleTypeThreads,
  SampleTypeRxRate,
  SampleTypeTxRate,
  SampleTypeRX,
  SampleTypeTX,
  SampleTypeNativeFrames,
  SampleTypeNativeFps,
  SampleTypeNativeMinDrawtime,
  SampleTypeNativeAvgDrawtime,
  SampleTypeNativeMaxDrawtime,
  SampleTypeOpenglFrames,
  SampleTypeOpenglFps,
  SampleTypeOpenglMinDrawtime,
  SampleTypeOpenglAvgDrawtime,
  SampleTypeOpenglMaxDrawtime,
  SampleType'
  #-}
