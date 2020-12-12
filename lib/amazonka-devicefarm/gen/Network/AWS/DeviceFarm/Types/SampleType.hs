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
        CPU,
        Memory,
        NativeAvgDrawtime,
        NativeFps,
        NativeFrames,
        NativeMaxDrawtime,
        NativeMinDrawtime,
        OpenglAvgDrawtime,
        OpenglFps,
        OpenglFrames,
        OpenglMaxDrawtime,
        OpenglMinDrawtime,
        RX,
        RxRate,
        TX,
        Threads,
        TxRate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SampleType = SampleType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CPU :: SampleType
pattern CPU = SampleType' "CPU"

pattern Memory :: SampleType
pattern Memory = SampleType' "MEMORY"

pattern NativeAvgDrawtime :: SampleType
pattern NativeAvgDrawtime = SampleType' "NATIVE_AVG_DRAWTIME"

pattern NativeFps :: SampleType
pattern NativeFps = SampleType' "NATIVE_FPS"

pattern NativeFrames :: SampleType
pattern NativeFrames = SampleType' "NATIVE_FRAMES"

pattern NativeMaxDrawtime :: SampleType
pattern NativeMaxDrawtime = SampleType' "NATIVE_MAX_DRAWTIME"

pattern NativeMinDrawtime :: SampleType
pattern NativeMinDrawtime = SampleType' "NATIVE_MIN_DRAWTIME"

pattern OpenglAvgDrawtime :: SampleType
pattern OpenglAvgDrawtime = SampleType' "OPENGL_AVG_DRAWTIME"

pattern OpenglFps :: SampleType
pattern OpenglFps = SampleType' "OPENGL_FPS"

pattern OpenglFrames :: SampleType
pattern OpenglFrames = SampleType' "OPENGL_FRAMES"

pattern OpenglMaxDrawtime :: SampleType
pattern OpenglMaxDrawtime = SampleType' "OPENGL_MAX_DRAWTIME"

pattern OpenglMinDrawtime :: SampleType
pattern OpenglMinDrawtime = SampleType' "OPENGL_MIN_DRAWTIME"

pattern RX :: SampleType
pattern RX = SampleType' "RX"

pattern RxRate :: SampleType
pattern RxRate = SampleType' "RX_RATE"

pattern TX :: SampleType
pattern TX = SampleType' "TX"

pattern Threads :: SampleType
pattern Threads = SampleType' "THREADS"

pattern TxRate :: SampleType
pattern TxRate = SampleType' "TX_RATE"

{-# COMPLETE
  CPU,
  Memory,
  NativeAvgDrawtime,
  NativeFps,
  NativeFrames,
  NativeMaxDrawtime,
  NativeMinDrawtime,
  OpenglAvgDrawtime,
  OpenglFps,
  OpenglFrames,
  OpenglMaxDrawtime,
  OpenglMinDrawtime,
  RX,
  RxRate,
  TX,
  Threads,
  TxRate,
  SampleType'
  #-}
