{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Types.SampleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.SampleType
  ( SampleType
      ( ..,
        SampleType_CPU,
        SampleType_MEMORY,
        SampleType_NATIVE_AVG_DRAWTIME,
        SampleType_NATIVE_FPS,
        SampleType_NATIVE_FRAMES,
        SampleType_NATIVE_MAX_DRAWTIME,
        SampleType_NATIVE_MIN_DRAWTIME,
        SampleType_OPENGL_AVG_DRAWTIME,
        SampleType_OPENGL_FPS,
        SampleType_OPENGL_FRAMES,
        SampleType_OPENGL_MAX_DRAWTIME,
        SampleType_OPENGL_MIN_DRAWTIME,
        SampleType_RX,
        SampleType_RX_RATE,
        SampleType_THREADS,
        SampleType_TX,
        SampleType_TX_RATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SampleType = SampleType'
  { fromSampleType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern SampleType_CPU :: SampleType
pattern SampleType_CPU = SampleType' "CPU"

pattern SampleType_MEMORY :: SampleType
pattern SampleType_MEMORY = SampleType' "MEMORY"

pattern SampleType_NATIVE_AVG_DRAWTIME :: SampleType
pattern SampleType_NATIVE_AVG_DRAWTIME = SampleType' "NATIVE_AVG_DRAWTIME"

pattern SampleType_NATIVE_FPS :: SampleType
pattern SampleType_NATIVE_FPS = SampleType' "NATIVE_FPS"

pattern SampleType_NATIVE_FRAMES :: SampleType
pattern SampleType_NATIVE_FRAMES = SampleType' "NATIVE_FRAMES"

pattern SampleType_NATIVE_MAX_DRAWTIME :: SampleType
pattern SampleType_NATIVE_MAX_DRAWTIME = SampleType' "NATIVE_MAX_DRAWTIME"

pattern SampleType_NATIVE_MIN_DRAWTIME :: SampleType
pattern SampleType_NATIVE_MIN_DRAWTIME = SampleType' "NATIVE_MIN_DRAWTIME"

pattern SampleType_OPENGL_AVG_DRAWTIME :: SampleType
pattern SampleType_OPENGL_AVG_DRAWTIME = SampleType' "OPENGL_AVG_DRAWTIME"

pattern SampleType_OPENGL_FPS :: SampleType
pattern SampleType_OPENGL_FPS = SampleType' "OPENGL_FPS"

pattern SampleType_OPENGL_FRAMES :: SampleType
pattern SampleType_OPENGL_FRAMES = SampleType' "OPENGL_FRAMES"

pattern SampleType_OPENGL_MAX_DRAWTIME :: SampleType
pattern SampleType_OPENGL_MAX_DRAWTIME = SampleType' "OPENGL_MAX_DRAWTIME"

pattern SampleType_OPENGL_MIN_DRAWTIME :: SampleType
pattern SampleType_OPENGL_MIN_DRAWTIME = SampleType' "OPENGL_MIN_DRAWTIME"

pattern SampleType_RX :: SampleType
pattern SampleType_RX = SampleType' "RX"

pattern SampleType_RX_RATE :: SampleType
pattern SampleType_RX_RATE = SampleType' "RX_RATE"

pattern SampleType_THREADS :: SampleType
pattern SampleType_THREADS = SampleType' "THREADS"

pattern SampleType_TX :: SampleType
pattern SampleType_TX = SampleType' "TX"

pattern SampleType_TX_RATE :: SampleType
pattern SampleType_TX_RATE = SampleType' "TX_RATE"

{-# COMPLETE
  SampleType_CPU,
  SampleType_MEMORY,
  SampleType_NATIVE_AVG_DRAWTIME,
  SampleType_NATIVE_FPS,
  SampleType_NATIVE_FRAMES,
  SampleType_NATIVE_MAX_DRAWTIME,
  SampleType_NATIVE_MIN_DRAWTIME,
  SampleType_OPENGL_AVG_DRAWTIME,
  SampleType_OPENGL_FPS,
  SampleType_OPENGL_FRAMES,
  SampleType_OPENGL_MAX_DRAWTIME,
  SampleType_OPENGL_MIN_DRAWTIME,
  SampleType_RX,
  SampleType_RX_RATE,
  SampleType_THREADS,
  SampleType_TX,
  SampleType_TX_RATE,
  SampleType'
  #-}
