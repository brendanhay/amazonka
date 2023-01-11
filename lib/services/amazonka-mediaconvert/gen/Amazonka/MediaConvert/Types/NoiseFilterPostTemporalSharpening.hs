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
-- Module      : Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpening
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpening
  ( NoiseFilterPostTemporalSharpening
      ( ..,
        NoiseFilterPostTemporalSharpening_AUTO,
        NoiseFilterPostTemporalSharpening_DISABLED,
        NoiseFilterPostTemporalSharpening_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), the
-- bandwidth and sharpness of your output is reduced. You can optionally
-- use Post temporal sharpening (postTemporalSharpening) to apply
-- sharpening to the edges of your output. Note that Post temporal
-- sharpening will also make the bandwidth reduction from the Noise reducer
-- smaller. The default behavior, Auto (AUTO), allows the transcoder to
-- determine whether to apply sharpening, depending on your input type and
-- quality. When you set Post temporal sharpening to Enabled (ENABLED),
-- specify how much sharpening is applied using Post temporal sharpening
-- strength (postTemporalSharpeningStrength). Set Post temporal sharpening
-- to Disabled (DISABLED) to not apply sharpening.
newtype NoiseFilterPostTemporalSharpening = NoiseFilterPostTemporalSharpening'
  { fromNoiseFilterPostTemporalSharpening ::
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

pattern NoiseFilterPostTemporalSharpening_AUTO :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpening_AUTO = NoiseFilterPostTemporalSharpening' "AUTO"

pattern NoiseFilterPostTemporalSharpening_DISABLED :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpening_DISABLED = NoiseFilterPostTemporalSharpening' "DISABLED"

pattern NoiseFilterPostTemporalSharpening_ENABLED :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpening_ENABLED = NoiseFilterPostTemporalSharpening' "ENABLED"

{-# COMPLETE
  NoiseFilterPostTemporalSharpening_AUTO,
  NoiseFilterPostTemporalSharpening_DISABLED,
  NoiseFilterPostTemporalSharpening_ENABLED,
  NoiseFilterPostTemporalSharpening'
  #-}
