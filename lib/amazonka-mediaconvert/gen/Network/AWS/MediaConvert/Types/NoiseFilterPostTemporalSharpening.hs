{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
  ( NoiseFilterPostTemporalSharpening
    ( NoiseFilterPostTemporalSharpening'
    , NoiseFilterPostTemporalSharpeningDisabled
    , NoiseFilterPostTemporalSharpeningEnabled
    , NoiseFilterPostTemporalSharpeningAuto
    , fromNoiseFilterPostTemporalSharpening
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
newtype NoiseFilterPostTemporalSharpening = NoiseFilterPostTemporalSharpening'{fromNoiseFilterPostTemporalSharpening
                                                                               :: Core.Text}
                                              deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                              Core.Show, Core.Generic)
                                              deriving newtype (Core.IsString, Core.Hashable,
                                                                Core.NFData, Core.ToJSONKey,
                                                                Core.FromJSONKey, Core.ToJSON,
                                                                Core.FromJSON, Core.ToXML,
                                                                Core.FromXML, Core.ToText,
                                                                Core.FromText, Core.ToByteString,
                                                                Core.ToQuery, Core.ToHeader)

pattern NoiseFilterPostTemporalSharpeningDisabled :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpeningDisabled = NoiseFilterPostTemporalSharpening' "DISABLED"

pattern NoiseFilterPostTemporalSharpeningEnabled :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpeningEnabled = NoiseFilterPostTemporalSharpening' "ENABLED"

pattern NoiseFilterPostTemporalSharpeningAuto :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpeningAuto = NoiseFilterPostTemporalSharpening' "AUTO"

{-# COMPLETE 
  NoiseFilterPostTemporalSharpeningDisabled,

  NoiseFilterPostTemporalSharpeningEnabled,

  NoiseFilterPostTemporalSharpeningAuto,
  NoiseFilterPostTemporalSharpening'
  #-}
