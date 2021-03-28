{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
  ( AudioNormalizationPeakCalculation
    ( AudioNormalizationPeakCalculation'
    , AudioNormalizationPeakCalculationTruePeak
    , AudioNormalizationPeakCalculationNone
    , fromAudioNormalizationPeakCalculation
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
newtype AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation'{fromAudioNormalizationPeakCalculation
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

pattern AudioNormalizationPeakCalculationTruePeak :: AudioNormalizationPeakCalculation
pattern AudioNormalizationPeakCalculationTruePeak = AudioNormalizationPeakCalculation' "TRUE_PEAK"

pattern AudioNormalizationPeakCalculationNone :: AudioNormalizationPeakCalculation
pattern AudioNormalizationPeakCalculationNone = AudioNormalizationPeakCalculation' "NONE"

{-# COMPLETE 
  AudioNormalizationPeakCalculationTruePeak,

  AudioNormalizationPeakCalculationNone,
  AudioNormalizationPeakCalculation'
  #-}
