{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265QualityTuningLevel
  ( H265QualityTuningLevel
    ( H265QualityTuningLevel'
    , H265QualityTuningLevelSinglePass
    , H265QualityTuningLevelSinglePassHq
    , H265QualityTuningLevelMultiPassHq
    , fromH265QualityTuningLevel
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
newtype H265QualityTuningLevel = H265QualityTuningLevel'{fromH265QualityTuningLevel
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern H265QualityTuningLevelSinglePass :: H265QualityTuningLevel
pattern H265QualityTuningLevelSinglePass = H265QualityTuningLevel' "SINGLE_PASS"

pattern H265QualityTuningLevelSinglePassHq :: H265QualityTuningLevel
pattern H265QualityTuningLevelSinglePassHq = H265QualityTuningLevel' "SINGLE_PASS_HQ"

pattern H265QualityTuningLevelMultiPassHq :: H265QualityTuningLevel
pattern H265QualityTuningLevelMultiPassHq = H265QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE 
  H265QualityTuningLevelSinglePass,

  H265QualityTuningLevelSinglePassHq,

  H265QualityTuningLevelMultiPassHq,
  H265QualityTuningLevel'
  #-}
