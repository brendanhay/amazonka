{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264QualityTuningLevel
  ( H264QualityTuningLevel
      ( H264QualityTuningLevel',
        H264QualityTuningLevelSinglePass,
        H264QualityTuningLevelSinglePassHq,
        H264QualityTuningLevelMultiPassHq,
        fromH264QualityTuningLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
newtype H264QualityTuningLevel = H264QualityTuningLevel'
  { fromH264QualityTuningLevel ::
      Core.Text
  }
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

pattern H264QualityTuningLevelSinglePass :: H264QualityTuningLevel
pattern H264QualityTuningLevelSinglePass = H264QualityTuningLevel' "SINGLE_PASS"

pattern H264QualityTuningLevelSinglePassHq :: H264QualityTuningLevel
pattern H264QualityTuningLevelSinglePassHq = H264QualityTuningLevel' "SINGLE_PASS_HQ"

pattern H264QualityTuningLevelMultiPassHq :: H264QualityTuningLevel
pattern H264QualityTuningLevelMultiPassHq = H264QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  H264QualityTuningLevelSinglePass,
  H264QualityTuningLevelSinglePassHq,
  H264QualityTuningLevelMultiPassHq,
  H264QualityTuningLevel'
  #-}
