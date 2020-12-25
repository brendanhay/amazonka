{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
  ( Mpeg2QualityTuningLevel
      ( Mpeg2QualityTuningLevel',
        Mpeg2QualityTuningLevelSinglePass,
        Mpeg2QualityTuningLevelMultiPass,
        fromMpeg2QualityTuningLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
newtype Mpeg2QualityTuningLevel = Mpeg2QualityTuningLevel'
  { fromMpeg2QualityTuningLevel ::
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

pattern Mpeg2QualityTuningLevelSinglePass :: Mpeg2QualityTuningLevel
pattern Mpeg2QualityTuningLevelSinglePass = Mpeg2QualityTuningLevel' "SINGLE_PASS"

pattern Mpeg2QualityTuningLevelMultiPass :: Mpeg2QualityTuningLevel
pattern Mpeg2QualityTuningLevelMultiPass = Mpeg2QualityTuningLevel' "MULTI_PASS"

{-# COMPLETE
  Mpeg2QualityTuningLevelSinglePass,
  Mpeg2QualityTuningLevelMultiPass,
  Mpeg2QualityTuningLevel'
  #-}
