{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264QualityLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264QualityLevel
  ( H264QualityLevel
      ( H264QualityLevel',
        H264QualityLevelEnhancedQuality,
        H264QualityLevelStandardQuality,
        fromH264QualityLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Quality Level
newtype H264QualityLevel = H264QualityLevel'
  { fromH264QualityLevel ::
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

pattern H264QualityLevelEnhancedQuality :: H264QualityLevel
pattern H264QualityLevelEnhancedQuality = H264QualityLevel' "ENHANCED_QUALITY"

pattern H264QualityLevelStandardQuality :: H264QualityLevel
pattern H264QualityLevelStandardQuality = H264QualityLevel' "STANDARD_QUALITY"

{-# COMPLETE
  H264QualityLevelEnhancedQuality,
  H264QualityLevelStandardQuality,
  H264QualityLevel'
  #-}
