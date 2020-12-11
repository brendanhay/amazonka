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
        EnhancedQuality,
        StandardQuality
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Quality Level
newtype H264QualityLevel = H264QualityLevel' Lude.Text
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

pattern EnhancedQuality :: H264QualityLevel
pattern EnhancedQuality = H264QualityLevel' "ENHANCED_QUALITY"

pattern StandardQuality :: H264QualityLevel
pattern StandardQuality = H264QualityLevel' "STANDARD_QUALITY"

{-# COMPLETE
  EnhancedQuality,
  StandardQuality,
  H264QualityLevel'
  #-}
