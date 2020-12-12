{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsOutputSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsOutputSelection
  ( HlsOutputSelection
      ( HlsOutputSelection',
        ManifestsAndSegments,
        SegmentsOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
newtype HlsOutputSelection = HlsOutputSelection' Lude.Text
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

pattern ManifestsAndSegments :: HlsOutputSelection
pattern ManifestsAndSegments = HlsOutputSelection' "MANIFESTS_AND_SEGMENTS"

pattern SegmentsOnly :: HlsOutputSelection
pattern SegmentsOnly = HlsOutputSelection' "SEGMENTS_ONLY"

{-# COMPLETE
  ManifestsAndSegments,
  SegmentsOnly,
  HlsOutputSelection'
  #-}
