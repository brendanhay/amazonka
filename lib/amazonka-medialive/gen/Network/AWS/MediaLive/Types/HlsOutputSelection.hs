-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsOutputSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsOutputSelection
  ( HlsOutputSelection
      ( HlsOutputSelection',
        ManifestsAndSegments,
        SegmentsOnly,
        VariantManifestsAndSegments
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Hls Output Selection
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

pattern VariantManifestsAndSegments :: HlsOutputSelection
pattern VariantManifestsAndSegments = HlsOutputSelection' "VARIANT_MANIFESTS_AND_SEGMENTS"

{-# COMPLETE
  ManifestsAndSegments,
  SegmentsOnly,
  VariantManifestsAndSegments,
  HlsOutputSelection'
  #-}
