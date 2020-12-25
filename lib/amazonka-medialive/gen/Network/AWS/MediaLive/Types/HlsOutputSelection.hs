{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        HlsOutputSelectionManifestsAndSegments,
        HlsOutputSelectionSegmentsOnly,
        HlsOutputSelectionVariantManifestsAndSegments,
        fromHlsOutputSelection
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Hls Output Selection
newtype HlsOutputSelection = HlsOutputSelection'
  { fromHlsOutputSelection ::
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

pattern HlsOutputSelectionManifestsAndSegments :: HlsOutputSelection
pattern HlsOutputSelectionManifestsAndSegments = HlsOutputSelection' "MANIFESTS_AND_SEGMENTS"

pattern HlsOutputSelectionSegmentsOnly :: HlsOutputSelection
pattern HlsOutputSelectionSegmentsOnly = HlsOutputSelection' "SEGMENTS_ONLY"

pattern HlsOutputSelectionVariantManifestsAndSegments :: HlsOutputSelection
pattern HlsOutputSelectionVariantManifestsAndSegments = HlsOutputSelection' "VARIANT_MANIFESTS_AND_SEGMENTS"

{-# COMPLETE
  HlsOutputSelectionManifestsAndSegments,
  HlsOutputSelectionSegmentsOnly,
  HlsOutputSelectionVariantManifestsAndSegments,
  HlsOutputSelection'
  #-}
