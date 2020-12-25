{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
  ( M2tsSegmentationMarkers
      ( M2tsSegmentationMarkers',
        M2tsSegmentationMarkersNone,
        M2tsSegmentationMarkersRaiSegstart,
        M2tsSegmentationMarkersRaiAdapt,
        M2tsSegmentationMarkersPsiSegstart,
        M2tsSegmentationMarkersEbp,
        M2tsSegmentationMarkersEbpLegacy,
        fromM2tsSegmentationMarkers
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers'
  { fromM2tsSegmentationMarkers ::
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

pattern M2tsSegmentationMarkersNone :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkersNone = M2tsSegmentationMarkers' "NONE"

pattern M2tsSegmentationMarkersRaiSegstart :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkersRaiSegstart = M2tsSegmentationMarkers' "RAI_SEGSTART"

pattern M2tsSegmentationMarkersRaiAdapt :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkersRaiAdapt = M2tsSegmentationMarkers' "RAI_ADAPT"

pattern M2tsSegmentationMarkersPsiSegstart :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkersPsiSegstart = M2tsSegmentationMarkers' "PSI_SEGSTART"

pattern M2tsSegmentationMarkersEbp :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkersEbp = M2tsSegmentationMarkers' "EBP"

pattern M2tsSegmentationMarkersEbpLegacy :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkersEbpLegacy = M2tsSegmentationMarkers' "EBP_LEGACY"

{-# COMPLETE
  M2tsSegmentationMarkersNone,
  M2tsSegmentationMarkersRaiSegstart,
  M2tsSegmentationMarkersRaiAdapt,
  M2tsSegmentationMarkersPsiSegstart,
  M2tsSegmentationMarkersEbp,
  M2tsSegmentationMarkersEbpLegacy,
  M2tsSegmentationMarkers'
  #-}
