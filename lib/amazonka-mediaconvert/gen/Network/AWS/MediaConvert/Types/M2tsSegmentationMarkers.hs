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
        MSMNone,
        MSMRaiSegstart,
        MSMRaiAdapt,
        MSMPsiSegstart,
        MSMEbp,
        MSMEbpLegacy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers' Lude.Text
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

pattern MSMNone :: M2tsSegmentationMarkers
pattern MSMNone = M2tsSegmentationMarkers' "NONE"

pattern MSMRaiSegstart :: M2tsSegmentationMarkers
pattern MSMRaiSegstart = M2tsSegmentationMarkers' "RAI_SEGSTART"

pattern MSMRaiAdapt :: M2tsSegmentationMarkers
pattern MSMRaiAdapt = M2tsSegmentationMarkers' "RAI_ADAPT"

pattern MSMPsiSegstart :: M2tsSegmentationMarkers
pattern MSMPsiSegstart = M2tsSegmentationMarkers' "PSI_SEGSTART"

pattern MSMEbp :: M2tsSegmentationMarkers
pattern MSMEbp = M2tsSegmentationMarkers' "EBP"

pattern MSMEbpLegacy :: M2tsSegmentationMarkers
pattern MSMEbpLegacy = M2tsSegmentationMarkers' "EBP_LEGACY"

{-# COMPLETE
  MSMNone,
  MSMRaiSegstart,
  MSMRaiAdapt,
  MSMPsiSegstart,
  MSMEbp,
  MSMEbpLegacy,
  M2tsSegmentationMarkers'
  #-}
