-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
  ( M2tsSegmentationMarkers
      ( M2tsSegmentationMarkers',
        MSMEbp,
        MSMEbpLegacy,
        MSMNone,
        MSMPsiSegstart,
        MSMRaiAdapt,
        MSMRaiSegstart
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Segmentation Markers
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

pattern MSMEbp :: M2tsSegmentationMarkers
pattern MSMEbp = M2tsSegmentationMarkers' "EBP"

pattern MSMEbpLegacy :: M2tsSegmentationMarkers
pattern MSMEbpLegacy = M2tsSegmentationMarkers' "EBP_LEGACY"

pattern MSMNone :: M2tsSegmentationMarkers
pattern MSMNone = M2tsSegmentationMarkers' "NONE"

pattern MSMPsiSegstart :: M2tsSegmentationMarkers
pattern MSMPsiSegstart = M2tsSegmentationMarkers' "PSI_SEGSTART"

pattern MSMRaiAdapt :: M2tsSegmentationMarkers
pattern MSMRaiAdapt = M2tsSegmentationMarkers' "RAI_ADAPT"

pattern MSMRaiSegstart :: M2tsSegmentationMarkers
pattern MSMRaiSegstart = M2tsSegmentationMarkers' "RAI_SEGSTART"

{-# COMPLETE
  MSMEbp,
  MSMEbpLegacy,
  MSMNone,
  MSMPsiSegstart,
  MSMRaiAdapt,
  MSMRaiSegstart,
  M2tsSegmentationMarkers'
  #-}
