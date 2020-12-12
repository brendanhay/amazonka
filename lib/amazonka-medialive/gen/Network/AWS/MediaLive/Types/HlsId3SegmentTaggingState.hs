{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
  ( HlsId3SegmentTaggingState
      ( HlsId3SegmentTaggingState',
        HISTSDisabled,
        HISTSEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | State of HLS ID3 Segment Tagging
newtype HlsId3SegmentTaggingState = HlsId3SegmentTaggingState' Lude.Text
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

pattern HISTSDisabled :: HlsId3SegmentTaggingState
pattern HISTSDisabled = HlsId3SegmentTaggingState' "DISABLED"

pattern HISTSEnabled :: HlsId3SegmentTaggingState
pattern HISTSEnabled = HlsId3SegmentTaggingState' "ENABLED"

{-# COMPLETE
  HISTSDisabled,
  HISTSEnabled,
  HlsId3SegmentTaggingState'
  #-}
