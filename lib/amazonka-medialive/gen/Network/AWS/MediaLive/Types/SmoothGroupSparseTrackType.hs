{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
  ( SmoothGroupSparseTrackType
      ( SmoothGroupSparseTrackType',
        SGSTTNone,
        SGSTTScte35,
        SGSTTScte35WithoutSegmentation
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Smooth Group Sparse Track Type
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType' Lude.Text
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

pattern SGSTTNone :: SmoothGroupSparseTrackType
pattern SGSTTNone = SmoothGroupSparseTrackType' "NONE"

pattern SGSTTScte35 :: SmoothGroupSparseTrackType
pattern SGSTTScte35 = SmoothGroupSparseTrackType' "SCTE_35"

pattern SGSTTScte35WithoutSegmentation :: SmoothGroupSparseTrackType
pattern SGSTTScte35WithoutSegmentation = SmoothGroupSparseTrackType' "SCTE_35_WITHOUT_SEGMENTATION"

{-# COMPLETE
  SGSTTNone,
  SGSTTScte35,
  SGSTTScte35WithoutSegmentation,
  SmoothGroupSparseTrackType'
  #-}
