{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
  ( SmoothGroupSparseTrackType
    ( SmoothGroupSparseTrackType'
    , SmoothGroupSparseTrackTypeNone
    , SmoothGroupSparseTrackTypeScte35
    , SmoothGroupSparseTrackTypeScte35WithoutSegmentation
    , fromSmoothGroupSparseTrackType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Smooth Group Sparse Track Type
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType'{fromSmoothGroupSparseTrackType
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern SmoothGroupSparseTrackTypeNone :: SmoothGroupSparseTrackType
pattern SmoothGroupSparseTrackTypeNone = SmoothGroupSparseTrackType' "NONE"

pattern SmoothGroupSparseTrackTypeScte35 :: SmoothGroupSparseTrackType
pattern SmoothGroupSparseTrackTypeScte35 = SmoothGroupSparseTrackType' "SCTE_35"

pattern SmoothGroupSparseTrackTypeScte35WithoutSegmentation :: SmoothGroupSparseTrackType
pattern SmoothGroupSparseTrackTypeScte35WithoutSegmentation = SmoothGroupSparseTrackType' "SCTE_35_WITHOUT_SEGMENTATION"

{-# COMPLETE 
  SmoothGroupSparseTrackTypeNone,

  SmoothGroupSparseTrackTypeScte35,

  SmoothGroupSparseTrackTypeScte35WithoutSegmentation,
  SmoothGroupSparseTrackType'
  #-}
