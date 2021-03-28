{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
  ( CmafWriteSegmentTimelineInRepresentation
    ( CmafWriteSegmentTimelineInRepresentation'
    , CmafWriteSegmentTimelineInRepresentationEnabled
    , CmafWriteSegmentTimelineInRepresentationDisabled
    , fromCmafWriteSegmentTimelineInRepresentation
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
newtype CmafWriteSegmentTimelineInRepresentation = CmafWriteSegmentTimelineInRepresentation'{fromCmafWriteSegmentTimelineInRepresentation
                                                                                             ::
                                                                                             Core.Text}
                                                     deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                     Core.Show, Core.Generic)
                                                     deriving newtype (Core.IsString, Core.Hashable,
                                                                       Core.NFData, Core.ToJSONKey,
                                                                       Core.FromJSONKey,
                                                                       Core.ToJSON, Core.FromJSON,
                                                                       Core.ToXML, Core.FromXML,
                                                                       Core.ToText, Core.FromText,
                                                                       Core.ToByteString,
                                                                       Core.ToQuery, Core.ToHeader)

pattern CmafWriteSegmentTimelineInRepresentationEnabled :: CmafWriteSegmentTimelineInRepresentation
pattern CmafWriteSegmentTimelineInRepresentationEnabled = CmafWriteSegmentTimelineInRepresentation' "ENABLED"

pattern CmafWriteSegmentTimelineInRepresentationDisabled :: CmafWriteSegmentTimelineInRepresentation
pattern CmafWriteSegmentTimelineInRepresentationDisabled = CmafWriteSegmentTimelineInRepresentation' "DISABLED"

{-# COMPLETE 
  CmafWriteSegmentTimelineInRepresentationEnabled,

  CmafWriteSegmentTimelineInRepresentationDisabled,
  CmafWriteSegmentTimelineInRepresentation'
  #-}
