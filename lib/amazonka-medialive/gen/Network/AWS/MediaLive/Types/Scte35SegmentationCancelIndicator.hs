{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
  ( Scte35SegmentationCancelIndicator
    ( Scte35SegmentationCancelIndicator'
    , Scte35SegmentationCancelIndicatorSegmentationEventNotCanceled
    , Scte35SegmentationCancelIndicatorSegmentationEventCanceled
    , fromScte35SegmentationCancelIndicator
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator. SEGMENTATION_EVENT_NOT_CANCELED corresponds to 0 in the SCTE-35 specification and indicates that this is an insertion request. SEGMENTATION_EVENT_CANCELED corresponds to 1 in the SCTE-35 specification and indicates that this is a cancelation request, in which case complete this field and the existing event ID to cancel.
newtype Scte35SegmentationCancelIndicator = Scte35SegmentationCancelIndicator'{fromScte35SegmentationCancelIndicator
                                                                               :: Core.Text}
                                              deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                              Core.Show, Core.Generic)
                                              deriving newtype (Core.IsString, Core.Hashable,
                                                                Core.NFData, Core.ToJSONKey,
                                                                Core.FromJSONKey, Core.ToJSON,
                                                                Core.FromJSON, Core.ToXML,
                                                                Core.FromXML, Core.ToText,
                                                                Core.FromText, Core.ToByteString,
                                                                Core.ToQuery, Core.ToHeader)

pattern Scte35SegmentationCancelIndicatorSegmentationEventNotCanceled :: Scte35SegmentationCancelIndicator
pattern Scte35SegmentationCancelIndicatorSegmentationEventNotCanceled = Scte35SegmentationCancelIndicator' "SEGMENTATION_EVENT_NOT_CANCELED"

pattern Scte35SegmentationCancelIndicatorSegmentationEventCanceled :: Scte35SegmentationCancelIndicator
pattern Scte35SegmentationCancelIndicatorSegmentationEventCanceled = Scte35SegmentationCancelIndicator' "SEGMENTATION_EVENT_CANCELED"

{-# COMPLETE 
  Scte35SegmentationCancelIndicatorSegmentationEventNotCanceled,

  Scte35SegmentationCancelIndicatorSegmentationEventCanceled,
  Scte35SegmentationCancelIndicator'
  #-}
