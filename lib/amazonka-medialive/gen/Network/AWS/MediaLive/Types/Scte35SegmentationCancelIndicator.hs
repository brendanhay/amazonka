-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
  ( Scte35SegmentationCancelIndicator
      ( Scte35SegmentationCancelIndicator',
        SegmentationEventCanceled,
        SegmentationEventNotCanceled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator. SEGMENTATION_EVENT_NOT_CANCELED corresponds to 0 in the SCTE-35 specification and indicates that this is an insertion request. SEGMENTATION_EVENT_CANCELED corresponds to 1 in the SCTE-35 specification and indicates that this is a cancelation request, in which case complete this field and the existing event ID to cancel.
newtype Scte35SegmentationCancelIndicator = Scte35SegmentationCancelIndicator' Lude.Text
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

pattern SegmentationEventCanceled :: Scte35SegmentationCancelIndicator
pattern SegmentationEventCanceled = Scte35SegmentationCancelIndicator' "SEGMENTATION_EVENT_CANCELED"

pattern SegmentationEventNotCanceled :: Scte35SegmentationCancelIndicator
pattern SegmentationEventNotCanceled = Scte35SegmentationCancelIndicator' "SEGMENTATION_EVENT_NOT_CANCELED"

{-# COMPLETE
  SegmentationEventCanceled,
  SegmentationEventNotCanceled,
  Scte35SegmentationCancelIndicator'
  #-}
