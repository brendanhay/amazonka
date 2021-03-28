{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
  ( Scte35SegmentationDescriptor (..)
  -- * Smart constructor
  , mkScte35SegmentationDescriptor
  -- * Lenses
  , ssdSegmentationEventId
  , ssdSegmentationCancelIndicator
  , ssdDeliveryRestrictions
  , ssdSegmentNum
  , ssdSegmentationDuration
  , ssdSegmentationTypeId
  , ssdSegmentationUpid
  , ssdSegmentationUpidType
  , ssdSegmentsExpected
  , ssdSubSegmentNum
  , ssdSubSegmentsExpected
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions as Types
import qualified Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator as Types
import qualified Network.AWS.Prelude as Core

-- | Corresponds to SCTE-35 segmentation_descriptor.
--
-- /See:/ 'mkScte35SegmentationDescriptor' smart constructor.
data Scte35SegmentationDescriptor = Scte35SegmentationDescriptor'
  { segmentationEventId :: Core.Natural
    -- ^ Corresponds to SCTE-35 segmentation_event_id. 
  , segmentationCancelIndicator :: Types.Scte35SegmentationCancelIndicator
    -- ^ Corresponds to SCTE-35 segmentation_event_cancel_indicator.
  , deliveryRestrictions :: Core.Maybe Types.Scte35DeliveryRestrictions
    -- ^ Holds the four SCTE-35 delivery restriction parameters.
  , segmentNum :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
  , segmentationDuration :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
  , segmentationTypeId :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
  , segmentationUpid :: Core.Maybe Core.Text
    -- ^ Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
  , segmentationUpidType :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
  , segmentsExpected :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
  , subSegmentNum :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
  , subSegmentsExpected :: Core.Maybe Core.Natural
    -- ^ Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35SegmentationDescriptor' value with any optional fields omitted.
mkScte35SegmentationDescriptor
    :: Core.Natural -- ^ 'segmentationEventId'
    -> Types.Scte35SegmentationCancelIndicator -- ^ 'segmentationCancelIndicator'
    -> Scte35SegmentationDescriptor
mkScte35SegmentationDescriptor segmentationEventId
  segmentationCancelIndicator
  = Scte35SegmentationDescriptor'{segmentationEventId,
                                  segmentationCancelIndicator, deliveryRestrictions = Core.Nothing,
                                  segmentNum = Core.Nothing, segmentationDuration = Core.Nothing,
                                  segmentationTypeId = Core.Nothing,
                                  segmentationUpid = Core.Nothing,
                                  segmentationUpidType = Core.Nothing,
                                  segmentsExpected = Core.Nothing, subSegmentNum = Core.Nothing,
                                  subSegmentsExpected = Core.Nothing}

-- | Corresponds to SCTE-35 segmentation_event_id. 
--
-- /Note:/ Consider using 'segmentationEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationEventId :: Lens.Lens' Scte35SegmentationDescriptor Core.Natural
ssdSegmentationEventId = Lens.field @"segmentationEventId"
{-# INLINEABLE ssdSegmentationEventId #-}
{-# DEPRECATED segmentationEventId "Use generic-lens or generic-optics with 'segmentationEventId' instead"  #-}

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
--
-- /Note:/ Consider using 'segmentationCancelIndicator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationCancelIndicator :: Lens.Lens' Scte35SegmentationDescriptor Types.Scte35SegmentationCancelIndicator
ssdSegmentationCancelIndicator = Lens.field @"segmentationCancelIndicator"
{-# INLINEABLE ssdSegmentationCancelIndicator #-}
{-# DEPRECATED segmentationCancelIndicator "Use generic-lens or generic-optics with 'segmentationCancelIndicator' instead"  #-}

-- | Holds the four SCTE-35 delivery restriction parameters.
--
-- /Note:/ Consider using 'deliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdDeliveryRestrictions :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Types.Scte35DeliveryRestrictions)
ssdDeliveryRestrictions = Lens.field @"deliveryRestrictions"
{-# INLINEABLE ssdDeliveryRestrictions #-}
{-# DEPRECATED deliveryRestrictions "Use generic-lens or generic-optics with 'deliveryRestrictions' instead"  #-}

-- | Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'segmentNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSegmentNum = Lens.field @"segmentNum"
{-# INLINEABLE ssdSegmentNum #-}
{-# DEPRECATED segmentNum "Use generic-lens or generic-optics with 'segmentNum' instead"  #-}

-- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
--
-- /Note:/ Consider using 'segmentationDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationDuration :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSegmentationDuration = Lens.field @"segmentationDuration"
{-# INLINEABLE ssdSegmentationDuration #-}
{-# DEPRECATED segmentationDuration "Use generic-lens or generic-optics with 'segmentationDuration' instead"  #-}

-- | Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
--
-- /Note:/ Consider using 'segmentationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationTypeId :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSegmentationTypeId = Lens.field @"segmentationTypeId"
{-# INLINEABLE ssdSegmentationTypeId #-}
{-# DEPRECATED segmentationTypeId "Use generic-lens or generic-optics with 'segmentationTypeId' instead"  #-}

-- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
--
-- /Note:/ Consider using 'segmentationUpid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationUpid :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Text)
ssdSegmentationUpid = Lens.field @"segmentationUpid"
{-# INLINEABLE ssdSegmentationUpid #-}
{-# DEPRECATED segmentationUpid "Use generic-lens or generic-optics with 'segmentationUpid' instead"  #-}

-- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
--
-- /Note:/ Consider using 'segmentationUpidType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationUpidType :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSegmentationUpidType = Lens.field @"segmentationUpidType"
{-# INLINEABLE ssdSegmentationUpidType #-}
{-# DEPRECATED segmentationUpidType "Use generic-lens or generic-optics with 'segmentationUpidType' instead"  #-}

-- | Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'segmentsExpected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSegmentsExpected = Lens.field @"segmentsExpected"
{-# INLINEABLE ssdSegmentsExpected #-}
{-# DEPRECATED segmentsExpected "Use generic-lens or generic-optics with 'segmentsExpected' instead"  #-}

-- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'subSegmentNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSubSegmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSubSegmentNum = Lens.field @"subSegmentNum"
{-# INLINEABLE ssdSubSegmentNum #-}
{-# DEPRECATED subSegmentNum "Use generic-lens or generic-optics with 'subSegmentNum' instead"  #-}

-- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'subSegmentsExpected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSubSegmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Core.Maybe Core.Natural)
ssdSubSegmentsExpected = Lens.field @"subSegmentsExpected"
{-# INLINEABLE ssdSubSegmentsExpected #-}
{-# DEPRECATED subSegmentsExpected "Use generic-lens or generic-optics with 'subSegmentsExpected' instead"  #-}

instance Core.FromJSON Scte35SegmentationDescriptor where
        toJSON Scte35SegmentationDescriptor{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("segmentationEventId" Core..= segmentationEventId),
                  Core.Just
                    ("segmentationCancelIndicator" Core..=
                       segmentationCancelIndicator),
                  ("deliveryRestrictions" Core..=) Core.<$> deliveryRestrictions,
                  ("segmentNum" Core..=) Core.<$> segmentNum,
                  ("segmentationDuration" Core..=) Core.<$> segmentationDuration,
                  ("segmentationTypeId" Core..=) Core.<$> segmentationTypeId,
                  ("segmentationUpid" Core..=) Core.<$> segmentationUpid,
                  ("segmentationUpidType" Core..=) Core.<$> segmentationUpidType,
                  ("segmentsExpected" Core..=) Core.<$> segmentsExpected,
                  ("subSegmentNum" Core..=) Core.<$> subSegmentNum,
                  ("subSegmentsExpected" Core..=) Core.<$> subSegmentsExpected])

instance Core.FromJSON Scte35SegmentationDescriptor where
        parseJSON
          = Core.withObject "Scte35SegmentationDescriptor" Core.$
              \ x ->
                Scte35SegmentationDescriptor' Core.<$>
                  (x Core..: "segmentationEventId") Core.<*>
                    x Core..: "segmentationCancelIndicator"
                    Core.<*> x Core..:? "deliveryRestrictions"
                    Core.<*> x Core..:? "segmentNum"
                    Core.<*> x Core..:? "segmentationDuration"
                    Core.<*> x Core..:? "segmentationTypeId"
                    Core.<*> x Core..:? "segmentationUpid"
                    Core.<*> x Core..:? "segmentationUpidType"
                    Core.<*> x Core..:? "segmentsExpected"
                    Core.<*> x Core..:? "subSegmentNum"
                    Core.<*> x Core..:? "subSegmentsExpected"
