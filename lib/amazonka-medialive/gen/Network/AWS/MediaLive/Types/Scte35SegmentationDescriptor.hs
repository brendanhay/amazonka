{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
  ( Scte35SegmentationDescriptor (..),

    -- * Smart constructor
    mkScte35SegmentationDescriptor,

    -- * Lenses
    ssdSegmentationEventId,
    ssdSegmentationUpidType,
    ssdSegmentsExpected,
    ssdSegmentationCancelIndicator,
    ssdSubSegmentsExpected,
    ssdSegmentNum,
    ssdSegmentationDuration,
    ssdSegmentationTypeId,
    ssdDeliveryRestrictions,
    ssdSegmentationUpid,
    ssdSubSegmentNum,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
import Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
import qualified Network.AWS.Prelude as Lude

-- | Corresponds to SCTE-35 segmentation_descriptor.
--
-- /See:/ 'mkScte35SegmentationDescriptor' smart constructor.
data Scte35SegmentationDescriptor = Scte35SegmentationDescriptor'
  { -- | Corresponds to SCTE-35 segmentation_event_id.
    segmentationEventId :: Lude.Natural,
    -- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
    segmentationUpidType :: Lude.Maybe Lude.Natural,
    -- | Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
    segmentsExpected :: Lude.Maybe Lude.Natural,
    -- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
    segmentationCancelIndicator :: Scte35SegmentationCancelIndicator,
    -- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
    subSegmentsExpected :: Lude.Maybe Lude.Natural,
    -- | Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
    segmentNum :: Lude.Maybe Lude.Natural,
    -- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
    segmentationDuration :: Lude.Maybe Lude.Natural,
    -- | Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
    segmentationTypeId :: Lude.Maybe Lude.Natural,
    -- | Holds the four SCTE-35 delivery restriction parameters.
    deliveryRestrictions :: Lude.Maybe Scte35DeliveryRestrictions,
    -- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
    segmentationUpid :: Lude.Maybe Lude.Text,
    -- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
    subSegmentNum :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35SegmentationDescriptor' with the minimum fields required to make a request.
--
-- * 'segmentationEventId' - Corresponds to SCTE-35 segmentation_event_id.
-- * 'segmentationUpidType' - Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
-- * 'segmentsExpected' - Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
-- * 'segmentationCancelIndicator' - Corresponds to SCTE-35 segmentation_event_cancel_indicator.
-- * 'subSegmentsExpected' - Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
-- * 'segmentNum' - Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
-- * 'segmentationDuration' - Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
-- * 'segmentationTypeId' - Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
-- * 'deliveryRestrictions' - Holds the four SCTE-35 delivery restriction parameters.
-- * 'segmentationUpid' - Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
-- * 'subSegmentNum' - Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
mkScte35SegmentationDescriptor ::
  -- | 'segmentationEventId'
  Lude.Natural ->
  -- | 'segmentationCancelIndicator'
  Scte35SegmentationCancelIndicator ->
  Scte35SegmentationDescriptor
mkScte35SegmentationDescriptor
  pSegmentationEventId_
  pSegmentationCancelIndicator_ =
    Scte35SegmentationDescriptor'
      { segmentationEventId =
          pSegmentationEventId_,
        segmentationUpidType = Lude.Nothing,
        segmentsExpected = Lude.Nothing,
        segmentationCancelIndicator = pSegmentationCancelIndicator_,
        subSegmentsExpected = Lude.Nothing,
        segmentNum = Lude.Nothing,
        segmentationDuration = Lude.Nothing,
        segmentationTypeId = Lude.Nothing,
        deliveryRestrictions = Lude.Nothing,
        segmentationUpid = Lude.Nothing,
        subSegmentNum = Lude.Nothing
      }

-- | Corresponds to SCTE-35 segmentation_event_id.
--
-- /Note:/ Consider using 'segmentationEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationEventId :: Lens.Lens' Scte35SegmentationDescriptor Lude.Natural
ssdSegmentationEventId = Lens.lens (segmentationEventId :: Scte35SegmentationDescriptor -> Lude.Natural) (\s a -> s {segmentationEventId = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentationEventId "Use generic-lens or generic-optics with 'segmentationEventId' instead." #-}

-- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
--
-- /Note:/ Consider using 'segmentationUpidType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationUpidType :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSegmentationUpidType = Lens.lens (segmentationUpidType :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {segmentationUpidType = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentationUpidType "Use generic-lens or generic-optics with 'segmentationUpidType' instead." #-}

-- | Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'segmentsExpected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSegmentsExpected = Lens.lens (segmentsExpected :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {segmentsExpected = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentsExpected "Use generic-lens or generic-optics with 'segmentsExpected' instead." #-}

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
--
-- /Note:/ Consider using 'segmentationCancelIndicator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationCancelIndicator :: Lens.Lens' Scte35SegmentationDescriptor Scte35SegmentationCancelIndicator
ssdSegmentationCancelIndicator = Lens.lens (segmentationCancelIndicator :: Scte35SegmentationDescriptor -> Scte35SegmentationCancelIndicator) (\s a -> s {segmentationCancelIndicator = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentationCancelIndicator "Use generic-lens or generic-optics with 'segmentationCancelIndicator' instead." #-}

-- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'subSegmentsExpected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSubSegmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSubSegmentsExpected = Lens.lens (subSegmentsExpected :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {subSegmentsExpected = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSubSegmentsExpected "Use generic-lens or generic-optics with 'subSegmentsExpected' instead." #-}

-- | Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'segmentNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSegmentNum = Lens.lens (segmentNum :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {segmentNum = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentNum "Use generic-lens or generic-optics with 'segmentNum' instead." #-}

-- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
--
-- /Note:/ Consider using 'segmentationDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationDuration :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSegmentationDuration = Lens.lens (segmentationDuration :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {segmentationDuration = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentationDuration "Use generic-lens or generic-optics with 'segmentationDuration' instead." #-}

-- | Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
--
-- /Note:/ Consider using 'segmentationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationTypeId :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSegmentationTypeId = Lens.lens (segmentationTypeId :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {segmentationTypeId = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentationTypeId "Use generic-lens or generic-optics with 'segmentationTypeId' instead." #-}

-- | Holds the four SCTE-35 delivery restriction parameters.
--
-- /Note:/ Consider using 'deliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdDeliveryRestrictions :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Scte35DeliveryRestrictions)
ssdDeliveryRestrictions = Lens.lens (deliveryRestrictions :: Scte35SegmentationDescriptor -> Lude.Maybe Scte35DeliveryRestrictions) (\s a -> s {deliveryRestrictions = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdDeliveryRestrictions "Use generic-lens or generic-optics with 'deliveryRestrictions' instead." #-}

-- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
--
-- /Note:/ Consider using 'segmentationUpid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentationUpid :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Text)
ssdSegmentationUpid = Lens.lens (segmentationUpid :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Text) (\s a -> s {segmentationUpid = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSegmentationUpid "Use generic-lens or generic-optics with 'segmentationUpid' instead." #-}

-- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
--
-- /Note:/ Consider using 'subSegmentNum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSubSegmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Lude.Maybe Lude.Natural)
ssdSubSegmentNum = Lens.lens (subSegmentNum :: Scte35SegmentationDescriptor -> Lude.Maybe Lude.Natural) (\s a -> s {subSegmentNum = a} :: Scte35SegmentationDescriptor)
{-# DEPRECATED ssdSubSegmentNum "Use generic-lens or generic-optics with 'subSegmentNum' instead." #-}

instance Lude.FromJSON Scte35SegmentationDescriptor where
  parseJSON =
    Lude.withObject
      "Scte35SegmentationDescriptor"
      ( \x ->
          Scte35SegmentationDescriptor'
            Lude.<$> (x Lude..: "segmentationEventId")
            Lude.<*> (x Lude..:? "segmentationUpidType")
            Lude.<*> (x Lude..:? "segmentsExpected")
            Lude.<*> (x Lude..: "segmentationCancelIndicator")
            Lude.<*> (x Lude..:? "subSegmentsExpected")
            Lude.<*> (x Lude..:? "segmentNum")
            Lude.<*> (x Lude..:? "segmentationDuration")
            Lude.<*> (x Lude..:? "segmentationTypeId")
            Lude.<*> (x Lude..:? "deliveryRestrictions")
            Lude.<*> (x Lude..:? "segmentationUpid")
            Lude.<*> (x Lude..:? "subSegmentNum")
      )

instance Lude.ToJSON Scte35SegmentationDescriptor where
  toJSON Scte35SegmentationDescriptor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("segmentationEventId" Lude..= segmentationEventId),
            ("segmentationUpidType" Lude..=) Lude.<$> segmentationUpidType,
            ("segmentsExpected" Lude..=) Lude.<$> segmentsExpected,
            Lude.Just
              ( "segmentationCancelIndicator"
                  Lude..= segmentationCancelIndicator
              ),
            ("subSegmentsExpected" Lude..=) Lude.<$> subSegmentsExpected,
            ("segmentNum" Lude..=) Lude.<$> segmentNum,
            ("segmentationDuration" Lude..=) Lude.<$> segmentationDuration,
            ("segmentationTypeId" Lude..=) Lude.<$> segmentationTypeId,
            ("deliveryRestrictions" Lude..=) Lude.<$> deliveryRestrictions,
            ("segmentationUpid" Lude..=) Lude.<$> segmentationUpid,
            ("subSegmentNum" Lude..=) Lude.<$> subSegmentNum
          ]
      )
