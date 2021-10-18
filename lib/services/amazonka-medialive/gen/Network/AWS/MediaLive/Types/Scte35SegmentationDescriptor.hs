{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
import Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
import qualified Network.AWS.Prelude as Prelude

-- | Corresponds to SCTE-35 segmentation_descriptor.
--
-- /See:/ 'newScte35SegmentationDescriptor' smart constructor.
data Scte35SegmentationDescriptor = Scte35SegmentationDescriptor'
  { -- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the
    -- specified segmentation_type_id.
    subSegmentNum :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the
    -- hexadecimal representation of the characters that make up the SCTE-35
    -- segmentation_upid value. Must contain an even number of hex characters.
    -- Do not include spaces between each hex pair. For example, the ASCII
    -- \"ADS Information\" becomes hex \"41445320496e666f726d6174696f6e.
    segmentationUpid :: Prelude.Maybe Prelude.Text,
    -- | Corresponds to SCTE-35 segment_num. A value that is valid for the
    -- specified segmentation_type_id.
    segmentNum :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for
    -- the specified segmentation_type_id.
    subSegmentsExpected :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one
    -- of the types listed in the SCTE-35 specification, converted to a
    -- decimal. For example, \"0x0C\" hex from the specification is \"12\" in
    -- decimal. In the CLI, API, or an SDK, enter one of the types listed in
    -- the SCTE-35 specification, in either hex (for example, \"0x0C\" ) or in
    -- decimal (for example, \"12\").
    segmentationUpidType :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segments_expected. A value that is valid for the
    -- specified segmentation_type_id.
    segmentsExpected :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_type_id. One of the
    -- segmentation_type_id values listed in the SCTE-35 specification. On the
    -- console, enter the ID in decimal (for example, \"52\"). In the CLI, API,
    -- or an SDK, enter the ID in hex (for example, \"0x34\") or decimal (for
    -- example, \"52\").
    segmentationTypeId :: Prelude.Maybe Prelude.Natural,
    -- | Holds the four SCTE-35 delivery restriction parameters.
    deliveryRestrictions :: Prelude.Maybe Scte35DeliveryRestrictions,
    -- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for
    -- the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple
    -- the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not
    -- enter a duration, the time_signal will continue until you insert a
    -- cancellation message.
    segmentationDuration :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_event_id.
    segmentationEventId :: Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
    segmentationCancelIndicator :: Scte35SegmentationCancelIndicator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte35SegmentationDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subSegmentNum', 'scte35SegmentationDescriptor_subSegmentNum' - Corresponds to SCTE-35 sub_segment_num. A value that is valid for the
-- specified segmentation_type_id.
--
-- 'segmentationUpid', 'scte35SegmentationDescriptor_segmentationUpid' - Corresponds to SCTE-35 segmentation_upid. Enter a string containing the
-- hexadecimal representation of the characters that make up the SCTE-35
-- segmentation_upid value. Must contain an even number of hex characters.
-- Do not include spaces between each hex pair. For example, the ASCII
-- \"ADS Information\" becomes hex \"41445320496e666f726d6174696f6e.
--
-- 'segmentNum', 'scte35SegmentationDescriptor_segmentNum' - Corresponds to SCTE-35 segment_num. A value that is valid for the
-- specified segmentation_type_id.
--
-- 'subSegmentsExpected', 'scte35SegmentationDescriptor_subSegmentsExpected' - Corresponds to SCTE-35 sub_segments_expected. A value that is valid for
-- the specified segmentation_type_id.
--
-- 'segmentationUpidType', 'scte35SegmentationDescriptor_segmentationUpidType' - Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one
-- of the types listed in the SCTE-35 specification, converted to a
-- decimal. For example, \"0x0C\" hex from the specification is \"12\" in
-- decimal. In the CLI, API, or an SDK, enter one of the types listed in
-- the SCTE-35 specification, in either hex (for example, \"0x0C\" ) or in
-- decimal (for example, \"12\").
--
-- 'segmentsExpected', 'scte35SegmentationDescriptor_segmentsExpected' - Corresponds to SCTE-35 segments_expected. A value that is valid for the
-- specified segmentation_type_id.
--
-- 'segmentationTypeId', 'scte35SegmentationDescriptor_segmentationTypeId' - Corresponds to SCTE-35 segmentation_type_id. One of the
-- segmentation_type_id values listed in the SCTE-35 specification. On the
-- console, enter the ID in decimal (for example, \"52\"). In the CLI, API,
-- or an SDK, enter the ID in hex (for example, \"0x34\") or decimal (for
-- example, \"52\").
--
-- 'deliveryRestrictions', 'scte35SegmentationDescriptor_deliveryRestrictions' - Holds the four SCTE-35 delivery restriction parameters.
--
-- 'segmentationDuration', 'scte35SegmentationDescriptor_segmentationDuration' - Corresponds to SCTE-35 segmentation_duration. Optional. The duration for
-- the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple
-- the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not
-- enter a duration, the time_signal will continue until you insert a
-- cancellation message.
--
-- 'segmentationEventId', 'scte35SegmentationDescriptor_segmentationEventId' - Corresponds to SCTE-35 segmentation_event_id.
--
-- 'segmentationCancelIndicator', 'scte35SegmentationDescriptor_segmentationCancelIndicator' - Corresponds to SCTE-35 segmentation_event_cancel_indicator.
newScte35SegmentationDescriptor ::
  -- | 'segmentationEventId'
  Prelude.Natural ->
  -- | 'segmentationCancelIndicator'
  Scte35SegmentationCancelIndicator ->
  Scte35SegmentationDescriptor
newScte35SegmentationDescriptor
  pSegmentationEventId_
  pSegmentationCancelIndicator_ =
    Scte35SegmentationDescriptor'
      { subSegmentNum =
          Prelude.Nothing,
        segmentationUpid = Prelude.Nothing,
        segmentNum = Prelude.Nothing,
        subSegmentsExpected = Prelude.Nothing,
        segmentationUpidType = Prelude.Nothing,
        segmentsExpected = Prelude.Nothing,
        segmentationTypeId = Prelude.Nothing,
        deliveryRestrictions = Prelude.Nothing,
        segmentationDuration = Prelude.Nothing,
        segmentationEventId = pSegmentationEventId_,
        segmentationCancelIndicator =
          pSegmentationCancelIndicator_
      }

-- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the
-- specified segmentation_type_id.
scte35SegmentationDescriptor_subSegmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_subSegmentNum = Lens.lens (\Scte35SegmentationDescriptor' {subSegmentNum} -> subSegmentNum) (\s@Scte35SegmentationDescriptor' {} a -> s {subSegmentNum = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the
-- hexadecimal representation of the characters that make up the SCTE-35
-- segmentation_upid value. Must contain an even number of hex characters.
-- Do not include spaces between each hex pair. For example, the ASCII
-- \"ADS Information\" becomes hex \"41445320496e666f726d6174696f6e.
scte35SegmentationDescriptor_segmentationUpid :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Text)
scte35SegmentationDescriptor_segmentationUpid = Lens.lens (\Scte35SegmentationDescriptor' {segmentationUpid} -> segmentationUpid) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationUpid = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segment_num. A value that is valid for the
-- specified segmentation_type_id.
scte35SegmentationDescriptor_segmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentNum = Lens.lens (\Scte35SegmentationDescriptor' {segmentNum} -> segmentNum) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentNum = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for
-- the specified segmentation_type_id.
scte35SegmentationDescriptor_subSegmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_subSegmentsExpected = Lens.lens (\Scte35SegmentationDescriptor' {subSegmentsExpected} -> subSegmentsExpected) (\s@Scte35SegmentationDescriptor' {} a -> s {subSegmentsExpected = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one
-- of the types listed in the SCTE-35 specification, converted to a
-- decimal. For example, \"0x0C\" hex from the specification is \"12\" in
-- decimal. In the CLI, API, or an SDK, enter one of the types listed in
-- the SCTE-35 specification, in either hex (for example, \"0x0C\" ) or in
-- decimal (for example, \"12\").
scte35SegmentationDescriptor_segmentationUpidType :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentationUpidType = Lens.lens (\Scte35SegmentationDescriptor' {segmentationUpidType} -> segmentationUpidType) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationUpidType = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segments_expected. A value that is valid for the
-- specified segmentation_type_id.
scte35SegmentationDescriptor_segmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentsExpected = Lens.lens (\Scte35SegmentationDescriptor' {segmentsExpected} -> segmentsExpected) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentsExpected = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_type_id. One of the
-- segmentation_type_id values listed in the SCTE-35 specification. On the
-- console, enter the ID in decimal (for example, \"52\"). In the CLI, API,
-- or an SDK, enter the ID in hex (for example, \"0x34\") or decimal (for
-- example, \"52\").
scte35SegmentationDescriptor_segmentationTypeId :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentationTypeId = Lens.lens (\Scte35SegmentationDescriptor' {segmentationTypeId} -> segmentationTypeId) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationTypeId = a} :: Scte35SegmentationDescriptor)

-- | Holds the four SCTE-35 delivery restriction parameters.
scte35SegmentationDescriptor_deliveryRestrictions :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Scte35DeliveryRestrictions)
scte35SegmentationDescriptor_deliveryRestrictions = Lens.lens (\Scte35SegmentationDescriptor' {deliveryRestrictions} -> deliveryRestrictions) (\s@Scte35SegmentationDescriptor' {} a -> s {deliveryRestrictions = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for
-- the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple
-- the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not
-- enter a duration, the time_signal will continue until you insert a
-- cancellation message.
scte35SegmentationDescriptor_segmentationDuration :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentationDuration = Lens.lens (\Scte35SegmentationDescriptor' {segmentationDuration} -> segmentationDuration) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationDuration = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_event_id.
scte35SegmentationDescriptor_segmentationEventId :: Lens.Lens' Scte35SegmentationDescriptor Prelude.Natural
scte35SegmentationDescriptor_segmentationEventId = Lens.lens (\Scte35SegmentationDescriptor' {segmentationEventId} -> segmentationEventId) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationEventId = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
scte35SegmentationDescriptor_segmentationCancelIndicator :: Lens.Lens' Scte35SegmentationDescriptor Scte35SegmentationCancelIndicator
scte35SegmentationDescriptor_segmentationCancelIndicator = Lens.lens (\Scte35SegmentationDescriptor' {segmentationCancelIndicator} -> segmentationCancelIndicator) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationCancelIndicator = a} :: Scte35SegmentationDescriptor)

instance Core.FromJSON Scte35SegmentationDescriptor where
  parseJSON =
    Core.withObject
      "Scte35SegmentationDescriptor"
      ( \x ->
          Scte35SegmentationDescriptor'
            Prelude.<$> (x Core..:? "subSegmentNum")
            Prelude.<*> (x Core..:? "segmentationUpid")
            Prelude.<*> (x Core..:? "segmentNum")
            Prelude.<*> (x Core..:? "subSegmentsExpected")
            Prelude.<*> (x Core..:? "segmentationUpidType")
            Prelude.<*> (x Core..:? "segmentsExpected")
            Prelude.<*> (x Core..:? "segmentationTypeId")
            Prelude.<*> (x Core..:? "deliveryRestrictions")
            Prelude.<*> (x Core..:? "segmentationDuration")
            Prelude.<*> (x Core..: "segmentationEventId")
            Prelude.<*> (x Core..: "segmentationCancelIndicator")
      )

instance
  Prelude.Hashable
    Scte35SegmentationDescriptor

instance Prelude.NFData Scte35SegmentationDescriptor

instance Core.ToJSON Scte35SegmentationDescriptor where
  toJSON Scte35SegmentationDescriptor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("subSegmentNum" Core..=) Prelude.<$> subSegmentNum,
            ("segmentationUpid" Core..=)
              Prelude.<$> segmentationUpid,
            ("segmentNum" Core..=) Prelude.<$> segmentNum,
            ("subSegmentsExpected" Core..=)
              Prelude.<$> subSegmentsExpected,
            ("segmentationUpidType" Core..=)
              Prelude.<$> segmentationUpidType,
            ("segmentsExpected" Core..=)
              Prelude.<$> segmentsExpected,
            ("segmentationTypeId" Core..=)
              Prelude.<$> segmentationTypeId,
            ("deliveryRestrictions" Core..=)
              Prelude.<$> deliveryRestrictions,
            ("segmentationDuration" Core..=)
              Prelude.<$> segmentationDuration,
            Prelude.Just
              ("segmentationEventId" Core..= segmentationEventId),
            Prelude.Just
              ( "segmentationCancelIndicator"
                  Core..= segmentationCancelIndicator
              )
          ]
      )
