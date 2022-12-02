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
-- Module      : Amazonka.MediaLive.Types.Scte35SegmentationDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35SegmentationDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Scte35DeliveryRestrictions
import Amazonka.MediaLive.Types.Scte35SegmentationCancelIndicator
import qualified Amazonka.Prelude as Prelude

-- | Corresponds to SCTE-35 segmentation_descriptor.
--
-- /See:/ 'newScte35SegmentationDescriptor' smart constructor.
data Scte35SegmentationDescriptor = Scte35SegmentationDescriptor'
  { -- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for
    -- the specified segmentation_type_id.
    subSegmentsExpected :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for
    -- the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple
    -- the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not
    -- enter a duration, the time_signal will continue until you insert a
    -- cancellation message.
    segmentationDuration :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_type_id. One of the
    -- segmentation_type_id values listed in the SCTE-35 specification. On the
    -- console, enter the ID in decimal (for example, \"52\"). In the CLI, API,
    -- or an SDK, enter the ID in hex (for example, \"0x34\") or decimal (for
    -- example, \"52\").
    segmentationTypeId :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one
    -- of the types listed in the SCTE-35 specification, converted to a
    -- decimal. For example, \"0x0C\" hex from the specification is \"12\" in
    -- decimal. In the CLI, API, or an SDK, enter one of the types listed in
    -- the SCTE-35 specification, in either hex (for example, \"0x0C\" ) or in
    -- decimal (for example, \"12\").
    segmentationUpidType :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segment_num. A value that is valid for the
    -- specified segmentation_type_id.
    segmentNum :: Prelude.Maybe Prelude.Natural,
    -- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the
    -- hexadecimal representation of the characters that make up the SCTE-35
    -- segmentation_upid value. Must contain an even number of hex characters.
    -- Do not include spaces between each hex pair. For example, the ASCII
    -- \"ADS Information\" becomes hex \"41445320496e666f726d6174696f6e.
    segmentationUpid :: Prelude.Maybe Prelude.Text,
    -- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the
    -- specified segmentation_type_id.
    subSegmentNum :: Prelude.Maybe Prelude.Natural,
    -- | Holds the four SCTE-35 delivery restriction parameters.
    deliveryRestrictions :: Prelude.Maybe Scte35DeliveryRestrictions,
    -- | Corresponds to SCTE-35 segments_expected. A value that is valid for the
    -- specified segmentation_type_id.
    segmentsExpected :: Prelude.Maybe Prelude.Natural,
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
-- 'subSegmentsExpected', 'scte35SegmentationDescriptor_subSegmentsExpected' - Corresponds to SCTE-35 sub_segments_expected. A value that is valid for
-- the specified segmentation_type_id.
--
-- 'segmentationDuration', 'scte35SegmentationDescriptor_segmentationDuration' - Corresponds to SCTE-35 segmentation_duration. Optional. The duration for
-- the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple
-- the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not
-- enter a duration, the time_signal will continue until you insert a
-- cancellation message.
--
-- 'segmentationTypeId', 'scte35SegmentationDescriptor_segmentationTypeId' - Corresponds to SCTE-35 segmentation_type_id. One of the
-- segmentation_type_id values listed in the SCTE-35 specification. On the
-- console, enter the ID in decimal (for example, \"52\"). In the CLI, API,
-- or an SDK, enter the ID in hex (for example, \"0x34\") or decimal (for
-- example, \"52\").
--
-- 'segmentationUpidType', 'scte35SegmentationDescriptor_segmentationUpidType' - Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one
-- of the types listed in the SCTE-35 specification, converted to a
-- decimal. For example, \"0x0C\" hex from the specification is \"12\" in
-- decimal. In the CLI, API, or an SDK, enter one of the types listed in
-- the SCTE-35 specification, in either hex (for example, \"0x0C\" ) or in
-- decimal (for example, \"12\").
--
-- 'segmentNum', 'scte35SegmentationDescriptor_segmentNum' - Corresponds to SCTE-35 segment_num. A value that is valid for the
-- specified segmentation_type_id.
--
-- 'segmentationUpid', 'scte35SegmentationDescriptor_segmentationUpid' - Corresponds to SCTE-35 segmentation_upid. Enter a string containing the
-- hexadecimal representation of the characters that make up the SCTE-35
-- segmentation_upid value. Must contain an even number of hex characters.
-- Do not include spaces between each hex pair. For example, the ASCII
-- \"ADS Information\" becomes hex \"41445320496e666f726d6174696f6e.
--
-- 'subSegmentNum', 'scte35SegmentationDescriptor_subSegmentNum' - Corresponds to SCTE-35 sub_segment_num. A value that is valid for the
-- specified segmentation_type_id.
--
-- 'deliveryRestrictions', 'scte35SegmentationDescriptor_deliveryRestrictions' - Holds the four SCTE-35 delivery restriction parameters.
--
-- 'segmentsExpected', 'scte35SegmentationDescriptor_segmentsExpected' - Corresponds to SCTE-35 segments_expected. A value that is valid for the
-- specified segmentation_type_id.
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
      { subSegmentsExpected =
          Prelude.Nothing,
        segmentationDuration = Prelude.Nothing,
        segmentationTypeId = Prelude.Nothing,
        segmentationUpidType = Prelude.Nothing,
        segmentNum = Prelude.Nothing,
        segmentationUpid = Prelude.Nothing,
        subSegmentNum = Prelude.Nothing,
        deliveryRestrictions = Prelude.Nothing,
        segmentsExpected = Prelude.Nothing,
        segmentationEventId = pSegmentationEventId_,
        segmentationCancelIndicator =
          pSegmentationCancelIndicator_
      }

-- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for
-- the specified segmentation_type_id.
scte35SegmentationDescriptor_subSegmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_subSegmentsExpected = Lens.lens (\Scte35SegmentationDescriptor' {subSegmentsExpected} -> subSegmentsExpected) (\s@Scte35SegmentationDescriptor' {} a -> s {subSegmentsExpected = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for
-- the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple
-- the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not
-- enter a duration, the time_signal will continue until you insert a
-- cancellation message.
scte35SegmentationDescriptor_segmentationDuration :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentationDuration = Lens.lens (\Scte35SegmentationDescriptor' {segmentationDuration} -> segmentationDuration) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationDuration = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_type_id. One of the
-- segmentation_type_id values listed in the SCTE-35 specification. On the
-- console, enter the ID in decimal (for example, \"52\"). In the CLI, API,
-- or an SDK, enter the ID in hex (for example, \"0x34\") or decimal (for
-- example, \"52\").
scte35SegmentationDescriptor_segmentationTypeId :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentationTypeId = Lens.lens (\Scte35SegmentationDescriptor' {segmentationTypeId} -> segmentationTypeId) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationTypeId = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one
-- of the types listed in the SCTE-35 specification, converted to a
-- decimal. For example, \"0x0C\" hex from the specification is \"12\" in
-- decimal. In the CLI, API, or an SDK, enter one of the types listed in
-- the SCTE-35 specification, in either hex (for example, \"0x0C\" ) or in
-- decimal (for example, \"12\").
scte35SegmentationDescriptor_segmentationUpidType :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentationUpidType = Lens.lens (\Scte35SegmentationDescriptor' {segmentationUpidType} -> segmentationUpidType) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationUpidType = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segment_num. A value that is valid for the
-- specified segmentation_type_id.
scte35SegmentationDescriptor_segmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentNum = Lens.lens (\Scte35SegmentationDescriptor' {segmentNum} -> segmentNum) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentNum = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the
-- hexadecimal representation of the characters that make up the SCTE-35
-- segmentation_upid value. Must contain an even number of hex characters.
-- Do not include spaces between each hex pair. For example, the ASCII
-- \"ADS Information\" becomes hex \"41445320496e666f726d6174696f6e.
scte35SegmentationDescriptor_segmentationUpid :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Text)
scte35SegmentationDescriptor_segmentationUpid = Lens.lens (\Scte35SegmentationDescriptor' {segmentationUpid} -> segmentationUpid) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationUpid = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the
-- specified segmentation_type_id.
scte35SegmentationDescriptor_subSegmentNum :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_subSegmentNum = Lens.lens (\Scte35SegmentationDescriptor' {subSegmentNum} -> subSegmentNum) (\s@Scte35SegmentationDescriptor' {} a -> s {subSegmentNum = a} :: Scte35SegmentationDescriptor)

-- | Holds the four SCTE-35 delivery restriction parameters.
scte35SegmentationDescriptor_deliveryRestrictions :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Scte35DeliveryRestrictions)
scte35SegmentationDescriptor_deliveryRestrictions = Lens.lens (\Scte35SegmentationDescriptor' {deliveryRestrictions} -> deliveryRestrictions) (\s@Scte35SegmentationDescriptor' {} a -> s {deliveryRestrictions = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segments_expected. A value that is valid for the
-- specified segmentation_type_id.
scte35SegmentationDescriptor_segmentsExpected :: Lens.Lens' Scte35SegmentationDescriptor (Prelude.Maybe Prelude.Natural)
scte35SegmentationDescriptor_segmentsExpected = Lens.lens (\Scte35SegmentationDescriptor' {segmentsExpected} -> segmentsExpected) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentsExpected = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_event_id.
scte35SegmentationDescriptor_segmentationEventId :: Lens.Lens' Scte35SegmentationDescriptor Prelude.Natural
scte35SegmentationDescriptor_segmentationEventId = Lens.lens (\Scte35SegmentationDescriptor' {segmentationEventId} -> segmentationEventId) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationEventId = a} :: Scte35SegmentationDescriptor)

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
scte35SegmentationDescriptor_segmentationCancelIndicator :: Lens.Lens' Scte35SegmentationDescriptor Scte35SegmentationCancelIndicator
scte35SegmentationDescriptor_segmentationCancelIndicator = Lens.lens (\Scte35SegmentationDescriptor' {segmentationCancelIndicator} -> segmentationCancelIndicator) (\s@Scte35SegmentationDescriptor' {} a -> s {segmentationCancelIndicator = a} :: Scte35SegmentationDescriptor)

instance Data.FromJSON Scte35SegmentationDescriptor where
  parseJSON =
    Data.withObject
      "Scte35SegmentationDescriptor"
      ( \x ->
          Scte35SegmentationDescriptor'
            Prelude.<$> (x Data..:? "subSegmentsExpected")
            Prelude.<*> (x Data..:? "segmentationDuration")
            Prelude.<*> (x Data..:? "segmentationTypeId")
            Prelude.<*> (x Data..:? "segmentationUpidType")
            Prelude.<*> (x Data..:? "segmentNum")
            Prelude.<*> (x Data..:? "segmentationUpid")
            Prelude.<*> (x Data..:? "subSegmentNum")
            Prelude.<*> (x Data..:? "deliveryRestrictions")
            Prelude.<*> (x Data..:? "segmentsExpected")
            Prelude.<*> (x Data..: "segmentationEventId")
            Prelude.<*> (x Data..: "segmentationCancelIndicator")
      )

instance
  Prelude.Hashable
    Scte35SegmentationDescriptor
  where
  hashWithSalt _salt Scte35SegmentationDescriptor' {..} =
    _salt `Prelude.hashWithSalt` subSegmentsExpected
      `Prelude.hashWithSalt` segmentationDuration
      `Prelude.hashWithSalt` segmentationTypeId
      `Prelude.hashWithSalt` segmentationUpidType
      `Prelude.hashWithSalt` segmentNum
      `Prelude.hashWithSalt` segmentationUpid
      `Prelude.hashWithSalt` subSegmentNum
      `Prelude.hashWithSalt` deliveryRestrictions
      `Prelude.hashWithSalt` segmentsExpected
      `Prelude.hashWithSalt` segmentationEventId
      `Prelude.hashWithSalt` segmentationCancelIndicator

instance Prelude.NFData Scte35SegmentationDescriptor where
  rnf Scte35SegmentationDescriptor' {..} =
    Prelude.rnf subSegmentsExpected
      `Prelude.seq` Prelude.rnf segmentationDuration
      `Prelude.seq` Prelude.rnf segmentationTypeId
      `Prelude.seq` Prelude.rnf segmentationUpidType
      `Prelude.seq` Prelude.rnf segmentNum
      `Prelude.seq` Prelude.rnf segmentationUpid
      `Prelude.seq` Prelude.rnf subSegmentNum
      `Prelude.seq` Prelude.rnf deliveryRestrictions
      `Prelude.seq` Prelude.rnf segmentsExpected
      `Prelude.seq` Prelude.rnf segmentationEventId
      `Prelude.seq` Prelude.rnf segmentationCancelIndicator

instance Data.ToJSON Scte35SegmentationDescriptor where
  toJSON Scte35SegmentationDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("subSegmentsExpected" Data..=)
              Prelude.<$> subSegmentsExpected,
            ("segmentationDuration" Data..=)
              Prelude.<$> segmentationDuration,
            ("segmentationTypeId" Data..=)
              Prelude.<$> segmentationTypeId,
            ("segmentationUpidType" Data..=)
              Prelude.<$> segmentationUpidType,
            ("segmentNum" Data..=) Prelude.<$> segmentNum,
            ("segmentationUpid" Data..=)
              Prelude.<$> segmentationUpid,
            ("subSegmentNum" Data..=) Prelude.<$> subSegmentNum,
            ("deliveryRestrictions" Data..=)
              Prelude.<$> deliveryRestrictions,
            ("segmentsExpected" Data..=)
              Prelude.<$> segmentsExpected,
            Prelude.Just
              ("segmentationEventId" Data..= segmentationEventId),
            Prelude.Just
              ( "segmentationCancelIndicator"
                  Data..= segmentationCancelIndicator
              )
          ]
      )
