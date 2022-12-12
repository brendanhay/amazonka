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
-- Module      : Amazonka.MediaTailor.Types.SegmentationDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SegmentationDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @segmentation_descriptor@ message can contain advanced metadata
-- fields, like content identifiers, to convey a wide range of information
-- about the ad break. MediaTailor writes the ad metadata in the egress
-- manifest as part of the @EXT-X-DATERANGE@ or @EventStream@ ad marker\'s
-- SCTE-35 data.
--
-- @segmentation_descriptor@ messages must be sent with the @time_signal@
-- message type.
--
-- See the @segmentation_descriptor()@ table of the 2022 SCTE-35
-- specification for more information.
--
-- /See:/ 'newSegmentationDescriptor' smart constructor.
data SegmentationDescriptor = SegmentationDescriptor'
  { -- | The segment number to assign to the
    -- @segmentation_descriptor.segment_num@ message, as defined in section
    -- 10.3.3.1 of the 2022 SCTE-35 specification Values must be between 0 and
    -- 256, inclusive. The default value is 0.
    segmentNum :: Prelude.Maybe Prelude.Int,
    -- | The Event Identifier to assign to the
    -- @segmentation_descriptor.segmentation_event_id@ message, as defined in
    -- section 10.3.3.1 of the 2022 SCTE-35 specification. The default value is
    -- 1.
    segmentationEventId :: Prelude.Maybe Prelude.Int,
    -- | The Type Identifier to assign to the
    -- @segmentation_descriptor.segmentation_type_id@ message, as defined in
    -- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
    -- between 0 and 256, inclusive. The default value is 48.
    segmentationTypeId :: Prelude.Maybe Prelude.Int,
    -- | The Upid to assign to the @segmentation_descriptor.segmentation_upid@
    -- message, as defined in section 10.3.3.1 of the 2022 SCTE-35
    -- specification. The value must be a hexadecimal string containing only
    -- the characters 0 though 9 and A through F. The default value is \"\" (an
    -- empty string).
    segmentationUpid :: Prelude.Maybe Prelude.Text,
    -- | The Upid Type to assign to the
    -- @segmentation_descriptor.segmentation_upid_type@ message, as defined in
    -- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
    -- between 0 and 256, inclusive. The default value is 14.
    segmentationUpidType :: Prelude.Maybe Prelude.Int,
    -- | The number of segments expected, which is assigned to the
    -- @segmentation_descriptor.segments_expectedS@ message, as defined in
    -- section 10.3.3.1 of the 2022 SCTE-35 specification Values must be
    -- between 0 and 256, inclusive. The default value is 0.
    segmentsExpected :: Prelude.Maybe Prelude.Int,
    -- | The sub-segment number to assign to the
    -- @segmentation_descriptor.sub_segment_num@ message, as defined in section
    -- 10.3.3.1 of the 2022 SCTE-35 specification. Values must be between 0 and
    -- 256, inclusive. The defualt value is null.
    subSegmentNum :: Prelude.Maybe Prelude.Int,
    -- | The number of sub-segments expected, which is assigned to the
    -- @segmentation_descriptor.sub_segments_expected@ message, as defined in
    -- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
    -- between 0 and 256, inclusive. The default value is null.
    subSegmentsExpected :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentationDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentNum', 'segmentationDescriptor_segmentNum' - The segment number to assign to the
-- @segmentation_descriptor.segment_num@ message, as defined in section
-- 10.3.3.1 of the 2022 SCTE-35 specification Values must be between 0 and
-- 256, inclusive. The default value is 0.
--
-- 'segmentationEventId', 'segmentationDescriptor_segmentationEventId' - The Event Identifier to assign to the
-- @segmentation_descriptor.segmentation_event_id@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. The default value is
-- 1.
--
-- 'segmentationTypeId', 'segmentationDescriptor_segmentationTypeId' - The Type Identifier to assign to the
-- @segmentation_descriptor.segmentation_type_id@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
-- between 0 and 256, inclusive. The default value is 48.
--
-- 'segmentationUpid', 'segmentationDescriptor_segmentationUpid' - The Upid to assign to the @segmentation_descriptor.segmentation_upid@
-- message, as defined in section 10.3.3.1 of the 2022 SCTE-35
-- specification. The value must be a hexadecimal string containing only
-- the characters 0 though 9 and A through F. The default value is \"\" (an
-- empty string).
--
-- 'segmentationUpidType', 'segmentationDescriptor_segmentationUpidType' - The Upid Type to assign to the
-- @segmentation_descriptor.segmentation_upid_type@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
-- between 0 and 256, inclusive. The default value is 14.
--
-- 'segmentsExpected', 'segmentationDescriptor_segmentsExpected' - The number of segments expected, which is assigned to the
-- @segmentation_descriptor.segments_expectedS@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification Values must be
-- between 0 and 256, inclusive. The default value is 0.
--
-- 'subSegmentNum', 'segmentationDescriptor_subSegmentNum' - The sub-segment number to assign to the
-- @segmentation_descriptor.sub_segment_num@ message, as defined in section
-- 10.3.3.1 of the 2022 SCTE-35 specification. Values must be between 0 and
-- 256, inclusive. The defualt value is null.
--
-- 'subSegmentsExpected', 'segmentationDescriptor_subSegmentsExpected' - The number of sub-segments expected, which is assigned to the
-- @segmentation_descriptor.sub_segments_expected@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
-- between 0 and 256, inclusive. The default value is null.
newSegmentationDescriptor ::
  SegmentationDescriptor
newSegmentationDescriptor =
  SegmentationDescriptor'
    { segmentNum =
        Prelude.Nothing,
      segmentationEventId = Prelude.Nothing,
      segmentationTypeId = Prelude.Nothing,
      segmentationUpid = Prelude.Nothing,
      segmentationUpidType = Prelude.Nothing,
      segmentsExpected = Prelude.Nothing,
      subSegmentNum = Prelude.Nothing,
      subSegmentsExpected = Prelude.Nothing
    }

-- | The segment number to assign to the
-- @segmentation_descriptor.segment_num@ message, as defined in section
-- 10.3.3.1 of the 2022 SCTE-35 specification Values must be between 0 and
-- 256, inclusive. The default value is 0.
segmentationDescriptor_segmentNum :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_segmentNum = Lens.lens (\SegmentationDescriptor' {segmentNum} -> segmentNum) (\s@SegmentationDescriptor' {} a -> s {segmentNum = a} :: SegmentationDescriptor)

-- | The Event Identifier to assign to the
-- @segmentation_descriptor.segmentation_event_id@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. The default value is
-- 1.
segmentationDescriptor_segmentationEventId :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_segmentationEventId = Lens.lens (\SegmentationDescriptor' {segmentationEventId} -> segmentationEventId) (\s@SegmentationDescriptor' {} a -> s {segmentationEventId = a} :: SegmentationDescriptor)

-- | The Type Identifier to assign to the
-- @segmentation_descriptor.segmentation_type_id@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
-- between 0 and 256, inclusive. The default value is 48.
segmentationDescriptor_segmentationTypeId :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_segmentationTypeId = Lens.lens (\SegmentationDescriptor' {segmentationTypeId} -> segmentationTypeId) (\s@SegmentationDescriptor' {} a -> s {segmentationTypeId = a} :: SegmentationDescriptor)

-- | The Upid to assign to the @segmentation_descriptor.segmentation_upid@
-- message, as defined in section 10.3.3.1 of the 2022 SCTE-35
-- specification. The value must be a hexadecimal string containing only
-- the characters 0 though 9 and A through F. The default value is \"\" (an
-- empty string).
segmentationDescriptor_segmentationUpid :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Text)
segmentationDescriptor_segmentationUpid = Lens.lens (\SegmentationDescriptor' {segmentationUpid} -> segmentationUpid) (\s@SegmentationDescriptor' {} a -> s {segmentationUpid = a} :: SegmentationDescriptor)

-- | The Upid Type to assign to the
-- @segmentation_descriptor.segmentation_upid_type@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
-- between 0 and 256, inclusive. The default value is 14.
segmentationDescriptor_segmentationUpidType :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_segmentationUpidType = Lens.lens (\SegmentationDescriptor' {segmentationUpidType} -> segmentationUpidType) (\s@SegmentationDescriptor' {} a -> s {segmentationUpidType = a} :: SegmentationDescriptor)

-- | The number of segments expected, which is assigned to the
-- @segmentation_descriptor.segments_expectedS@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification Values must be
-- between 0 and 256, inclusive. The default value is 0.
segmentationDescriptor_segmentsExpected :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_segmentsExpected = Lens.lens (\SegmentationDescriptor' {segmentsExpected} -> segmentsExpected) (\s@SegmentationDescriptor' {} a -> s {segmentsExpected = a} :: SegmentationDescriptor)

-- | The sub-segment number to assign to the
-- @segmentation_descriptor.sub_segment_num@ message, as defined in section
-- 10.3.3.1 of the 2022 SCTE-35 specification. Values must be between 0 and
-- 256, inclusive. The defualt value is null.
segmentationDescriptor_subSegmentNum :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_subSegmentNum = Lens.lens (\SegmentationDescriptor' {subSegmentNum} -> subSegmentNum) (\s@SegmentationDescriptor' {} a -> s {subSegmentNum = a} :: SegmentationDescriptor)

-- | The number of sub-segments expected, which is assigned to the
-- @segmentation_descriptor.sub_segments_expected@ message, as defined in
-- section 10.3.3.1 of the 2022 SCTE-35 specification. Values must be
-- between 0 and 256, inclusive. The default value is null.
segmentationDescriptor_subSegmentsExpected :: Lens.Lens' SegmentationDescriptor (Prelude.Maybe Prelude.Int)
segmentationDescriptor_subSegmentsExpected = Lens.lens (\SegmentationDescriptor' {subSegmentsExpected} -> subSegmentsExpected) (\s@SegmentationDescriptor' {} a -> s {subSegmentsExpected = a} :: SegmentationDescriptor)

instance Data.FromJSON SegmentationDescriptor where
  parseJSON =
    Data.withObject
      "SegmentationDescriptor"
      ( \x ->
          SegmentationDescriptor'
            Prelude.<$> (x Data..:? "SegmentNum")
            Prelude.<*> (x Data..:? "SegmentationEventId")
            Prelude.<*> (x Data..:? "SegmentationTypeId")
            Prelude.<*> (x Data..:? "SegmentationUpid")
            Prelude.<*> (x Data..:? "SegmentationUpidType")
            Prelude.<*> (x Data..:? "SegmentsExpected")
            Prelude.<*> (x Data..:? "SubSegmentNum")
            Prelude.<*> (x Data..:? "SubSegmentsExpected")
      )

instance Prelude.Hashable SegmentationDescriptor where
  hashWithSalt _salt SegmentationDescriptor' {..} =
    _salt `Prelude.hashWithSalt` segmentNum
      `Prelude.hashWithSalt` segmentationEventId
      `Prelude.hashWithSalt` segmentationTypeId
      `Prelude.hashWithSalt` segmentationUpid
      `Prelude.hashWithSalt` segmentationUpidType
      `Prelude.hashWithSalt` segmentsExpected
      `Prelude.hashWithSalt` subSegmentNum
      `Prelude.hashWithSalt` subSegmentsExpected

instance Prelude.NFData SegmentationDescriptor where
  rnf SegmentationDescriptor' {..} =
    Prelude.rnf segmentNum
      `Prelude.seq` Prelude.rnf segmentationEventId
      `Prelude.seq` Prelude.rnf segmentationTypeId
      `Prelude.seq` Prelude.rnf segmentationUpid
      `Prelude.seq` Prelude.rnf segmentationUpidType
      `Prelude.seq` Prelude.rnf segmentsExpected
      `Prelude.seq` Prelude.rnf subSegmentNum
      `Prelude.seq` Prelude.rnf subSegmentsExpected

instance Data.ToJSON SegmentationDescriptor where
  toJSON SegmentationDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SegmentNum" Data..=) Prelude.<$> segmentNum,
            ("SegmentationEventId" Data..=)
              Prelude.<$> segmentationEventId,
            ("SegmentationTypeId" Data..=)
              Prelude.<$> segmentationTypeId,
            ("SegmentationUpid" Data..=)
              Prelude.<$> segmentationUpid,
            ("SegmentationUpidType" Data..=)
              Prelude.<$> segmentationUpidType,
            ("SegmentsExpected" Data..=)
              Prelude.<$> segmentsExpected,
            ("SubSegmentNum" Data..=) Prelude.<$> subSegmentNum,
            ("SubSegmentsExpected" Data..=)
              Prelude.<$> subSegmentsExpected
          ]
      )
