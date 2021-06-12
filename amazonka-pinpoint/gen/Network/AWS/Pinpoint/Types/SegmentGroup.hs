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
-- Module      : Network.AWS.Pinpoint.Types.SegmentGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentReference
import Network.AWS.Pinpoint.Types.SourceType
import Network.AWS.Pinpoint.Types.Type

-- | Specifies the base segments and dimensions for a segment, and the
-- relationships between these base segments and dimensions.
--
-- /See:/ 'newSegmentGroup' smart constructor.
data SegmentGroup = SegmentGroup'
  { -- | An array that defines the dimensions for the segment.
    dimensions :: Core.Maybe [SegmentDimensions],
    -- | Specifies how to handle multiple dimensions for the segment. For
    -- example, if you specify three dimensions for the segment, whether the
    -- resulting segment includes endpoints that match all, any, or none of the
    -- dimensions.
    type' :: Core.Maybe Type,
    -- | The base segment to build the segment on. A base segment, also referred
    -- to as a /source segment/, defines the initial population of endpoints
    -- for a segment. When you add dimensions to a segment, Amazon Pinpoint
    -- filters the base segment by using the dimensions that you specify.
    --
    -- You can specify more than one dimensional segment or only one imported
    -- segment. If you specify an imported segment, the Amazon Pinpoint console
    -- displays a segment size estimate that indicates the size of the imported
    -- segment without any filters applied to it.
    sourceSegments :: Core.Maybe [SegmentReference],
    -- | Specifies how to handle multiple base segments for the segment. For
    -- example, if you specify three base segments for the segment, whether the
    -- resulting segment is based on all, any, or none of the base segments.
    sourceType :: Core.Maybe SourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SegmentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'segmentGroup_dimensions' - An array that defines the dimensions for the segment.
--
-- 'type'', 'segmentGroup_type' - Specifies how to handle multiple dimensions for the segment. For
-- example, if you specify three dimensions for the segment, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- dimensions.
--
-- 'sourceSegments', 'segmentGroup_sourceSegments' - The base segment to build the segment on. A base segment, also referred
-- to as a /source segment/, defines the initial population of endpoints
-- for a segment. When you add dimensions to a segment, Amazon Pinpoint
-- filters the base segment by using the dimensions that you specify.
--
-- You can specify more than one dimensional segment or only one imported
-- segment. If you specify an imported segment, the Amazon Pinpoint console
-- displays a segment size estimate that indicates the size of the imported
-- segment without any filters applied to it.
--
-- 'sourceType', 'segmentGroup_sourceType' - Specifies how to handle multiple base segments for the segment. For
-- example, if you specify three base segments for the segment, whether the
-- resulting segment is based on all, any, or none of the base segments.
newSegmentGroup ::
  SegmentGroup
newSegmentGroup =
  SegmentGroup'
    { dimensions = Core.Nothing,
      type' = Core.Nothing,
      sourceSegments = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | An array that defines the dimensions for the segment.
segmentGroup_dimensions :: Lens.Lens' SegmentGroup (Core.Maybe [SegmentDimensions])
segmentGroup_dimensions = Lens.lens (\SegmentGroup' {dimensions} -> dimensions) (\s@SegmentGroup' {} a -> s {dimensions = a} :: SegmentGroup) Core.. Lens.mapping Lens._Coerce

-- | Specifies how to handle multiple dimensions for the segment. For
-- example, if you specify three dimensions for the segment, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- dimensions.
segmentGroup_type :: Lens.Lens' SegmentGroup (Core.Maybe Type)
segmentGroup_type = Lens.lens (\SegmentGroup' {type'} -> type') (\s@SegmentGroup' {} a -> s {type' = a} :: SegmentGroup)

-- | The base segment to build the segment on. A base segment, also referred
-- to as a /source segment/, defines the initial population of endpoints
-- for a segment. When you add dimensions to a segment, Amazon Pinpoint
-- filters the base segment by using the dimensions that you specify.
--
-- You can specify more than one dimensional segment or only one imported
-- segment. If you specify an imported segment, the Amazon Pinpoint console
-- displays a segment size estimate that indicates the size of the imported
-- segment without any filters applied to it.
segmentGroup_sourceSegments :: Lens.Lens' SegmentGroup (Core.Maybe [SegmentReference])
segmentGroup_sourceSegments = Lens.lens (\SegmentGroup' {sourceSegments} -> sourceSegments) (\s@SegmentGroup' {} a -> s {sourceSegments = a} :: SegmentGroup) Core.. Lens.mapping Lens._Coerce

-- | Specifies how to handle multiple base segments for the segment. For
-- example, if you specify three base segments for the segment, whether the
-- resulting segment is based on all, any, or none of the base segments.
segmentGroup_sourceType :: Lens.Lens' SegmentGroup (Core.Maybe SourceType)
segmentGroup_sourceType = Lens.lens (\SegmentGroup' {sourceType} -> sourceType) (\s@SegmentGroup' {} a -> s {sourceType = a} :: SegmentGroup)

instance Core.FromJSON SegmentGroup where
  parseJSON =
    Core.withObject
      "SegmentGroup"
      ( \x ->
          SegmentGroup'
            Core.<$> (x Core..:? "Dimensions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "SourceSegments" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SourceType")
      )

instance Core.Hashable SegmentGroup

instance Core.NFData SegmentGroup

instance Core.ToJSON SegmentGroup where
  toJSON SegmentGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Dimensions" Core..=) Core.<$> dimensions,
            ("Type" Core..=) Core.<$> type',
            ("SourceSegments" Core..=) Core.<$> sourceSegments,
            ("SourceType" Core..=) Core.<$> sourceType
          ]
      )
