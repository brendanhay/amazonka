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
-- Module      : Amazonka.Pinpoint.Types.SegmentGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.SegmentDimensions
import Amazonka.Pinpoint.Types.SegmentReference
import Amazonka.Pinpoint.Types.SourceType
import Amazonka.Pinpoint.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | Specifies the base segments and dimensions for a segment, and the
-- relationships between these base segments and dimensions.
--
-- /See:/ 'newSegmentGroup' smart constructor.
data SegmentGroup = SegmentGroup'
  { -- | An array that defines the dimensions for the segment.
    dimensions :: Prelude.Maybe [SegmentDimensions],
    -- | The base segment to build the segment on. A base segment, also referred
    -- to as a /source segment/, defines the initial population of endpoints
    -- for a segment. When you add dimensions to a segment, Amazon Pinpoint
    -- filters the base segment by using the dimensions that you specify.
    --
    -- You can specify more than one dimensional segment or only one imported
    -- segment. If you specify an imported segment, the Amazon Pinpoint console
    -- displays a segment size estimate that indicates the size of the imported
    -- segment without any filters applied to it.
    sourceSegments :: Prelude.Maybe [SegmentReference],
    -- | Specifies how to handle multiple base segments for the segment. For
    -- example, if you specify three base segments for the segment, whether the
    -- resulting segment is based on all, any, or none of the base segments.
    sourceType :: Prelude.Maybe SourceType,
    -- | Specifies how to handle multiple dimensions for the segment. For
    -- example, if you specify three dimensions for the segment, whether the
    -- resulting segment includes endpoints that match all, any, or none of the
    -- dimensions.
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- 'type'', 'segmentGroup_type' - Specifies how to handle multiple dimensions for the segment. For
-- example, if you specify three dimensions for the segment, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- dimensions.
newSegmentGroup ::
  SegmentGroup
newSegmentGroup =
  SegmentGroup'
    { dimensions = Prelude.Nothing,
      sourceSegments = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An array that defines the dimensions for the segment.
segmentGroup_dimensions :: Lens.Lens' SegmentGroup (Prelude.Maybe [SegmentDimensions])
segmentGroup_dimensions = Lens.lens (\SegmentGroup' {dimensions} -> dimensions) (\s@SegmentGroup' {} a -> s {dimensions = a} :: SegmentGroup) Prelude.. Lens.mapping Lens.coerced

-- | The base segment to build the segment on. A base segment, also referred
-- to as a /source segment/, defines the initial population of endpoints
-- for a segment. When you add dimensions to a segment, Amazon Pinpoint
-- filters the base segment by using the dimensions that you specify.
--
-- You can specify more than one dimensional segment or only one imported
-- segment. If you specify an imported segment, the Amazon Pinpoint console
-- displays a segment size estimate that indicates the size of the imported
-- segment without any filters applied to it.
segmentGroup_sourceSegments :: Lens.Lens' SegmentGroup (Prelude.Maybe [SegmentReference])
segmentGroup_sourceSegments = Lens.lens (\SegmentGroup' {sourceSegments} -> sourceSegments) (\s@SegmentGroup' {} a -> s {sourceSegments = a} :: SegmentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies how to handle multiple base segments for the segment. For
-- example, if you specify three base segments for the segment, whether the
-- resulting segment is based on all, any, or none of the base segments.
segmentGroup_sourceType :: Lens.Lens' SegmentGroup (Prelude.Maybe SourceType)
segmentGroup_sourceType = Lens.lens (\SegmentGroup' {sourceType} -> sourceType) (\s@SegmentGroup' {} a -> s {sourceType = a} :: SegmentGroup)

-- | Specifies how to handle multiple dimensions for the segment. For
-- example, if you specify three dimensions for the segment, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- dimensions.
segmentGroup_type :: Lens.Lens' SegmentGroup (Prelude.Maybe Type)
segmentGroup_type = Lens.lens (\SegmentGroup' {type'} -> type') (\s@SegmentGroup' {} a -> s {type' = a} :: SegmentGroup)

instance Data.FromJSON SegmentGroup where
  parseJSON =
    Data.withObject
      "SegmentGroup"
      ( \x ->
          SegmentGroup'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceSegments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceType")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable SegmentGroup where
  hashWithSalt _salt SegmentGroup' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` sourceSegments
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SegmentGroup where
  rnf SegmentGroup' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf sourceSegments
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON SegmentGroup where
  toJSON SegmentGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("SourceSegments" Data..=)
              Prelude.<$> sourceSegments,
            ("SourceType" Data..=) Prelude.<$> sourceType,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
