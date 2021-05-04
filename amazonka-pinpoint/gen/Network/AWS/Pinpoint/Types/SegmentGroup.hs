{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentReference
import Network.AWS.Pinpoint.Types.SourceType
import Network.AWS.Pinpoint.Types.Type
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the base segments and dimensions for a segment, and the
-- relationships between these base segments and dimensions.
--
-- /See:/ 'newSegmentGroup' smart constructor.
data SegmentGroup = SegmentGroup'
  { -- | An array that defines the dimensions for the segment.
    dimensions :: Prelude.Maybe [SegmentDimensions],
    -- | Specifies how to handle multiple dimensions for the segment. For
    -- example, if you specify three dimensions for the segment, whether the
    -- resulting segment includes endpoints that match all, any, or none of the
    -- dimensions.
    type' :: Prelude.Maybe Type,
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
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { dimensions = Prelude.Nothing,
      type' = Prelude.Nothing,
      sourceSegments = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | An array that defines the dimensions for the segment.
segmentGroup_dimensions :: Lens.Lens' SegmentGroup (Prelude.Maybe [SegmentDimensions])
segmentGroup_dimensions = Lens.lens (\SegmentGroup' {dimensions} -> dimensions) (\s@SegmentGroup' {} a -> s {dimensions = a} :: SegmentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies how to handle multiple dimensions for the segment. For
-- example, if you specify three dimensions for the segment, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- dimensions.
segmentGroup_type :: Lens.Lens' SegmentGroup (Prelude.Maybe Type)
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
segmentGroup_sourceSegments :: Lens.Lens' SegmentGroup (Prelude.Maybe [SegmentReference])
segmentGroup_sourceSegments = Lens.lens (\SegmentGroup' {sourceSegments} -> sourceSegments) (\s@SegmentGroup' {} a -> s {sourceSegments = a} :: SegmentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies how to handle multiple base segments for the segment. For
-- example, if you specify three base segments for the segment, whether the
-- resulting segment is based on all, any, or none of the base segments.
segmentGroup_sourceType :: Lens.Lens' SegmentGroup (Prelude.Maybe SourceType)
segmentGroup_sourceType = Lens.lens (\SegmentGroup' {sourceType} -> sourceType) (\s@SegmentGroup' {} a -> s {sourceType = a} :: SegmentGroup)

instance Prelude.FromJSON SegmentGroup where
  parseJSON =
    Prelude.withObject
      "SegmentGroup"
      ( \x ->
          SegmentGroup'
            Prelude.<$> ( x Prelude..:? "Dimensions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> ( x Prelude..:? "SourceSegments"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "SourceType")
      )

instance Prelude.Hashable SegmentGroup

instance Prelude.NFData SegmentGroup

instance Prelude.ToJSON SegmentGroup where
  toJSON SegmentGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Dimensions" Prelude..=) Prelude.<$> dimensions,
            ("Type" Prelude..=) Prelude.<$> type',
            ("SourceSegments" Prelude..=)
              Prelude.<$> sourceSegments,
            ("SourceType" Prelude..=) Prelude.<$> sourceType
          ]
      )
