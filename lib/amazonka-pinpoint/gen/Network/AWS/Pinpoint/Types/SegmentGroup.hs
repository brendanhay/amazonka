{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentGroup
  ( SegmentGroup (..),

    -- * Smart constructor
    mkSegmentGroup,

    -- * Lenses
    sgDimensions,
    sgSourceSegments,
    sgSourceType,
    sgType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SegmentDimensions as Types
import qualified Network.AWS.Pinpoint.Types.SegmentReference as Types
import qualified Network.AWS.Pinpoint.Types.SourceType as Types
import qualified Network.AWS.Pinpoint.Types.Type as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the base segments and dimensions for a segment, and the relationships between these base segments and dimensions.
--
-- /See:/ 'mkSegmentGroup' smart constructor.
data SegmentGroup = SegmentGroup'
  { -- | An array that defines the dimensions for the segment.
    dimensions :: Core.Maybe [Types.SegmentDimensions],
    -- | The base segment to build the segment on. A base segment, also referred to as a /source segment/ , defines the initial population of endpoints for a segment. When you add dimensions to a segment, Amazon Pinpoint filters the base segment by using the dimensions that you specify.
    --
    -- You can specify more than one dimensional segment or only one imported segment. If you specify an imported segment, the Amazon Pinpoint console displays a segment size estimate that indicates the size of the imported segment without any filters applied to it.
    sourceSegments :: Core.Maybe [Types.SegmentReference],
    -- | Specifies how to handle multiple base segments for the segment. For example, if you specify three base segments for the segment, whether the resulting segment is based on all, any, or none of the base segments.
    sourceType :: Core.Maybe Types.SourceType,
    -- | Specifies how to handle multiple dimensions for the segment. For example, if you specify three dimensions for the segment, whether the resulting segment includes endpoints that match all, any, or none of the dimensions.
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentGroup' value with any optional fields omitted.
mkSegmentGroup ::
  SegmentGroup
mkSegmentGroup =
  SegmentGroup'
    { dimensions = Core.Nothing,
      sourceSegments = Core.Nothing,
      sourceType = Core.Nothing,
      type' = Core.Nothing
    }

-- | An array that defines the dimensions for the segment.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDimensions :: Lens.Lens' SegmentGroup (Core.Maybe [Types.SegmentDimensions])
sgDimensions = Lens.field @"dimensions"
{-# DEPRECATED sgDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The base segment to build the segment on. A base segment, also referred to as a /source segment/ , defines the initial population of endpoints for a segment. When you add dimensions to a segment, Amazon Pinpoint filters the base segment by using the dimensions that you specify.
--
-- You can specify more than one dimensional segment or only one imported segment. If you specify an imported segment, the Amazon Pinpoint console displays a segment size estimate that indicates the size of the imported segment without any filters applied to it.
--
-- /Note:/ Consider using 'sourceSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSourceSegments :: Lens.Lens' SegmentGroup (Core.Maybe [Types.SegmentReference])
sgSourceSegments = Lens.field @"sourceSegments"
{-# DEPRECATED sgSourceSegments "Use generic-lens or generic-optics with 'sourceSegments' instead." #-}

-- | Specifies how to handle multiple base segments for the segment. For example, if you specify three base segments for the segment, whether the resulting segment is based on all, any, or none of the base segments.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSourceType :: Lens.Lens' SegmentGroup (Core.Maybe Types.SourceType)
sgSourceType = Lens.field @"sourceType"
{-# DEPRECATED sgSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Specifies how to handle multiple dimensions for the segment. For example, if you specify three dimensions for the segment, whether the resulting segment includes endpoints that match all, any, or none of the dimensions.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgType :: Lens.Lens' SegmentGroup (Core.Maybe Types.Type)
sgType = Lens.field @"type'"
{-# DEPRECATED sgType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON SegmentGroup where
  toJSON SegmentGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("Dimensions" Core..=) Core.<$> dimensions,
            ("SourceSegments" Core..=) Core.<$> sourceSegments,
            ("SourceType" Core..=) Core.<$> sourceType,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON SegmentGroup where
  parseJSON =
    Core.withObject "SegmentGroup" Core.$
      \x ->
        SegmentGroup'
          Core.<$> (x Core..:? "Dimensions")
          Core.<*> (x Core..:? "SourceSegments")
          Core.<*> (x Core..:? "SourceType")
          Core.<*> (x Core..:? "Type")
