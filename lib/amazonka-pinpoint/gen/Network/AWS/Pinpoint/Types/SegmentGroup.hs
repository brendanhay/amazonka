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
    sgSourceSegments,
    sgSourceType,
    sgType,
    sgDimensions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentReference
import Network.AWS.Pinpoint.Types.SourceType
import Network.AWS.Pinpoint.Types.Type
import qualified Network.AWS.Prelude as Lude

-- | Specifies the base segments and dimensions for a segment, and the relationships between these base segments and dimensions.
--
-- /See:/ 'mkSegmentGroup' smart constructor.
data SegmentGroup = SegmentGroup'
  { -- | The base segment to build the segment on. A base segment, also referred to as a /source segment/ , defines the initial population of endpoints for a segment. When you add dimensions to a segment, Amazon Pinpoint filters the base segment by using the dimensions that you specify.
    --
    -- You can specify more than one dimensional segment or only one imported segment. If you specify an imported segment, the Amazon Pinpoint console displays a segment size estimate that indicates the size of the imported segment without any filters applied to it.
    sourceSegments :: Lude.Maybe [SegmentReference],
    -- | Specifies how to handle multiple base segments for the segment. For example, if you specify three base segments for the segment, whether the resulting segment is based on all, any, or none of the base segments.
    sourceType :: Lude.Maybe SourceType,
    -- | Specifies how to handle multiple dimensions for the segment. For example, if you specify three dimensions for the segment, whether the resulting segment includes endpoints that match all, any, or none of the dimensions.
    type' :: Lude.Maybe Type,
    -- | An array that defines the dimensions for the segment.
    dimensions :: Lude.Maybe [SegmentDimensions]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentGroup' with the minimum fields required to make a request.
--
-- * 'sourceSegments' - The base segment to build the segment on. A base segment, also referred to as a /source segment/ , defines the initial population of endpoints for a segment. When you add dimensions to a segment, Amazon Pinpoint filters the base segment by using the dimensions that you specify.
--
-- You can specify more than one dimensional segment or only one imported segment. If you specify an imported segment, the Amazon Pinpoint console displays a segment size estimate that indicates the size of the imported segment without any filters applied to it.
-- * 'sourceType' - Specifies how to handle multiple base segments for the segment. For example, if you specify three base segments for the segment, whether the resulting segment is based on all, any, or none of the base segments.
-- * 'type'' - Specifies how to handle multiple dimensions for the segment. For example, if you specify three dimensions for the segment, whether the resulting segment includes endpoints that match all, any, or none of the dimensions.
-- * 'dimensions' - An array that defines the dimensions for the segment.
mkSegmentGroup ::
  SegmentGroup
mkSegmentGroup =
  SegmentGroup'
    { sourceSegments = Lude.Nothing,
      sourceType = Lude.Nothing,
      type' = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The base segment to build the segment on. A base segment, also referred to as a /source segment/ , defines the initial population of endpoints for a segment. When you add dimensions to a segment, Amazon Pinpoint filters the base segment by using the dimensions that you specify.
--
-- You can specify more than one dimensional segment or only one imported segment. If you specify an imported segment, the Amazon Pinpoint console displays a segment size estimate that indicates the size of the imported segment without any filters applied to it.
--
-- /Note:/ Consider using 'sourceSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSourceSegments :: Lens.Lens' SegmentGroup (Lude.Maybe [SegmentReference])
sgSourceSegments = Lens.lens (sourceSegments :: SegmentGroup -> Lude.Maybe [SegmentReference]) (\s a -> s {sourceSegments = a} :: SegmentGroup)
{-# DEPRECATED sgSourceSegments "Use generic-lens or generic-optics with 'sourceSegments' instead." #-}

-- | Specifies how to handle multiple base segments for the segment. For example, if you specify three base segments for the segment, whether the resulting segment is based on all, any, or none of the base segments.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSourceType :: Lens.Lens' SegmentGroup (Lude.Maybe SourceType)
sgSourceType = Lens.lens (sourceType :: SegmentGroup -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: SegmentGroup)
{-# DEPRECATED sgSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Specifies how to handle multiple dimensions for the segment. For example, if you specify three dimensions for the segment, whether the resulting segment includes endpoints that match all, any, or none of the dimensions.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgType :: Lens.Lens' SegmentGroup (Lude.Maybe Type)
sgType = Lens.lens (type' :: SegmentGroup -> Lude.Maybe Type) (\s a -> s {type' = a} :: SegmentGroup)
{-# DEPRECATED sgType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An array that defines the dimensions for the segment.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDimensions :: Lens.Lens' SegmentGroup (Lude.Maybe [SegmentDimensions])
sgDimensions = Lens.lens (dimensions :: SegmentGroup -> Lude.Maybe [SegmentDimensions]) (\s a -> s {dimensions = a} :: SegmentGroup)
{-# DEPRECATED sgDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromJSON SegmentGroup where
  parseJSON =
    Lude.withObject
      "SegmentGroup"
      ( \x ->
          SegmentGroup'
            Lude.<$> (x Lude..:? "SourceSegments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Dimensions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SegmentGroup where
  toJSON SegmentGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceSegments" Lude..=) Lude.<$> sourceSegments,
            ("SourceType" Lude..=) Lude.<$> sourceType,
            ("Type" Lude..=) Lude.<$> type',
            ("Dimensions" Lude..=) Lude.<$> dimensions
          ]
      )
