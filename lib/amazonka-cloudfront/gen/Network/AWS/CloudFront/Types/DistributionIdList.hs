{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionIdList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionIdList
  ( DistributionIdList (..),

    -- * Smart constructor
    mkDistributionIdList,

    -- * Lenses
    dilMarker,
    dilMaxItems,
    dilIsTruncated,
    dilQuantity,
    dilItems,
    dilNextMarker,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of distribution IDs.
--
-- /See:/ 'mkDistributionIdList' smart constructor.
data DistributionIdList = DistributionIdList'
  { -- | The value provided in the @Marker@ request field.
    marker :: Types.String,
    -- | The maximum number of distribution IDs requested.
    maxItems :: Core.Int,
    -- | A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
    isTruncated :: Core.Bool,
    -- | The total number of distribution IDs returned in the response.
    quantity :: Core.Int,
    -- | Contains the distribution IDs in the list.
    items :: Core.Maybe [Types.String],
    -- | Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
    nextMarker :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DistributionIdList' value with any optional fields omitted.
mkDistributionIdList ::
  -- | 'marker'
  Types.String ->
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  DistributionIdList
mkDistributionIdList marker maxItems isTruncated quantity =
  DistributionIdList'
    { marker,
      maxItems,
      isTruncated,
      quantity,
      items = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | The value provided in the @Marker@ request field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilMarker :: Lens.Lens' DistributionIdList Types.String
dilMarker = Lens.field @"marker"
{-# DEPRECATED dilMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distribution IDs requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilMaxItems :: Lens.Lens' DistributionIdList Core.Int
dilMaxItems = Lens.field @"maxItems"
{-# DEPRECATED dilMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilIsTruncated :: Lens.Lens' DistributionIdList Core.Bool
dilIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED dilIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The total number of distribution IDs returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilQuantity :: Lens.Lens' DistributionIdList Core.Int
dilQuantity = Lens.field @"quantity"
{-# DEPRECATED dilQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Contains the distribution IDs in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilItems :: Lens.Lens' DistributionIdList (Core.Maybe [Types.String])
dilItems = Lens.field @"items"
{-# DEPRECATED dilItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilNextMarker :: Lens.Lens' DistributionIdList (Core.Maybe Types.String)
dilNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED dilNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromXML DistributionIdList where
  parseXML x =
    DistributionIdList'
      Core.<$> (x Core..@ "Marker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "DistributionId")
      Core.<*> (x Core..@? "NextMarker")
