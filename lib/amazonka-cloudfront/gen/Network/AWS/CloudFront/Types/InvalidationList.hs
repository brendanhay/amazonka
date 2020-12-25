{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationList
  ( InvalidationList (..),

    -- * Smart constructor
    mkInvalidationList,

    -- * Lenses
    ilMarker,
    ilMaxItems,
    ilIsTruncated,
    ilQuantity,
    ilItems,
    ilNextMarker,
  )
where

import qualified Network.AWS.CloudFront.Types.InvalidationSummary as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @InvalidationList@ complex type describes the list of invalidation objects. For more information about invalidation, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html Invalidating Objects (Web Distributions Only)> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkInvalidationList' smart constructor.
data InvalidationList = InvalidationList'
  { -- | The value that you provided for the @Marker@ request parameter.
    marker :: Types.String,
    -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Core.Int,
    -- | A flag that indicates whether more invalidation batch requests remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more invalidation batches in the list.
    isTruncated :: Core.Bool,
    -- | The number of invalidation batches that were created by the current AWS account.
    quantity :: Core.Int,
    -- | A complex type that contains one @InvalidationSummary@ element for each invalidation batch created by the current AWS account.
    items :: Core.Maybe [Types.InvalidationSummary],
    -- | If @IsTruncated@ is @true@ , this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your invalidation batches where they left off.
    nextMarker :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InvalidationList' value with any optional fields omitted.
mkInvalidationList ::
  -- | 'marker'
  Types.String ->
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  InvalidationList
mkInvalidationList marker maxItems isTruncated quantity =
  InvalidationList'
    { marker,
      maxItems,
      isTruncated,
      quantity,
      items = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | The value that you provided for the @Marker@ request parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilMarker :: Lens.Lens' InvalidationList Types.String
ilMarker = Lens.field @"marker"
{-# DEPRECATED ilMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value that you provided for the @MaxItems@ request parameter.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilMaxItems :: Lens.Lens' InvalidationList Core.Int
ilMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ilMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A flag that indicates whether more invalidation batch requests remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more invalidation batches in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilIsTruncated :: Lens.Lens' InvalidationList Core.Bool
ilIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED ilIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The number of invalidation batches that were created by the current AWS account.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilQuantity :: Lens.Lens' InvalidationList Core.Int
ilQuantity = Lens.field @"quantity"
{-# DEPRECATED ilQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains one @InvalidationSummary@ element for each invalidation batch created by the current AWS account.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilItems :: Lens.Lens' InvalidationList (Core.Maybe [Types.InvalidationSummary])
ilItems = Lens.field @"items"
{-# DEPRECATED ilItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If @IsTruncated@ is @true@ , this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your invalidation batches where they left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilNextMarker :: Lens.Lens' InvalidationList (Core.Maybe Types.String)
ilNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED ilNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromXML InvalidationList where
  parseXML x =
    InvalidationList'
      Core.<$> (x Core..@ "Marker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "InvalidationSummary"
               )
      Core.<*> (x Core..@? "NextMarker")
