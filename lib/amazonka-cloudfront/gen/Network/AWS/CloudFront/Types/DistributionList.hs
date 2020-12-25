{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionList
  ( DistributionList (..),

    -- * Smart constructor
    mkDistributionList,

    -- * Lenses
    dlMarker,
    dlMaxItems,
    dlIsTruncated,
    dlQuantity,
    dlItems,
    dlNextMarker,
  )
where

import qualified Network.AWS.CloudFront.Types.DistributionSummary as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A distribution list.
--
-- /See:/ 'mkDistributionList' smart constructor.
data DistributionList = DistributionList'
  { -- | The value you provided for the @Marker@ request parameter.
    marker :: Types.String,
    -- | The value you provided for the @MaxItems@ request parameter.
    maxItems :: Core.Int,
    -- | A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
    isTruncated :: Core.Bool,
    -- | The number of distributions that were created by the current AWS account.
    quantity :: Core.Int,
    -- | A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
    items :: Core.Maybe [Types.DistributionSummary],
    -- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
    nextMarker :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DistributionList' value with any optional fields omitted.
mkDistributionList ::
  -- | 'marker'
  Types.String ->
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  DistributionList
mkDistributionList marker maxItems isTruncated quantity =
  DistributionList'
    { marker,
      maxItems,
      isTruncated,
      quantity,
      items = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | The value you provided for the @Marker@ request parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMarker :: Lens.Lens' DistributionList Types.String
dlMarker = Lens.field @"marker"
{-# DEPRECATED dlMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value you provided for the @MaxItems@ request parameter.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMaxItems :: Lens.Lens' DistributionList Core.Int
dlMaxItems = Lens.field @"maxItems"
{-# DEPRECATED dlMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlIsTruncated :: Lens.Lens' DistributionList Core.Bool
dlIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED dlIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The number of distributions that were created by the current AWS account.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlQuantity :: Lens.Lens' DistributionList Core.Int
dlQuantity = Lens.field @"quantity"
{-# DEPRECATED dlQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlItems :: Lens.Lens' DistributionList (Core.Maybe [Types.DistributionSummary])
dlItems = Lens.field @"items"
{-# DEPRECATED dlItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlNextMarker :: Lens.Lens' DistributionList (Core.Maybe Types.String)
dlNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED dlNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromXML DistributionList where
  parseXML x =
    DistributionList'
      Core.<$> (x Core..@ "Marker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "DistributionSummary"
               )
      Core.<*> (x Core..@? "NextMarker")
