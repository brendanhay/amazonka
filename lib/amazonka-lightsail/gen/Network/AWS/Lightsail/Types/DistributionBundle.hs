{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.DistributionBundle
  ( DistributionBundle (..)
  -- * Smart constructor
  , mkDistributionBundle
  -- * Lenses
  , dbBundleId
  , dbIsActive
  , dbName
  , dbPrice
  , dbTransferPerMonthInGb
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the specifications of a distribution bundle.
--
-- /See:/ 'mkDistributionBundle' smart constructor.
data DistributionBundle = DistributionBundle'
  { bundleId :: Core.Maybe Core.Text
    -- ^ The ID of the bundle.
  , isActive :: Core.Maybe Core.Bool
    -- ^ Indicates whether the bundle is active, and can be specified for a new distribution.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the distribution bundle.
  , price :: Core.Maybe Core.Double
    -- ^ The monthly price, in US dollars, of the bundle.
  , transferPerMonthInGb :: Core.Maybe Core.Int
    -- ^ The monthly network transfer quota of the bundle.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DistributionBundle' value with any optional fields omitted.
mkDistributionBundle
    :: DistributionBundle
mkDistributionBundle
  = DistributionBundle'{bundleId = Core.Nothing,
                        isActive = Core.Nothing, name = Core.Nothing, price = Core.Nothing,
                        transferPerMonthInGb = Core.Nothing}

-- | The ID of the bundle.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBundleId :: Lens.Lens' DistributionBundle (Core.Maybe Core.Text)
dbBundleId = Lens.field @"bundleId"
{-# INLINEABLE dbBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | Indicates whether the bundle is active, and can be specified for a new distribution.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbIsActive :: Lens.Lens' DistributionBundle (Core.Maybe Core.Bool)
dbIsActive = Lens.field @"isActive"
{-# INLINEABLE dbIsActive #-}
{-# DEPRECATED isActive "Use generic-lens or generic-optics with 'isActive' instead"  #-}

-- | The name of the distribution bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbName :: Lens.Lens' DistributionBundle (Core.Maybe Core.Text)
dbName = Lens.field @"name"
{-# INLINEABLE dbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The monthly price, in US dollars, of the bundle.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbPrice :: Lens.Lens' DistributionBundle (Core.Maybe Core.Double)
dbPrice = Lens.field @"price"
{-# INLINEABLE dbPrice #-}
{-# DEPRECATED price "Use generic-lens or generic-optics with 'price' instead"  #-}

-- | The monthly network transfer quota of the bundle.
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbTransferPerMonthInGb :: Lens.Lens' DistributionBundle (Core.Maybe Core.Int)
dbTransferPerMonthInGb = Lens.field @"transferPerMonthInGb"
{-# INLINEABLE dbTransferPerMonthInGb #-}
{-# DEPRECATED transferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead"  #-}

instance Core.FromJSON DistributionBundle where
        parseJSON
          = Core.withObject "DistributionBundle" Core.$
              \ x ->
                DistributionBundle' Core.<$>
                  (x Core..:? "bundleId") Core.<*> x Core..:? "isActive" Core.<*>
                    x Core..:? "name"
                    Core.<*> x Core..:? "price"
                    Core.<*> x Core..:? "transferPerMonthInGb"
