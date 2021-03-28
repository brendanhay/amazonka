{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ConstraintDetail
  ( ConstraintDetail (..)
  -- * Smart constructor
  , mkConstraintDetail
  -- * Lenses
  , cdConstraintId
  , cdDescription
  , cdOwner
  , cdPortfolioId
  , cdProductId
  , cdType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ConstraintId as Types
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.Owner as Types
import qualified Network.AWS.ServiceCatalog.Types.PortfolioId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.Type as Types

-- | Information about a constraint.
--
-- /See:/ 'mkConstraintDetail' smart constructor.
data ConstraintDetail = ConstraintDetail'
  { constraintId :: Core.Maybe Types.ConstraintId
    -- ^ The identifier of the constraint.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the constraint.
  , owner :: Core.Maybe Types.Owner
    -- ^ The owner of the constraint.
  , portfolioId :: Core.Maybe Types.PortfolioId
    -- ^ The identifier of the portfolio the product resides in. The constraint applies only to the instance of the product that lives within this portfolio.
  , productId :: Core.Maybe Types.ProductId
    -- ^ The identifier of the product the constraint applies to. Note that a constraint applies to a specific instance of a product within a certain portfolio.
  , type' :: Core.Maybe Types.Type
    -- ^ The type of constraint.
--
--
--     * @LAUNCH@ 
--
--
--     * @NOTIFICATION@ 
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConstraintDetail' value with any optional fields omitted.
mkConstraintDetail
    :: ConstraintDetail
mkConstraintDetail
  = ConstraintDetail'{constraintId = Core.Nothing,
                      description = Core.Nothing, owner = Core.Nothing,
                      portfolioId = Core.Nothing, productId = Core.Nothing,
                      type' = Core.Nothing}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'constraintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConstraintId :: Lens.Lens' ConstraintDetail (Core.Maybe Types.ConstraintId)
cdConstraintId = Lens.field @"constraintId"
{-# INLINEABLE cdConstraintId #-}
{-# DEPRECATED constraintId "Use generic-lens or generic-optics with 'constraintId' instead"  #-}

-- | The description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' ConstraintDetail (Core.Maybe Types.Description)
cdDescription = Lens.field @"description"
{-# INLINEABLE cdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The owner of the constraint.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOwner :: Lens.Lens' ConstraintDetail (Core.Maybe Types.Owner)
cdOwner = Lens.field @"owner"
{-# INLINEABLE cdOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The identifier of the portfolio the product resides in. The constraint applies only to the instance of the product that lives within this portfolio.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPortfolioId :: Lens.Lens' ConstraintDetail (Core.Maybe Types.PortfolioId)
cdPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE cdPortfolioId #-}
{-# DEPRECATED portfolioId "Use generic-lens or generic-optics with 'portfolioId' instead"  #-}

-- | The identifier of the product the constraint applies to. Note that a constraint applies to a specific instance of a product within a certain portfolio.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdProductId :: Lens.Lens' ConstraintDetail (Core.Maybe Types.ProductId)
cdProductId = Lens.field @"productId"
{-# INLINEABLE cdProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The type of constraint.
--
--
--     * @LAUNCH@ 
--
--
--     * @NOTIFICATION@ 
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@ 
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdType :: Lens.Lens' ConstraintDetail (Core.Maybe Types.Type)
cdType = Lens.field @"type'"
{-# INLINEABLE cdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ConstraintDetail where
        parseJSON
          = Core.withObject "ConstraintDetail" Core.$
              \ x ->
                ConstraintDetail' Core.<$>
                  (x Core..:? "ConstraintId") Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Owner"
                    Core.<*> x Core..:? "PortfolioId"
                    Core.<*> x Core..:? "ProductId"
                    Core.<*> x Core..:? "Type"
