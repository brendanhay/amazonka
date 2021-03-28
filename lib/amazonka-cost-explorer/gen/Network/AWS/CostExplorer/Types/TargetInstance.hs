{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TargetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.TargetInstance
  ( TargetInstance (..)
  -- * Smart constructor
  , mkTargetInstance
  -- * Lenses
  , tiCurrencyCode
  , tiDefaultTargetInstance
  , tiEstimatedMonthlyCost
  , tiEstimatedMonthlySavings
  , tiExpectedResourceUtilization
  , tiResourceDetails
  ) where

import qualified Network.AWS.CostExplorer.Types.CurrencyCode as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedMonthlyCost as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedMonthlySavings as Types
import qualified Network.AWS.CostExplorer.Types.ResourceDetails as Types
import qualified Network.AWS.CostExplorer.Types.ResourceUtilization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on recommended instance.
--
-- /See:/ 'mkTargetInstance' smart constructor.
data TargetInstance = TargetInstance'
  { currencyCode :: Core.Maybe Types.CurrencyCode
    -- ^ The currency code that AWS used to calculate the costs for this instance.
  , defaultTargetInstance :: Core.Maybe Core.Bool
    -- ^ Indicates whether this recommendation is the defaulted AWS recommendation.
  , estimatedMonthlyCost :: Core.Maybe Types.EstimatedMonthlyCost
    -- ^ Expected cost to operate this instance type on a monthly basis.
  , estimatedMonthlySavings :: Core.Maybe Types.EstimatedMonthlySavings
    -- ^ Estimated savings resulting from modification, on a monthly basis.
  , expectedResourceUtilization :: Core.Maybe Types.ResourceUtilization
    -- ^ Expected utilization metrics for target instance type.
  , resourceDetails :: Core.Maybe Types.ResourceDetails
    -- ^ Details on the target instance type. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetInstance' value with any optional fields omitted.
mkTargetInstance
    :: TargetInstance
mkTargetInstance
  = TargetInstance'{currencyCode = Core.Nothing,
                    defaultTargetInstance = Core.Nothing,
                    estimatedMonthlyCost = Core.Nothing,
                    estimatedMonthlySavings = Core.Nothing,
                    expectedResourceUtilization = Core.Nothing,
                    resourceDetails = Core.Nothing}

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiCurrencyCode :: Lens.Lens' TargetInstance (Core.Maybe Types.CurrencyCode)
tiCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE tiCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | Indicates whether this recommendation is the defaulted AWS recommendation.
--
-- /Note:/ Consider using 'defaultTargetInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDefaultTargetInstance :: Lens.Lens' TargetInstance (Core.Maybe Core.Bool)
tiDefaultTargetInstance = Lens.field @"defaultTargetInstance"
{-# INLINEABLE tiDefaultTargetInstance #-}
{-# DEPRECATED defaultTargetInstance "Use generic-lens or generic-optics with 'defaultTargetInstance' instead"  #-}

-- | Expected cost to operate this instance type on a monthly basis.
--
-- /Note:/ Consider using 'estimatedMonthlyCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiEstimatedMonthlyCost :: Lens.Lens' TargetInstance (Core.Maybe Types.EstimatedMonthlyCost)
tiEstimatedMonthlyCost = Lens.field @"estimatedMonthlyCost"
{-# INLINEABLE tiEstimatedMonthlyCost #-}
{-# DEPRECATED estimatedMonthlyCost "Use generic-lens or generic-optics with 'estimatedMonthlyCost' instead"  #-}

-- | Estimated savings resulting from modification, on a monthly basis.
--
-- /Note:/ Consider using 'estimatedMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiEstimatedMonthlySavings :: Lens.Lens' TargetInstance (Core.Maybe Types.EstimatedMonthlySavings)
tiEstimatedMonthlySavings = Lens.field @"estimatedMonthlySavings"
{-# INLINEABLE tiEstimatedMonthlySavings #-}
{-# DEPRECATED estimatedMonthlySavings "Use generic-lens or generic-optics with 'estimatedMonthlySavings' instead"  #-}

-- | Expected utilization metrics for target instance type.
--
-- /Note:/ Consider using 'expectedResourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiExpectedResourceUtilization :: Lens.Lens' TargetInstance (Core.Maybe Types.ResourceUtilization)
tiExpectedResourceUtilization = Lens.field @"expectedResourceUtilization"
{-# INLINEABLE tiExpectedResourceUtilization #-}
{-# DEPRECATED expectedResourceUtilization "Use generic-lens or generic-optics with 'expectedResourceUtilization' instead"  #-}

-- | Details on the target instance type. 
--
-- /Note:/ Consider using 'resourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiResourceDetails :: Lens.Lens' TargetInstance (Core.Maybe Types.ResourceDetails)
tiResourceDetails = Lens.field @"resourceDetails"
{-# INLINEABLE tiResourceDetails #-}
{-# DEPRECATED resourceDetails "Use generic-lens or generic-optics with 'resourceDetails' instead"  #-}

instance Core.FromJSON TargetInstance where
        parseJSON
          = Core.withObject "TargetInstance" Core.$
              \ x ->
                TargetInstance' Core.<$>
                  (x Core..:? "CurrencyCode") Core.<*>
                    x Core..:? "DefaultTargetInstance"
                    Core.<*> x Core..:? "EstimatedMonthlyCost"
                    Core.<*> x Core..:? "EstimatedMonthlySavings"
                    Core.<*> x Core..:? "ExpectedResourceUtilization"
                    Core.<*> x Core..:? "ResourceDetails"
