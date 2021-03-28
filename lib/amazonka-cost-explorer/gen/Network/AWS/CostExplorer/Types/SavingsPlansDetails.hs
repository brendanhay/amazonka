{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SavingsPlansDetails
  ( SavingsPlansDetails (..)
  -- * Smart constructor
  , mkSavingsPlansDetails
  -- * Lenses
  , spdInstanceFamily
  , spdOfferingId
  , spdRegion
  ) where

import qualified Network.AWS.CostExplorer.Types.InstanceFamily as Types
import qualified Network.AWS.CostExplorer.Types.OfferingId as Types
import qualified Network.AWS.CostExplorer.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attribute details on a specific Savings Plan.
--
-- /See:/ 'mkSavingsPlansDetails' smart constructor.
data SavingsPlansDetails = SavingsPlansDetails'
  { instanceFamily :: Core.Maybe Types.InstanceFamily
    -- ^ A group of instance types that Savings Plans applies to.
  , offeringId :: Core.Maybe Types.OfferingId
    -- ^ The unique ID used to distinguish Savings Plans from one another.
  , region :: Core.Maybe Types.Region
    -- ^ A collection of AWS resources in a geographic area. Each AWS Region is isolated and independent of the other Regions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansDetails' value with any optional fields omitted.
mkSavingsPlansDetails
    :: SavingsPlansDetails
mkSavingsPlansDetails
  = SavingsPlansDetails'{instanceFamily = Core.Nothing,
                         offeringId = Core.Nothing, region = Core.Nothing}

-- | A group of instance types that Savings Plans applies to.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spdInstanceFamily :: Lens.Lens' SavingsPlansDetails (Core.Maybe Types.InstanceFamily)
spdInstanceFamily = Lens.field @"instanceFamily"
{-# INLINEABLE spdInstanceFamily #-}
{-# DEPRECATED instanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead"  #-}

-- | The unique ID used to distinguish Savings Plans from one another.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spdOfferingId :: Lens.Lens' SavingsPlansDetails (Core.Maybe Types.OfferingId)
spdOfferingId = Lens.field @"offeringId"
{-# INLINEABLE spdOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | A collection of AWS resources in a geographic area. Each AWS Region is isolated and independent of the other Regions.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spdRegion :: Lens.Lens' SavingsPlansDetails (Core.Maybe Types.Region)
spdRegion = Lens.field @"region"
{-# INLINEABLE spdRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

instance Core.FromJSON SavingsPlansDetails where
        parseJSON
          = Core.withObject "SavingsPlansDetails" Core.$
              \ x ->
                SavingsPlansDetails' Core.<$>
                  (x Core..:? "InstanceFamily") Core.<*> x Core..:? "OfferingId"
                    Core.<*> x Core..:? "Region"
