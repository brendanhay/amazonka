{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
  ( CostCategoryProcessingStatus (..)
  -- * Smart constructor
  , mkCostCategoryProcessingStatus
  -- * Lenses
  , ccpsComponent
  , ccpsStatus
  ) where

import qualified Network.AWS.CostExplorer.Types.CostCategoryStatus as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryStatusComponent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The list of processing statuses for Cost Management products for a specific cost category. 
--
-- /See:/ 'mkCostCategoryProcessingStatus' smart constructor.
data CostCategoryProcessingStatus = CostCategoryProcessingStatus'
  { component :: Core.Maybe Types.CostCategoryStatusComponent
    -- ^ The Cost Management product name of the applied status. 
  , status :: Core.Maybe Types.CostCategoryStatus
    -- ^ The process status for a specific cost category. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CostCategoryProcessingStatus' value with any optional fields omitted.
mkCostCategoryProcessingStatus
    :: CostCategoryProcessingStatus
mkCostCategoryProcessingStatus
  = CostCategoryProcessingStatus'{component = Core.Nothing,
                                  status = Core.Nothing}

-- | The Cost Management product name of the applied status. 
--
-- /Note:/ Consider using 'component' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpsComponent :: Lens.Lens' CostCategoryProcessingStatus (Core.Maybe Types.CostCategoryStatusComponent)
ccpsComponent = Lens.field @"component"
{-# INLINEABLE ccpsComponent #-}
{-# DEPRECATED component "Use generic-lens or generic-optics with 'component' instead"  #-}

-- | The process status for a specific cost category. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpsStatus :: Lens.Lens' CostCategoryProcessingStatus (Core.Maybe Types.CostCategoryStatus)
ccpsStatus = Lens.field @"status"
{-# INLINEABLE ccpsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON CostCategoryProcessingStatus where
        parseJSON
          = Core.withObject "CostCategoryProcessingStatus" Core.$
              \ x ->
                CostCategoryProcessingStatus' Core.<$>
                  (x Core..:? "Component") Core.<*> x Core..:? "Status"
