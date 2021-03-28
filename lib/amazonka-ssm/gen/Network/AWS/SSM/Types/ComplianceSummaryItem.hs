{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ComplianceSummaryItem
  ( ComplianceSummaryItem (..)
  -- * Smart constructor
  , mkComplianceSummaryItem
  -- * Lenses
  , csiComplianceType
  , csiCompliantSummary
  , csiNonCompliantSummary
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ComplianceType as Types
import qualified Network.AWS.SSM.Types.CompliantSummary as Types
import qualified Network.AWS.SSM.Types.NonCompliantSummary as Types

-- | A summary of compliance information by compliance type.
--
-- /See:/ 'mkComplianceSummaryItem' smart constructor.
data ComplianceSummaryItem = ComplianceSummaryItem'
  { complianceType :: Core.Maybe Types.ComplianceType
    -- ^ The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
  , compliantSummary :: Core.Maybe Types.CompliantSummary
    -- ^ A list of COMPLIANT items for the specified compliance type.
  , nonCompliantSummary :: Core.Maybe Types.NonCompliantSummary
    -- ^ A list of NON_COMPLIANT items for the specified compliance type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceSummaryItem' value with any optional fields omitted.
mkComplianceSummaryItem
    :: ComplianceSummaryItem
mkComplianceSummaryItem
  = ComplianceSummaryItem'{complianceType = Core.Nothing,
                           compliantSummary = Core.Nothing,
                           nonCompliantSummary = Core.Nothing}

-- | The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiComplianceType :: Lens.Lens' ComplianceSummaryItem (Core.Maybe Types.ComplianceType)
csiComplianceType = Lens.field @"complianceType"
{-# INLINEABLE csiComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | A list of COMPLIANT items for the specified compliance type.
--
-- /Note:/ Consider using 'compliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiCompliantSummary :: Lens.Lens' ComplianceSummaryItem (Core.Maybe Types.CompliantSummary)
csiCompliantSummary = Lens.field @"compliantSummary"
{-# INLINEABLE csiCompliantSummary #-}
{-# DEPRECATED compliantSummary "Use generic-lens or generic-optics with 'compliantSummary' instead"  #-}

-- | A list of NON_COMPLIANT items for the specified compliance type.
--
-- /Note:/ Consider using 'nonCompliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiNonCompliantSummary :: Lens.Lens' ComplianceSummaryItem (Core.Maybe Types.NonCompliantSummary)
csiNonCompliantSummary = Lens.field @"nonCompliantSummary"
{-# INLINEABLE csiNonCompliantSummary #-}
{-# DEPRECATED nonCompliantSummary "Use generic-lens or generic-optics with 'nonCompliantSummary' instead"  #-}

instance Core.FromJSON ComplianceSummaryItem where
        parseJSON
          = Core.withObject "ComplianceSummaryItem" Core.$
              \ x ->
                ComplianceSummaryItem' Core.<$>
                  (x Core..:? "ComplianceType") Core.<*>
                    x Core..:? "CompliantSummary"
                    Core.<*> x Core..:? "NonCompliantSummary"
