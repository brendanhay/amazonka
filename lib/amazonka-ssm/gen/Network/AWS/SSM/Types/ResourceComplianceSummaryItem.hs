{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceComplianceSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceComplianceSummaryItem
  ( ResourceComplianceSummaryItem (..)
  -- * Smart constructor
  , mkResourceComplianceSummaryItem
  -- * Lenses
  , rcsiComplianceType
  , rcsiCompliantSummary
  , rcsiExecutionSummary
  , rcsiNonCompliantSummary
  , rcsiOverallSeverity
  , rcsiResourceId
  , rcsiResourceType
  , rcsiStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ComplianceExecutionSummary as Types
import qualified Network.AWS.SSM.Types.ComplianceResourceId as Types
import qualified Network.AWS.SSM.Types.ComplianceResourceType as Types
import qualified Network.AWS.SSM.Types.ComplianceSeverity as Types
import qualified Network.AWS.SSM.Types.ComplianceStatus as Types
import qualified Network.AWS.SSM.Types.ComplianceTypeName as Types
import qualified Network.AWS.SSM.Types.CompliantSummary as Types
import qualified Network.AWS.SSM.Types.NonCompliantSummary as Types

-- | Compliance summary information for a specific resource. 
--
-- /See:/ 'mkResourceComplianceSummaryItem' smart constructor.
data ResourceComplianceSummaryItem = ResourceComplianceSummaryItem'
  { complianceType :: Core.Maybe Types.ComplianceTypeName
    -- ^ The compliance type.
  , compliantSummary :: Core.Maybe Types.CompliantSummary
    -- ^ A list of items that are compliant for the resource.
  , executionSummary :: Core.Maybe Types.ComplianceExecutionSummary
    -- ^ Information about the execution.
  , nonCompliantSummary :: Core.Maybe Types.NonCompliantSummary
    -- ^ A list of items that aren't compliant for the resource.
  , overallSeverity :: Core.Maybe Types.ComplianceSeverity
    -- ^ The highest severity item found for the resource. The resource is compliant for this item.
  , resourceId :: Core.Maybe Types.ComplianceResourceId
    -- ^ The resource ID.
  , resourceType :: Core.Maybe Types.ComplianceResourceType
    -- ^ The resource type.
  , status :: Core.Maybe Types.ComplianceStatus
    -- ^ The compliance status for the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResourceComplianceSummaryItem' value with any optional fields omitted.
mkResourceComplianceSummaryItem
    :: ResourceComplianceSummaryItem
mkResourceComplianceSummaryItem
  = ResourceComplianceSummaryItem'{complianceType = Core.Nothing,
                                   compliantSummary = Core.Nothing, executionSummary = Core.Nothing,
                                   nonCompliantSummary = Core.Nothing,
                                   overallSeverity = Core.Nothing, resourceId = Core.Nothing,
                                   resourceType = Core.Nothing, status = Core.Nothing}

-- | The compliance type.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiComplianceType :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.ComplianceTypeName)
rcsiComplianceType = Lens.field @"complianceType"
{-# INLINEABLE rcsiComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | A list of items that are compliant for the resource.
--
-- /Note:/ Consider using 'compliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.CompliantSummary)
rcsiCompliantSummary = Lens.field @"compliantSummary"
{-# INLINEABLE rcsiCompliantSummary #-}
{-# DEPRECATED compliantSummary "Use generic-lens or generic-optics with 'compliantSummary' instead"  #-}

-- | Information about the execution.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiExecutionSummary :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.ComplianceExecutionSummary)
rcsiExecutionSummary = Lens.field @"executionSummary"
{-# INLINEABLE rcsiExecutionSummary #-}
{-# DEPRECATED executionSummary "Use generic-lens or generic-optics with 'executionSummary' instead"  #-}

-- | A list of items that aren't compliant for the resource.
--
-- /Note:/ Consider using 'nonCompliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiNonCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.NonCompliantSummary)
rcsiNonCompliantSummary = Lens.field @"nonCompliantSummary"
{-# INLINEABLE rcsiNonCompliantSummary #-}
{-# DEPRECATED nonCompliantSummary "Use generic-lens or generic-optics with 'nonCompliantSummary' instead"  #-}

-- | The highest severity item found for the resource. The resource is compliant for this item.
--
-- /Note:/ Consider using 'overallSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiOverallSeverity :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.ComplianceSeverity)
rcsiOverallSeverity = Lens.field @"overallSeverity"
{-# INLINEABLE rcsiOverallSeverity #-}
{-# DEPRECATED overallSeverity "Use generic-lens or generic-optics with 'overallSeverity' instead"  #-}

-- | The resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiResourceId :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.ComplianceResourceId)
rcsiResourceId = Lens.field @"resourceId"
{-# INLINEABLE rcsiResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiResourceType :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.ComplianceResourceType)
rcsiResourceType = Lens.field @"resourceType"
{-# INLINEABLE rcsiResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The compliance status for the resource.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiStatus :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Types.ComplianceStatus)
rcsiStatus = Lens.field @"status"
{-# INLINEABLE rcsiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ResourceComplianceSummaryItem where
        parseJSON
          = Core.withObject "ResourceComplianceSummaryItem" Core.$
              \ x ->
                ResourceComplianceSummaryItem' Core.<$>
                  (x Core..:? "ComplianceType") Core.<*>
                    x Core..:? "CompliantSummary"
                    Core.<*> x Core..:? "ExecutionSummary"
                    Core.<*> x Core..:? "NonCompliantSummary"
                    Core.<*> x Core..:? "OverallSeverity"
                    Core.<*> x Core..:? "ResourceId"
                    Core.<*> x Core..:? "ResourceType"
                    Core.<*> x Core..:? "Status"
