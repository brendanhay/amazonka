-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceComplianceSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceComplianceSummaryItem
  ( ResourceComplianceSummaryItem (..),

    -- * Smart constructor
    mkResourceComplianceSummaryItem,

    -- * Lenses
    rcsiNonCompliantSummary,
    rcsiStatus,
    rcsiResourceId,
    rcsiResourceType,
    rcsiCompliantSummary,
    rcsiExecutionSummary,
    rcsiOverallSeverity,
    rcsiComplianceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.NonCompliantSummary

-- | Compliance summary information for a specific resource.
--
-- /See:/ 'mkResourceComplianceSummaryItem' smart constructor.
data ResourceComplianceSummaryItem = ResourceComplianceSummaryItem'
  { nonCompliantSummary ::
      Lude.Maybe NonCompliantSummary,
    status ::
      Lude.Maybe ComplianceStatus,
    resourceId ::
      Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe Lude.Text,
    compliantSummary ::
      Lude.Maybe CompliantSummary,
    executionSummary ::
      Lude.Maybe
        ComplianceExecutionSummary,
    overallSeverity ::
      Lude.Maybe ComplianceSeverity,
    complianceType ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceComplianceSummaryItem' with the minimum fields required to make a request.
--
-- * 'complianceType' - The compliance type.
-- * 'compliantSummary' - A list of items that are compliant for the resource.
-- * 'executionSummary' - Information about the execution.
-- * 'nonCompliantSummary' - A list of items that aren't compliant for the resource.
-- * 'overallSeverity' - The highest severity item found for the resource. The resource is compliant for this item.
-- * 'resourceId' - The resource ID.
-- * 'resourceType' - The resource type.
-- * 'status' - The compliance status for the resource.
mkResourceComplianceSummaryItem ::
  ResourceComplianceSummaryItem
mkResourceComplianceSummaryItem =
  ResourceComplianceSummaryItem'
    { nonCompliantSummary =
        Lude.Nothing,
      status = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      compliantSummary = Lude.Nothing,
      executionSummary = Lude.Nothing,
      overallSeverity = Lude.Nothing,
      complianceType = Lude.Nothing
    }

-- | A list of items that aren't compliant for the resource.
--
-- /Note:/ Consider using 'nonCompliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiNonCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe NonCompliantSummary)
rcsiNonCompliantSummary = Lens.lens (nonCompliantSummary :: ResourceComplianceSummaryItem -> Lude.Maybe NonCompliantSummary) (\s a -> s {nonCompliantSummary = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiNonCompliantSummary "Use generic-lens or generic-optics with 'nonCompliantSummary' instead." #-}

-- | The compliance status for the resource.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiStatus :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe ComplianceStatus)
rcsiStatus = Lens.lens (status :: ResourceComplianceSummaryItem -> Lude.Maybe ComplianceStatus) (\s a -> s {status = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiResourceId :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe Lude.Text)
rcsiResourceId = Lens.lens (resourceId :: ResourceComplianceSummaryItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiResourceType :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe Lude.Text)
rcsiResourceType = Lens.lens (resourceType :: ResourceComplianceSummaryItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A list of items that are compliant for the resource.
--
-- /Note:/ Consider using 'compliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe CompliantSummary)
rcsiCompliantSummary = Lens.lens (compliantSummary :: ResourceComplianceSummaryItem -> Lude.Maybe CompliantSummary) (\s a -> s {compliantSummary = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiCompliantSummary "Use generic-lens or generic-optics with 'compliantSummary' instead." #-}

-- | Information about the execution.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiExecutionSummary :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe ComplianceExecutionSummary)
rcsiExecutionSummary = Lens.lens (executionSummary :: ResourceComplianceSummaryItem -> Lude.Maybe ComplianceExecutionSummary) (\s a -> s {executionSummary = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiExecutionSummary "Use generic-lens or generic-optics with 'executionSummary' instead." #-}

-- | The highest severity item found for the resource. The resource is compliant for this item.
--
-- /Note:/ Consider using 'overallSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiOverallSeverity :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe ComplianceSeverity)
rcsiOverallSeverity = Lens.lens (overallSeverity :: ResourceComplianceSummaryItem -> Lude.Maybe ComplianceSeverity) (\s a -> s {overallSeverity = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiOverallSeverity "Use generic-lens or generic-optics with 'overallSeverity' instead." #-}

-- | The compliance type.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsiComplianceType :: Lens.Lens' ResourceComplianceSummaryItem (Lude.Maybe Lude.Text)
rcsiComplianceType = Lens.lens (complianceType :: ResourceComplianceSummaryItem -> Lude.Maybe Lude.Text) (\s a -> s {complianceType = a} :: ResourceComplianceSummaryItem)
{-# DEPRECATED rcsiComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.FromJSON ResourceComplianceSummaryItem where
  parseJSON =
    Lude.withObject
      "ResourceComplianceSummaryItem"
      ( \x ->
          ResourceComplianceSummaryItem'
            Lude.<$> (x Lude..:? "NonCompliantSummary")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "CompliantSummary")
            Lude.<*> (x Lude..:? "ExecutionSummary")
            Lude.<*> (x Lude..:? "OverallSeverity")
            Lude.<*> (x Lude..:? "ComplianceType")
      )
