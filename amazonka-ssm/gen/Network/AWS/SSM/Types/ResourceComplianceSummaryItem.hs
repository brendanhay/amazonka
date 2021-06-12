{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceComplianceSummaryItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceComplianceSummaryItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.NonCompliantSummary

-- | Compliance summary information for a specific resource.
--
-- /See:/ 'newResourceComplianceSummaryItem' smart constructor.
data ResourceComplianceSummaryItem = ResourceComplianceSummaryItem'
  { -- | The resource ID.
    resourceId :: Core.Maybe Core.Text,
    -- | The compliance status for the resource.
    status :: Core.Maybe ComplianceStatus,
    -- | The highest severity item found for the resource. The resource is
    -- compliant for this item.
    overallSeverity :: Core.Maybe ComplianceSeverity,
    -- | A list of items that are compliant for the resource.
    compliantSummary :: Core.Maybe CompliantSummary,
    -- | The compliance type.
    complianceType :: Core.Maybe Core.Text,
    -- | The resource type.
    resourceType :: Core.Maybe Core.Text,
    -- | A list of items that aren\'t compliant for the resource.
    nonCompliantSummary :: Core.Maybe NonCompliantSummary,
    -- | Information about the execution.
    executionSummary :: Core.Maybe ComplianceExecutionSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceComplianceSummaryItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'resourceComplianceSummaryItem_resourceId' - The resource ID.
--
-- 'status', 'resourceComplianceSummaryItem_status' - The compliance status for the resource.
--
-- 'overallSeverity', 'resourceComplianceSummaryItem_overallSeverity' - The highest severity item found for the resource. The resource is
-- compliant for this item.
--
-- 'compliantSummary', 'resourceComplianceSummaryItem_compliantSummary' - A list of items that are compliant for the resource.
--
-- 'complianceType', 'resourceComplianceSummaryItem_complianceType' - The compliance type.
--
-- 'resourceType', 'resourceComplianceSummaryItem_resourceType' - The resource type.
--
-- 'nonCompliantSummary', 'resourceComplianceSummaryItem_nonCompliantSummary' - A list of items that aren\'t compliant for the resource.
--
-- 'executionSummary', 'resourceComplianceSummaryItem_executionSummary' - Information about the execution.
newResourceComplianceSummaryItem ::
  ResourceComplianceSummaryItem
newResourceComplianceSummaryItem =
  ResourceComplianceSummaryItem'
    { resourceId =
        Core.Nothing,
      status = Core.Nothing,
      overallSeverity = Core.Nothing,
      compliantSummary = Core.Nothing,
      complianceType = Core.Nothing,
      resourceType = Core.Nothing,
      nonCompliantSummary = Core.Nothing,
      executionSummary = Core.Nothing
    }

-- | The resource ID.
resourceComplianceSummaryItem_resourceId :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Core.Text)
resourceComplianceSummaryItem_resourceId = Lens.lens (\ResourceComplianceSummaryItem' {resourceId} -> resourceId) (\s@ResourceComplianceSummaryItem' {} a -> s {resourceId = a} :: ResourceComplianceSummaryItem)

-- | The compliance status for the resource.
resourceComplianceSummaryItem_status :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe ComplianceStatus)
resourceComplianceSummaryItem_status = Lens.lens (\ResourceComplianceSummaryItem' {status} -> status) (\s@ResourceComplianceSummaryItem' {} a -> s {status = a} :: ResourceComplianceSummaryItem)

-- | The highest severity item found for the resource. The resource is
-- compliant for this item.
resourceComplianceSummaryItem_overallSeverity :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe ComplianceSeverity)
resourceComplianceSummaryItem_overallSeverity = Lens.lens (\ResourceComplianceSummaryItem' {overallSeverity} -> overallSeverity) (\s@ResourceComplianceSummaryItem' {} a -> s {overallSeverity = a} :: ResourceComplianceSummaryItem)

-- | A list of items that are compliant for the resource.
resourceComplianceSummaryItem_compliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe CompliantSummary)
resourceComplianceSummaryItem_compliantSummary = Lens.lens (\ResourceComplianceSummaryItem' {compliantSummary} -> compliantSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {compliantSummary = a} :: ResourceComplianceSummaryItem)

-- | The compliance type.
resourceComplianceSummaryItem_complianceType :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Core.Text)
resourceComplianceSummaryItem_complianceType = Lens.lens (\ResourceComplianceSummaryItem' {complianceType} -> complianceType) (\s@ResourceComplianceSummaryItem' {} a -> s {complianceType = a} :: ResourceComplianceSummaryItem)

-- | The resource type.
resourceComplianceSummaryItem_resourceType :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe Core.Text)
resourceComplianceSummaryItem_resourceType = Lens.lens (\ResourceComplianceSummaryItem' {resourceType} -> resourceType) (\s@ResourceComplianceSummaryItem' {} a -> s {resourceType = a} :: ResourceComplianceSummaryItem)

-- | A list of items that aren\'t compliant for the resource.
resourceComplianceSummaryItem_nonCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe NonCompliantSummary)
resourceComplianceSummaryItem_nonCompliantSummary = Lens.lens (\ResourceComplianceSummaryItem' {nonCompliantSummary} -> nonCompliantSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {nonCompliantSummary = a} :: ResourceComplianceSummaryItem)

-- | Information about the execution.
resourceComplianceSummaryItem_executionSummary :: Lens.Lens' ResourceComplianceSummaryItem (Core.Maybe ComplianceExecutionSummary)
resourceComplianceSummaryItem_executionSummary = Lens.lens (\ResourceComplianceSummaryItem' {executionSummary} -> executionSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {executionSummary = a} :: ResourceComplianceSummaryItem)

instance Core.FromJSON ResourceComplianceSummaryItem where
  parseJSON =
    Core.withObject
      "ResourceComplianceSummaryItem"
      ( \x ->
          ResourceComplianceSummaryItem'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "OverallSeverity")
            Core.<*> (x Core..:? "CompliantSummary")
            Core.<*> (x Core..:? "ComplianceType")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (x Core..:? "NonCompliantSummary")
            Core.<*> (x Core..:? "ExecutionSummary")
      )

instance Core.Hashable ResourceComplianceSummaryItem

instance Core.NFData ResourceComplianceSummaryItem
