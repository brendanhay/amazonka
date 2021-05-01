{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The compliance status for the resource.
    status :: Prelude.Maybe ComplianceStatus,
    -- | The highest severity item found for the resource. The resource is
    -- compliant for this item.
    overallSeverity :: Prelude.Maybe ComplianceSeverity,
    -- | A list of items that are compliant for the resource.
    compliantSummary :: Prelude.Maybe CompliantSummary,
    -- | The compliance type.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | A list of items that aren\'t compliant for the resource.
    nonCompliantSummary :: Prelude.Maybe NonCompliantSummary,
    -- | Information about the execution.
    executionSummary :: Prelude.Maybe ComplianceExecutionSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      overallSeverity = Prelude.Nothing,
      compliantSummary = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      nonCompliantSummary = Prelude.Nothing,
      executionSummary = Prelude.Nothing
    }

-- | The resource ID.
resourceComplianceSummaryItem_resourceId :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe Prelude.Text)
resourceComplianceSummaryItem_resourceId = Lens.lens (\ResourceComplianceSummaryItem' {resourceId} -> resourceId) (\s@ResourceComplianceSummaryItem' {} a -> s {resourceId = a} :: ResourceComplianceSummaryItem)

-- | The compliance status for the resource.
resourceComplianceSummaryItem_status :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe ComplianceStatus)
resourceComplianceSummaryItem_status = Lens.lens (\ResourceComplianceSummaryItem' {status} -> status) (\s@ResourceComplianceSummaryItem' {} a -> s {status = a} :: ResourceComplianceSummaryItem)

-- | The highest severity item found for the resource. The resource is
-- compliant for this item.
resourceComplianceSummaryItem_overallSeverity :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe ComplianceSeverity)
resourceComplianceSummaryItem_overallSeverity = Lens.lens (\ResourceComplianceSummaryItem' {overallSeverity} -> overallSeverity) (\s@ResourceComplianceSummaryItem' {} a -> s {overallSeverity = a} :: ResourceComplianceSummaryItem)

-- | A list of items that are compliant for the resource.
resourceComplianceSummaryItem_compliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe CompliantSummary)
resourceComplianceSummaryItem_compliantSummary = Lens.lens (\ResourceComplianceSummaryItem' {compliantSummary} -> compliantSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {compliantSummary = a} :: ResourceComplianceSummaryItem)

-- | The compliance type.
resourceComplianceSummaryItem_complianceType :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe Prelude.Text)
resourceComplianceSummaryItem_complianceType = Lens.lens (\ResourceComplianceSummaryItem' {complianceType} -> complianceType) (\s@ResourceComplianceSummaryItem' {} a -> s {complianceType = a} :: ResourceComplianceSummaryItem)

-- | The resource type.
resourceComplianceSummaryItem_resourceType :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe Prelude.Text)
resourceComplianceSummaryItem_resourceType = Lens.lens (\ResourceComplianceSummaryItem' {resourceType} -> resourceType) (\s@ResourceComplianceSummaryItem' {} a -> s {resourceType = a} :: ResourceComplianceSummaryItem)

-- | A list of items that aren\'t compliant for the resource.
resourceComplianceSummaryItem_nonCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe NonCompliantSummary)
resourceComplianceSummaryItem_nonCompliantSummary = Lens.lens (\ResourceComplianceSummaryItem' {nonCompliantSummary} -> nonCompliantSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {nonCompliantSummary = a} :: ResourceComplianceSummaryItem)

-- | Information about the execution.
resourceComplianceSummaryItem_executionSummary :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe ComplianceExecutionSummary)
resourceComplianceSummaryItem_executionSummary = Lens.lens (\ResourceComplianceSummaryItem' {executionSummary} -> executionSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {executionSummary = a} :: ResourceComplianceSummaryItem)

instance
  Prelude.FromJSON
    ResourceComplianceSummaryItem
  where
  parseJSON =
    Prelude.withObject
      "ResourceComplianceSummaryItem"
      ( \x ->
          ResourceComplianceSummaryItem'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "OverallSeverity")
            Prelude.<*> (x Prelude..:? "CompliantSummary")
            Prelude.<*> (x Prelude..:? "ComplianceType")
            Prelude.<*> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..:? "NonCompliantSummary")
            Prelude.<*> (x Prelude..:? "ExecutionSummary")
      )

instance
  Prelude.Hashable
    ResourceComplianceSummaryItem

instance Prelude.NFData ResourceComplianceSummaryItem
