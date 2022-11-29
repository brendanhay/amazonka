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
-- Module      : Amazonka.SSM.Types.ResourceComplianceSummaryItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceComplianceSummaryItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ComplianceExecutionSummary
import Amazonka.SSM.Types.ComplianceSeverity
import Amazonka.SSM.Types.ComplianceStatus
import Amazonka.SSM.Types.CompliantSummary
import Amazonka.SSM.Types.NonCompliantSummary

-- | Compliance summary information for a specific resource.
--
-- /See:/ 'newResourceComplianceSummaryItem' smart constructor.
data ResourceComplianceSummaryItem = ResourceComplianceSummaryItem'
  { -- | The resource ID.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | A list of items that are compliant for the resource.
    compliantSummary :: Prelude.Maybe CompliantSummary,
    -- | The compliance status for the resource.
    status :: Prelude.Maybe ComplianceStatus,
    -- | Information about the execution.
    executionSummary :: Prelude.Maybe ComplianceExecutionSummary,
    -- | A list of items that aren\'t compliant for the resource.
    nonCompliantSummary :: Prelude.Maybe NonCompliantSummary,
    -- | The compliance type.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The highest severity item found for the resource. The resource is
    -- compliant for this item.
    overallSeverity :: Prelude.Maybe ComplianceSeverity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceType', 'resourceComplianceSummaryItem_resourceType' - The resource type.
--
-- 'compliantSummary', 'resourceComplianceSummaryItem_compliantSummary' - A list of items that are compliant for the resource.
--
-- 'status', 'resourceComplianceSummaryItem_status' - The compliance status for the resource.
--
-- 'executionSummary', 'resourceComplianceSummaryItem_executionSummary' - Information about the execution.
--
-- 'nonCompliantSummary', 'resourceComplianceSummaryItem_nonCompliantSummary' - A list of items that aren\'t compliant for the resource.
--
-- 'complianceType', 'resourceComplianceSummaryItem_complianceType' - The compliance type.
--
-- 'overallSeverity', 'resourceComplianceSummaryItem_overallSeverity' - The highest severity item found for the resource. The resource is
-- compliant for this item.
newResourceComplianceSummaryItem ::
  ResourceComplianceSummaryItem
newResourceComplianceSummaryItem =
  ResourceComplianceSummaryItem'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      compliantSummary = Prelude.Nothing,
      status = Prelude.Nothing,
      executionSummary = Prelude.Nothing,
      nonCompliantSummary = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      overallSeverity = Prelude.Nothing
    }

-- | The resource ID.
resourceComplianceSummaryItem_resourceId :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe Prelude.Text)
resourceComplianceSummaryItem_resourceId = Lens.lens (\ResourceComplianceSummaryItem' {resourceId} -> resourceId) (\s@ResourceComplianceSummaryItem' {} a -> s {resourceId = a} :: ResourceComplianceSummaryItem)

-- | The resource type.
resourceComplianceSummaryItem_resourceType :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe Prelude.Text)
resourceComplianceSummaryItem_resourceType = Lens.lens (\ResourceComplianceSummaryItem' {resourceType} -> resourceType) (\s@ResourceComplianceSummaryItem' {} a -> s {resourceType = a} :: ResourceComplianceSummaryItem)

-- | A list of items that are compliant for the resource.
resourceComplianceSummaryItem_compliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe CompliantSummary)
resourceComplianceSummaryItem_compliantSummary = Lens.lens (\ResourceComplianceSummaryItem' {compliantSummary} -> compliantSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {compliantSummary = a} :: ResourceComplianceSummaryItem)

-- | The compliance status for the resource.
resourceComplianceSummaryItem_status :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe ComplianceStatus)
resourceComplianceSummaryItem_status = Lens.lens (\ResourceComplianceSummaryItem' {status} -> status) (\s@ResourceComplianceSummaryItem' {} a -> s {status = a} :: ResourceComplianceSummaryItem)

-- | Information about the execution.
resourceComplianceSummaryItem_executionSummary :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe ComplianceExecutionSummary)
resourceComplianceSummaryItem_executionSummary = Lens.lens (\ResourceComplianceSummaryItem' {executionSummary} -> executionSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {executionSummary = a} :: ResourceComplianceSummaryItem)

-- | A list of items that aren\'t compliant for the resource.
resourceComplianceSummaryItem_nonCompliantSummary :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe NonCompliantSummary)
resourceComplianceSummaryItem_nonCompliantSummary = Lens.lens (\ResourceComplianceSummaryItem' {nonCompliantSummary} -> nonCompliantSummary) (\s@ResourceComplianceSummaryItem' {} a -> s {nonCompliantSummary = a} :: ResourceComplianceSummaryItem)

-- | The compliance type.
resourceComplianceSummaryItem_complianceType :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe Prelude.Text)
resourceComplianceSummaryItem_complianceType = Lens.lens (\ResourceComplianceSummaryItem' {complianceType} -> complianceType) (\s@ResourceComplianceSummaryItem' {} a -> s {complianceType = a} :: ResourceComplianceSummaryItem)

-- | The highest severity item found for the resource. The resource is
-- compliant for this item.
resourceComplianceSummaryItem_overallSeverity :: Lens.Lens' ResourceComplianceSummaryItem (Prelude.Maybe ComplianceSeverity)
resourceComplianceSummaryItem_overallSeverity = Lens.lens (\ResourceComplianceSummaryItem' {overallSeverity} -> overallSeverity) (\s@ResourceComplianceSummaryItem' {} a -> s {overallSeverity = a} :: ResourceComplianceSummaryItem)

instance Core.FromJSON ResourceComplianceSummaryItem where
  parseJSON =
    Core.withObject
      "ResourceComplianceSummaryItem"
      ( \x ->
          ResourceComplianceSummaryItem'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "CompliantSummary")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ExecutionSummary")
            Prelude.<*> (x Core..:? "NonCompliantSummary")
            Prelude.<*> (x Core..:? "ComplianceType")
            Prelude.<*> (x Core..:? "OverallSeverity")
      )

instance
  Prelude.Hashable
    ResourceComplianceSummaryItem
  where
  hashWithSalt _salt ResourceComplianceSummaryItem' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` compliantSummary
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` executionSummary
      `Prelude.hashWithSalt` nonCompliantSummary
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` overallSeverity

instance Prelude.NFData ResourceComplianceSummaryItem where
  rnf ResourceComplianceSummaryItem' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf compliantSummary
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionSummary
      `Prelude.seq` Prelude.rnf nonCompliantSummary
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf overallSeverity
