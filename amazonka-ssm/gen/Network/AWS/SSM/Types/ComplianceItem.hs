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
-- Module      : Network.AWS.SSM.Types.ComplianceItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus

-- | Information about the compliance as defined by the resource type. For
-- example, for a patch resource type, @Items@ includes information about
-- the PatchSeverity, Classification, and so on.
--
-- /See:/ 'newComplianceItem' smart constructor.
data ComplianceItem = ComplianceItem'
  { -- | An ID for the resource. For a managed instance, this is the instance ID.
    resourceId :: Core.Maybe Core.Text,
    -- | The status of the compliance item. An item is either COMPLIANT,
    -- NON_COMPLIANT, or an empty string (for Windows patches that aren\'t
    -- applicable).
    status :: Core.Maybe ComplianceStatus,
    -- | The severity of the compliance status. Severity can be one of the
    -- following: Critical, High, Medium, Low, Informational, Unspecified.
    severity :: Core.Maybe ComplianceSeverity,
    -- | A title for the compliance item. For example, if the compliance item is
    -- a Windows patch, the title could be the title of the KB article for the
    -- patch; for example: Security Update for Active Directory Federation
    -- Services.
    title :: Core.Maybe Core.Text,
    -- | An ID for the compliance item. For example, if the compliance item is a
    -- Windows patch, the ID could be the number of the KB article; for
    -- example: KB4010320.
    id :: Core.Maybe Core.Text,
    -- | The compliance type. For example, Association (for a State Manager
    -- association), Patch, or Custom:@string@ are all valid compliance types.
    complianceType :: Core.Maybe Core.Text,
    -- | The type of resource. @ManagedInstance@ is currently the only supported
    -- resource type.
    resourceType :: Core.Maybe Core.Text,
    -- | A \"Key\": \"Value\" tag combination for the compliance item.
    details :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A summary for the compliance item. The summary includes an execution ID,
    -- the execution type (for example, command), and the execution time.
    executionSummary :: Core.Maybe ComplianceExecutionSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComplianceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'complianceItem_resourceId' - An ID for the resource. For a managed instance, this is the instance ID.
--
-- 'status', 'complianceItem_status' - The status of the compliance item. An item is either COMPLIANT,
-- NON_COMPLIANT, or an empty string (for Windows patches that aren\'t
-- applicable).
--
-- 'severity', 'complianceItem_severity' - The severity of the compliance status. Severity can be one of the
-- following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- 'title', 'complianceItem_title' - A title for the compliance item. For example, if the compliance item is
-- a Windows patch, the title could be the title of the KB article for the
-- patch; for example: Security Update for Active Directory Federation
-- Services.
--
-- 'id', 'complianceItem_id' - An ID for the compliance item. For example, if the compliance item is a
-- Windows patch, the ID could be the number of the KB article; for
-- example: KB4010320.
--
-- 'complianceType', 'complianceItem_complianceType' - The compliance type. For example, Association (for a State Manager
-- association), Patch, or Custom:@string@ are all valid compliance types.
--
-- 'resourceType', 'complianceItem_resourceType' - The type of resource. @ManagedInstance@ is currently the only supported
-- resource type.
--
-- 'details', 'complianceItem_details' - A \"Key\": \"Value\" tag combination for the compliance item.
--
-- 'executionSummary', 'complianceItem_executionSummary' - A summary for the compliance item. The summary includes an execution ID,
-- the execution type (for example, command), and the execution time.
newComplianceItem ::
  ComplianceItem
newComplianceItem =
  ComplianceItem'
    { resourceId = Core.Nothing,
      status = Core.Nothing,
      severity = Core.Nothing,
      title = Core.Nothing,
      id = Core.Nothing,
      complianceType = Core.Nothing,
      resourceType = Core.Nothing,
      details = Core.Nothing,
      executionSummary = Core.Nothing
    }

-- | An ID for the resource. For a managed instance, this is the instance ID.
complianceItem_resourceId :: Lens.Lens' ComplianceItem (Core.Maybe Core.Text)
complianceItem_resourceId = Lens.lens (\ComplianceItem' {resourceId} -> resourceId) (\s@ComplianceItem' {} a -> s {resourceId = a} :: ComplianceItem)

-- | The status of the compliance item. An item is either COMPLIANT,
-- NON_COMPLIANT, or an empty string (for Windows patches that aren\'t
-- applicable).
complianceItem_status :: Lens.Lens' ComplianceItem (Core.Maybe ComplianceStatus)
complianceItem_status = Lens.lens (\ComplianceItem' {status} -> status) (\s@ComplianceItem' {} a -> s {status = a} :: ComplianceItem)

-- | The severity of the compliance status. Severity can be one of the
-- following: Critical, High, Medium, Low, Informational, Unspecified.
complianceItem_severity :: Lens.Lens' ComplianceItem (Core.Maybe ComplianceSeverity)
complianceItem_severity = Lens.lens (\ComplianceItem' {severity} -> severity) (\s@ComplianceItem' {} a -> s {severity = a} :: ComplianceItem)

-- | A title for the compliance item. For example, if the compliance item is
-- a Windows patch, the title could be the title of the KB article for the
-- patch; for example: Security Update for Active Directory Federation
-- Services.
complianceItem_title :: Lens.Lens' ComplianceItem (Core.Maybe Core.Text)
complianceItem_title = Lens.lens (\ComplianceItem' {title} -> title) (\s@ComplianceItem' {} a -> s {title = a} :: ComplianceItem)

-- | An ID for the compliance item. For example, if the compliance item is a
-- Windows patch, the ID could be the number of the KB article; for
-- example: KB4010320.
complianceItem_id :: Lens.Lens' ComplianceItem (Core.Maybe Core.Text)
complianceItem_id = Lens.lens (\ComplianceItem' {id} -> id) (\s@ComplianceItem' {} a -> s {id = a} :: ComplianceItem)

-- | The compliance type. For example, Association (for a State Manager
-- association), Patch, or Custom:@string@ are all valid compliance types.
complianceItem_complianceType :: Lens.Lens' ComplianceItem (Core.Maybe Core.Text)
complianceItem_complianceType = Lens.lens (\ComplianceItem' {complianceType} -> complianceType) (\s@ComplianceItem' {} a -> s {complianceType = a} :: ComplianceItem)

-- | The type of resource. @ManagedInstance@ is currently the only supported
-- resource type.
complianceItem_resourceType :: Lens.Lens' ComplianceItem (Core.Maybe Core.Text)
complianceItem_resourceType = Lens.lens (\ComplianceItem' {resourceType} -> resourceType) (\s@ComplianceItem' {} a -> s {resourceType = a} :: ComplianceItem)

-- | A \"Key\": \"Value\" tag combination for the compliance item.
complianceItem_details :: Lens.Lens' ComplianceItem (Core.Maybe (Core.HashMap Core.Text Core.Text))
complianceItem_details = Lens.lens (\ComplianceItem' {details} -> details) (\s@ComplianceItem' {} a -> s {details = a} :: ComplianceItem) Core.. Lens.mapping Lens._Coerce

-- | A summary for the compliance item. The summary includes an execution ID,
-- the execution type (for example, command), and the execution time.
complianceItem_executionSummary :: Lens.Lens' ComplianceItem (Core.Maybe ComplianceExecutionSummary)
complianceItem_executionSummary = Lens.lens (\ComplianceItem' {executionSummary} -> executionSummary) (\s@ComplianceItem' {} a -> s {executionSummary = a} :: ComplianceItem)

instance Core.FromJSON ComplianceItem where
  parseJSON =
    Core.withObject
      "ComplianceItem"
      ( \x ->
          ComplianceItem'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "Severity")
            Core.<*> (x Core..:? "Title")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "ComplianceType")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (x Core..:? "Details" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ExecutionSummary")
      )

instance Core.Hashable ComplianceItem

instance Core.NFData ComplianceItem
