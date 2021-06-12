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
-- Module      : Network.AWS.IoT.Types.AuditFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFinding where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AuditFindingSeverity
import Network.AWS.IoT.Types.NonCompliantResource
import Network.AWS.IoT.Types.RelatedResource
import qualified Network.AWS.Lens as Lens

-- | The findings (results) of the audit.
--
-- /See:/ 'newAuditFinding' smart constructor.
data AuditFinding = AuditFinding'
  { -- | The severity of the result (finding).
    severity :: Core.Maybe AuditFindingSeverity,
    -- | A unique identifier for this set of audit findings. This identifier is
    -- used to apply mitigation tasks to one or more sets of findings.
    findingId :: Core.Maybe Core.Text,
    -- | The ID of the audit that generated this result (finding).
    taskId :: Core.Maybe Core.Text,
    -- | A code that indicates the reason that the resource was noncompliant.
    reasonForNonComplianceCode :: Core.Maybe Core.Text,
    -- | The reason the resource was noncompliant.
    reasonForNonCompliance :: Core.Maybe Core.Text,
    -- | Indicates whether the audit finding was suppressed or not during
    -- reporting.
    isSuppressed :: Core.Maybe Core.Bool,
    -- | The audit check that generated this result.
    checkName :: Core.Maybe Core.Text,
    -- | The list of related resources.
    relatedResources :: Core.Maybe [RelatedResource],
    -- | The time the result (finding) was discovered.
    findingTime :: Core.Maybe Core.POSIX,
    -- | The time the audit started.
    taskStartTime :: Core.Maybe Core.POSIX,
    -- | The resource that was found to be noncompliant with the audit check.
    nonCompliantResource :: Core.Maybe NonCompliantResource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuditFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severity', 'auditFinding_severity' - The severity of the result (finding).
--
-- 'findingId', 'auditFinding_findingId' - A unique identifier for this set of audit findings. This identifier is
-- used to apply mitigation tasks to one or more sets of findings.
--
-- 'taskId', 'auditFinding_taskId' - The ID of the audit that generated this result (finding).
--
-- 'reasonForNonComplianceCode', 'auditFinding_reasonForNonComplianceCode' - A code that indicates the reason that the resource was noncompliant.
--
-- 'reasonForNonCompliance', 'auditFinding_reasonForNonCompliance' - The reason the resource was noncompliant.
--
-- 'isSuppressed', 'auditFinding_isSuppressed' - Indicates whether the audit finding was suppressed or not during
-- reporting.
--
-- 'checkName', 'auditFinding_checkName' - The audit check that generated this result.
--
-- 'relatedResources', 'auditFinding_relatedResources' - The list of related resources.
--
-- 'findingTime', 'auditFinding_findingTime' - The time the result (finding) was discovered.
--
-- 'taskStartTime', 'auditFinding_taskStartTime' - The time the audit started.
--
-- 'nonCompliantResource', 'auditFinding_nonCompliantResource' - The resource that was found to be noncompliant with the audit check.
newAuditFinding ::
  AuditFinding
newAuditFinding =
  AuditFinding'
    { severity = Core.Nothing,
      findingId = Core.Nothing,
      taskId = Core.Nothing,
      reasonForNonComplianceCode = Core.Nothing,
      reasonForNonCompliance = Core.Nothing,
      isSuppressed = Core.Nothing,
      checkName = Core.Nothing,
      relatedResources = Core.Nothing,
      findingTime = Core.Nothing,
      taskStartTime = Core.Nothing,
      nonCompliantResource = Core.Nothing
    }

-- | The severity of the result (finding).
auditFinding_severity :: Lens.Lens' AuditFinding (Core.Maybe AuditFindingSeverity)
auditFinding_severity = Lens.lens (\AuditFinding' {severity} -> severity) (\s@AuditFinding' {} a -> s {severity = a} :: AuditFinding)

-- | A unique identifier for this set of audit findings. This identifier is
-- used to apply mitigation tasks to one or more sets of findings.
auditFinding_findingId :: Lens.Lens' AuditFinding (Core.Maybe Core.Text)
auditFinding_findingId = Lens.lens (\AuditFinding' {findingId} -> findingId) (\s@AuditFinding' {} a -> s {findingId = a} :: AuditFinding)

-- | The ID of the audit that generated this result (finding).
auditFinding_taskId :: Lens.Lens' AuditFinding (Core.Maybe Core.Text)
auditFinding_taskId = Lens.lens (\AuditFinding' {taskId} -> taskId) (\s@AuditFinding' {} a -> s {taskId = a} :: AuditFinding)

-- | A code that indicates the reason that the resource was noncompliant.
auditFinding_reasonForNonComplianceCode :: Lens.Lens' AuditFinding (Core.Maybe Core.Text)
auditFinding_reasonForNonComplianceCode = Lens.lens (\AuditFinding' {reasonForNonComplianceCode} -> reasonForNonComplianceCode) (\s@AuditFinding' {} a -> s {reasonForNonComplianceCode = a} :: AuditFinding)

-- | The reason the resource was noncompliant.
auditFinding_reasonForNonCompliance :: Lens.Lens' AuditFinding (Core.Maybe Core.Text)
auditFinding_reasonForNonCompliance = Lens.lens (\AuditFinding' {reasonForNonCompliance} -> reasonForNonCompliance) (\s@AuditFinding' {} a -> s {reasonForNonCompliance = a} :: AuditFinding)

-- | Indicates whether the audit finding was suppressed or not during
-- reporting.
auditFinding_isSuppressed :: Lens.Lens' AuditFinding (Core.Maybe Core.Bool)
auditFinding_isSuppressed = Lens.lens (\AuditFinding' {isSuppressed} -> isSuppressed) (\s@AuditFinding' {} a -> s {isSuppressed = a} :: AuditFinding)

-- | The audit check that generated this result.
auditFinding_checkName :: Lens.Lens' AuditFinding (Core.Maybe Core.Text)
auditFinding_checkName = Lens.lens (\AuditFinding' {checkName} -> checkName) (\s@AuditFinding' {} a -> s {checkName = a} :: AuditFinding)

-- | The list of related resources.
auditFinding_relatedResources :: Lens.Lens' AuditFinding (Core.Maybe [RelatedResource])
auditFinding_relatedResources = Lens.lens (\AuditFinding' {relatedResources} -> relatedResources) (\s@AuditFinding' {} a -> s {relatedResources = a} :: AuditFinding) Core.. Lens.mapping Lens._Coerce

-- | The time the result (finding) was discovered.
auditFinding_findingTime :: Lens.Lens' AuditFinding (Core.Maybe Core.UTCTime)
auditFinding_findingTime = Lens.lens (\AuditFinding' {findingTime} -> findingTime) (\s@AuditFinding' {} a -> s {findingTime = a} :: AuditFinding) Core.. Lens.mapping Core._Time

-- | The time the audit started.
auditFinding_taskStartTime :: Lens.Lens' AuditFinding (Core.Maybe Core.UTCTime)
auditFinding_taskStartTime = Lens.lens (\AuditFinding' {taskStartTime} -> taskStartTime) (\s@AuditFinding' {} a -> s {taskStartTime = a} :: AuditFinding) Core.. Lens.mapping Core._Time

-- | The resource that was found to be noncompliant with the audit check.
auditFinding_nonCompliantResource :: Lens.Lens' AuditFinding (Core.Maybe NonCompliantResource)
auditFinding_nonCompliantResource = Lens.lens (\AuditFinding' {nonCompliantResource} -> nonCompliantResource) (\s@AuditFinding' {} a -> s {nonCompliantResource = a} :: AuditFinding)

instance Core.FromJSON AuditFinding where
  parseJSON =
    Core.withObject
      "AuditFinding"
      ( \x ->
          AuditFinding'
            Core.<$> (x Core..:? "severity")
            Core.<*> (x Core..:? "findingId")
            Core.<*> (x Core..:? "taskId")
            Core.<*> (x Core..:? "reasonForNonComplianceCode")
            Core.<*> (x Core..:? "reasonForNonCompliance")
            Core.<*> (x Core..:? "isSuppressed")
            Core.<*> (x Core..:? "checkName")
            Core.<*> (x Core..:? "relatedResources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "findingTime")
            Core.<*> (x Core..:? "taskStartTime")
            Core.<*> (x Core..:? "nonCompliantResource")
      )

instance Core.Hashable AuditFinding

instance Core.NFData AuditFinding
