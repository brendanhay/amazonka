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
-- Module      : Network.AWS.IoT.Types.AuditFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFinding where

import Network.AWS.IoT.Types.AuditFindingSeverity
import Network.AWS.IoT.Types.NonCompliantResource
import Network.AWS.IoT.Types.RelatedResource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The findings (results) of the audit.
--
-- /See:/ 'newAuditFinding' smart constructor.
data AuditFinding = AuditFinding'
  { -- | The severity of the result (finding).
    severity :: Prelude.Maybe AuditFindingSeverity,
    -- | A unique identifier for this set of audit findings. This identifier is
    -- used to apply mitigation tasks to one or more sets of findings.
    findingId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the audit that generated this result (finding).
    taskId :: Prelude.Maybe Prelude.Text,
    -- | A code that indicates the reason that the resource was noncompliant.
    reasonForNonComplianceCode :: Prelude.Maybe Prelude.Text,
    -- | The reason the resource was noncompliant.
    reasonForNonCompliance :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the audit finding was suppressed or not during
    -- reporting.
    isSuppressed :: Prelude.Maybe Prelude.Bool,
    -- | The audit check that generated this result.
    checkName :: Prelude.Maybe Prelude.Text,
    -- | The list of related resources.
    relatedResources :: Prelude.Maybe [RelatedResource],
    -- | The time the result (finding) was discovered.
    findingTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time the audit started.
    taskStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The resource that was found to be noncompliant with the audit check.
    nonCompliantResource :: Prelude.Maybe NonCompliantResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { severity = Prelude.Nothing,
      findingId = Prelude.Nothing,
      taskId = Prelude.Nothing,
      reasonForNonComplianceCode = Prelude.Nothing,
      reasonForNonCompliance = Prelude.Nothing,
      isSuppressed = Prelude.Nothing,
      checkName = Prelude.Nothing,
      relatedResources = Prelude.Nothing,
      findingTime = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      nonCompliantResource = Prelude.Nothing
    }

-- | The severity of the result (finding).
auditFinding_severity :: Lens.Lens' AuditFinding (Prelude.Maybe AuditFindingSeverity)
auditFinding_severity = Lens.lens (\AuditFinding' {severity} -> severity) (\s@AuditFinding' {} a -> s {severity = a} :: AuditFinding)

-- | A unique identifier for this set of audit findings. This identifier is
-- used to apply mitigation tasks to one or more sets of findings.
auditFinding_findingId :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_findingId = Lens.lens (\AuditFinding' {findingId} -> findingId) (\s@AuditFinding' {} a -> s {findingId = a} :: AuditFinding)

-- | The ID of the audit that generated this result (finding).
auditFinding_taskId :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_taskId = Lens.lens (\AuditFinding' {taskId} -> taskId) (\s@AuditFinding' {} a -> s {taskId = a} :: AuditFinding)

-- | A code that indicates the reason that the resource was noncompliant.
auditFinding_reasonForNonComplianceCode :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_reasonForNonComplianceCode = Lens.lens (\AuditFinding' {reasonForNonComplianceCode} -> reasonForNonComplianceCode) (\s@AuditFinding' {} a -> s {reasonForNonComplianceCode = a} :: AuditFinding)

-- | The reason the resource was noncompliant.
auditFinding_reasonForNonCompliance :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_reasonForNonCompliance = Lens.lens (\AuditFinding' {reasonForNonCompliance} -> reasonForNonCompliance) (\s@AuditFinding' {} a -> s {reasonForNonCompliance = a} :: AuditFinding)

-- | Indicates whether the audit finding was suppressed or not during
-- reporting.
auditFinding_isSuppressed :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Bool)
auditFinding_isSuppressed = Lens.lens (\AuditFinding' {isSuppressed} -> isSuppressed) (\s@AuditFinding' {} a -> s {isSuppressed = a} :: AuditFinding)

-- | The audit check that generated this result.
auditFinding_checkName :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_checkName = Lens.lens (\AuditFinding' {checkName} -> checkName) (\s@AuditFinding' {} a -> s {checkName = a} :: AuditFinding)

-- | The list of related resources.
auditFinding_relatedResources :: Lens.Lens' AuditFinding (Prelude.Maybe [RelatedResource])
auditFinding_relatedResources = Lens.lens (\AuditFinding' {relatedResources} -> relatedResources) (\s@AuditFinding' {} a -> s {relatedResources = a} :: AuditFinding) Prelude.. Lens.mapping Prelude._Coerce

-- | The time the result (finding) was discovered.
auditFinding_findingTime :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.UTCTime)
auditFinding_findingTime = Lens.lens (\AuditFinding' {findingTime} -> findingTime) (\s@AuditFinding' {} a -> s {findingTime = a} :: AuditFinding) Prelude.. Lens.mapping Prelude._Time

-- | The time the audit started.
auditFinding_taskStartTime :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.UTCTime)
auditFinding_taskStartTime = Lens.lens (\AuditFinding' {taskStartTime} -> taskStartTime) (\s@AuditFinding' {} a -> s {taskStartTime = a} :: AuditFinding) Prelude.. Lens.mapping Prelude._Time

-- | The resource that was found to be noncompliant with the audit check.
auditFinding_nonCompliantResource :: Lens.Lens' AuditFinding (Prelude.Maybe NonCompliantResource)
auditFinding_nonCompliantResource = Lens.lens (\AuditFinding' {nonCompliantResource} -> nonCompliantResource) (\s@AuditFinding' {} a -> s {nonCompliantResource = a} :: AuditFinding)

instance Prelude.FromJSON AuditFinding where
  parseJSON =
    Prelude.withObject
      "AuditFinding"
      ( \x ->
          AuditFinding'
            Prelude.<$> (x Prelude..:? "severity")
            Prelude.<*> (x Prelude..:? "findingId")
            Prelude.<*> (x Prelude..:? "taskId")
            Prelude.<*> (x Prelude..:? "reasonForNonComplianceCode")
            Prelude.<*> (x Prelude..:? "reasonForNonCompliance")
            Prelude.<*> (x Prelude..:? "isSuppressed")
            Prelude.<*> (x Prelude..:? "checkName")
            Prelude.<*> ( x Prelude..:? "relatedResources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "findingTime")
            Prelude.<*> (x Prelude..:? "taskStartTime")
            Prelude.<*> (x Prelude..:? "nonCompliantResource")
      )

instance Prelude.Hashable AuditFinding

instance Prelude.NFData AuditFinding
