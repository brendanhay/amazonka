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
-- Module      : Network.AWS.IoT.Types.AuditCheckDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AuditCheckRunStatus
import qualified Network.AWS.Lens as Lens

-- | Information about the audit check.
--
-- /See:/ 'newAuditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { -- | True if the check is complete and found all resources compliant.
    checkCompliant :: Core.Maybe Core.Bool,
    -- | The message associated with any error encountered when this check is
    -- performed during this audit.
    message :: Core.Maybe Core.Text,
    -- | Describes how many of the non-compliant resources created during the
    -- evaluation of an audit check were marked as suppressed.
    suppressedNonCompliantResourcesCount :: Core.Maybe Core.Integer,
    -- | The completion status of this check. One of \"IN_PROGRESS\",
    -- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
    -- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
    checkRunStatus :: Core.Maybe AuditCheckRunStatus,
    -- | The number of resources on which the check was performed.
    totalResourcesCount :: Core.Maybe Core.Integer,
    -- | The code of any error encountered when this check is performed during
    -- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
    -- \"AUDIT_CHECK_DISABLED\".
    errorCode :: Core.Maybe Core.Text,
    -- | The number of resources that were found noncompliant during the check.
    nonCompliantResourcesCount :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuditCheckDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkCompliant', 'auditCheckDetails_checkCompliant' - True if the check is complete and found all resources compliant.
--
-- 'message', 'auditCheckDetails_message' - The message associated with any error encountered when this check is
-- performed during this audit.
--
-- 'suppressedNonCompliantResourcesCount', 'auditCheckDetails_suppressedNonCompliantResourcesCount' - Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
--
-- 'checkRunStatus', 'auditCheckDetails_checkRunStatus' - The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
--
-- 'totalResourcesCount', 'auditCheckDetails_totalResourcesCount' - The number of resources on which the check was performed.
--
-- 'errorCode', 'auditCheckDetails_errorCode' - The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
--
-- 'nonCompliantResourcesCount', 'auditCheckDetails_nonCompliantResourcesCount' - The number of resources that were found noncompliant during the check.
newAuditCheckDetails ::
  AuditCheckDetails
newAuditCheckDetails =
  AuditCheckDetails'
    { checkCompliant = Core.Nothing,
      message = Core.Nothing,
      suppressedNonCompliantResourcesCount = Core.Nothing,
      checkRunStatus = Core.Nothing,
      totalResourcesCount = Core.Nothing,
      errorCode = Core.Nothing,
      nonCompliantResourcesCount = Core.Nothing
    }

-- | True if the check is complete and found all resources compliant.
auditCheckDetails_checkCompliant :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Bool)
auditCheckDetails_checkCompliant = Lens.lens (\AuditCheckDetails' {checkCompliant} -> checkCompliant) (\s@AuditCheckDetails' {} a -> s {checkCompliant = a} :: AuditCheckDetails)

-- | The message associated with any error encountered when this check is
-- performed during this audit.
auditCheckDetails_message :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Text)
auditCheckDetails_message = Lens.lens (\AuditCheckDetails' {message} -> message) (\s@AuditCheckDetails' {} a -> s {message = a} :: AuditCheckDetails)

-- | Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
auditCheckDetails_suppressedNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Integer)
auditCheckDetails_suppressedNonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {suppressedNonCompliantResourcesCount} -> suppressedNonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {suppressedNonCompliantResourcesCount = a} :: AuditCheckDetails)

-- | The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
auditCheckDetails_checkRunStatus :: Lens.Lens' AuditCheckDetails (Core.Maybe AuditCheckRunStatus)
auditCheckDetails_checkRunStatus = Lens.lens (\AuditCheckDetails' {checkRunStatus} -> checkRunStatus) (\s@AuditCheckDetails' {} a -> s {checkRunStatus = a} :: AuditCheckDetails)

-- | The number of resources on which the check was performed.
auditCheckDetails_totalResourcesCount :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Integer)
auditCheckDetails_totalResourcesCount = Lens.lens (\AuditCheckDetails' {totalResourcesCount} -> totalResourcesCount) (\s@AuditCheckDetails' {} a -> s {totalResourcesCount = a} :: AuditCheckDetails)

-- | The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
auditCheckDetails_errorCode :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Text)
auditCheckDetails_errorCode = Lens.lens (\AuditCheckDetails' {errorCode} -> errorCode) (\s@AuditCheckDetails' {} a -> s {errorCode = a} :: AuditCheckDetails)

-- | The number of resources that were found noncompliant during the check.
auditCheckDetails_nonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Integer)
auditCheckDetails_nonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {nonCompliantResourcesCount} -> nonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {nonCompliantResourcesCount = a} :: AuditCheckDetails)

instance Core.FromJSON AuditCheckDetails where
  parseJSON =
    Core.withObject
      "AuditCheckDetails"
      ( \x ->
          AuditCheckDetails'
            Core.<$> (x Core..:? "checkCompliant")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "suppressedNonCompliantResourcesCount")
            Core.<*> (x Core..:? "checkRunStatus")
            Core.<*> (x Core..:? "totalResourcesCount")
            Core.<*> (x Core..:? "errorCode")
            Core.<*> (x Core..:? "nonCompliantResourcesCount")
      )

instance Core.Hashable AuditCheckDetails

instance Core.NFData AuditCheckDetails
