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
import qualified Network.AWS.Prelude as Prelude

-- | Information about the audit check.
--
-- /See:/ 'newAuditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { -- | Describes how many of the non-compliant resources created during the
    -- evaluation of an audit check were marked as suppressed.
    suppressedNonCompliantResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of resources on which the check was performed.
    totalResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | True if the check is complete and found all resources compliant.
    checkCompliant :: Prelude.Maybe Prelude.Bool,
    -- | The number of resources that were found noncompliant during the check.
    nonCompliantResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | The code of any error encountered when this check is performed during
    -- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
    -- \"AUDIT_CHECK_DISABLED\".
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The message associated with any error encountered when this check is
    -- performed during this audit.
    message :: Prelude.Maybe Prelude.Text,
    -- | The completion status of this check. One of \"IN_PROGRESS\",
    -- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
    -- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
    checkRunStatus :: Prelude.Maybe AuditCheckRunStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditCheckDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressedNonCompliantResourcesCount', 'auditCheckDetails_suppressedNonCompliantResourcesCount' - Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
--
-- 'totalResourcesCount', 'auditCheckDetails_totalResourcesCount' - The number of resources on which the check was performed.
--
-- 'checkCompliant', 'auditCheckDetails_checkCompliant' - True if the check is complete and found all resources compliant.
--
-- 'nonCompliantResourcesCount', 'auditCheckDetails_nonCompliantResourcesCount' - The number of resources that were found noncompliant during the check.
--
-- 'errorCode', 'auditCheckDetails_errorCode' - The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
--
-- 'message', 'auditCheckDetails_message' - The message associated with any error encountered when this check is
-- performed during this audit.
--
-- 'checkRunStatus', 'auditCheckDetails_checkRunStatus' - The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
newAuditCheckDetails ::
  AuditCheckDetails
newAuditCheckDetails =
  AuditCheckDetails'
    { suppressedNonCompliantResourcesCount =
        Prelude.Nothing,
      totalResourcesCount = Prelude.Nothing,
      checkCompliant = Prelude.Nothing,
      nonCompliantResourcesCount = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      message = Prelude.Nothing,
      checkRunStatus = Prelude.Nothing
    }

-- | Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
auditCheckDetails_suppressedNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_suppressedNonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {suppressedNonCompliantResourcesCount} -> suppressedNonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {suppressedNonCompliantResourcesCount = a} :: AuditCheckDetails)

-- | The number of resources on which the check was performed.
auditCheckDetails_totalResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_totalResourcesCount = Lens.lens (\AuditCheckDetails' {totalResourcesCount} -> totalResourcesCount) (\s@AuditCheckDetails' {} a -> s {totalResourcesCount = a} :: AuditCheckDetails)

-- | True if the check is complete and found all resources compliant.
auditCheckDetails_checkCompliant :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Bool)
auditCheckDetails_checkCompliant = Lens.lens (\AuditCheckDetails' {checkCompliant} -> checkCompliant) (\s@AuditCheckDetails' {} a -> s {checkCompliant = a} :: AuditCheckDetails)

-- | The number of resources that were found noncompliant during the check.
auditCheckDetails_nonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_nonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {nonCompliantResourcesCount} -> nonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {nonCompliantResourcesCount = a} :: AuditCheckDetails)

-- | The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
auditCheckDetails_errorCode :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Text)
auditCheckDetails_errorCode = Lens.lens (\AuditCheckDetails' {errorCode} -> errorCode) (\s@AuditCheckDetails' {} a -> s {errorCode = a} :: AuditCheckDetails)

-- | The message associated with any error encountered when this check is
-- performed during this audit.
auditCheckDetails_message :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Text)
auditCheckDetails_message = Lens.lens (\AuditCheckDetails' {message} -> message) (\s@AuditCheckDetails' {} a -> s {message = a} :: AuditCheckDetails)

-- | The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
auditCheckDetails_checkRunStatus :: Lens.Lens' AuditCheckDetails (Prelude.Maybe AuditCheckRunStatus)
auditCheckDetails_checkRunStatus = Lens.lens (\AuditCheckDetails' {checkRunStatus} -> checkRunStatus) (\s@AuditCheckDetails' {} a -> s {checkRunStatus = a} :: AuditCheckDetails)

instance Core.FromJSON AuditCheckDetails where
  parseJSON =
    Core.withObject
      "AuditCheckDetails"
      ( \x ->
          AuditCheckDetails'
            Prelude.<$> (x Core..:? "suppressedNonCompliantResourcesCount")
            Prelude.<*> (x Core..:? "totalResourcesCount")
            Prelude.<*> (x Core..:? "checkCompliant")
            Prelude.<*> (x Core..:? "nonCompliantResourcesCount")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "message")
            Prelude.<*> (x Core..:? "checkRunStatus")
      )

instance Prelude.Hashable AuditCheckDetails

instance Prelude.NFData AuditCheckDetails
