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
-- Module      : Amazonka.IoT.Types.AuditCheckDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditCheckDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AuditCheckRunStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the audit check.
--
-- /See:/ 'newAuditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { -- | True if the check is complete and found all resources compliant.
    checkCompliant :: Prelude.Maybe Prelude.Bool,
    -- | The completion status of this check. One of \"IN_PROGRESS\",
    -- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
    -- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
    checkRunStatus :: Prelude.Maybe AuditCheckRunStatus,
    -- | The code of any error encountered when this check is performed during
    -- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
    -- \"AUDIT_CHECK_DISABLED\".
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The message associated with any error encountered when this check is
    -- performed during this audit.
    message :: Prelude.Maybe Prelude.Text,
    -- | The number of resources that were found noncompliant during the check.
    nonCompliantResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | Describes how many of the non-compliant resources created during the
    -- evaluation of an audit check were marked as suppressed.
    suppressedNonCompliantResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of resources on which the check was performed.
    totalResourcesCount :: Prelude.Maybe Prelude.Integer
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
-- 'checkCompliant', 'auditCheckDetails_checkCompliant' - True if the check is complete and found all resources compliant.
--
-- 'checkRunStatus', 'auditCheckDetails_checkRunStatus' - The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
--
-- 'errorCode', 'auditCheckDetails_errorCode' - The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
--
-- 'message', 'auditCheckDetails_message' - The message associated with any error encountered when this check is
-- performed during this audit.
--
-- 'nonCompliantResourcesCount', 'auditCheckDetails_nonCompliantResourcesCount' - The number of resources that were found noncompliant during the check.
--
-- 'suppressedNonCompliantResourcesCount', 'auditCheckDetails_suppressedNonCompliantResourcesCount' - Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
--
-- 'totalResourcesCount', 'auditCheckDetails_totalResourcesCount' - The number of resources on which the check was performed.
newAuditCheckDetails ::
  AuditCheckDetails
newAuditCheckDetails =
  AuditCheckDetails'
    { checkCompliant =
        Prelude.Nothing,
      checkRunStatus = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      message = Prelude.Nothing,
      nonCompliantResourcesCount = Prelude.Nothing,
      suppressedNonCompliantResourcesCount =
        Prelude.Nothing,
      totalResourcesCount = Prelude.Nothing
    }

-- | True if the check is complete and found all resources compliant.
auditCheckDetails_checkCompliant :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Bool)
auditCheckDetails_checkCompliant = Lens.lens (\AuditCheckDetails' {checkCompliant} -> checkCompliant) (\s@AuditCheckDetails' {} a -> s {checkCompliant = a} :: AuditCheckDetails)

-- | The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
auditCheckDetails_checkRunStatus :: Lens.Lens' AuditCheckDetails (Prelude.Maybe AuditCheckRunStatus)
auditCheckDetails_checkRunStatus = Lens.lens (\AuditCheckDetails' {checkRunStatus} -> checkRunStatus) (\s@AuditCheckDetails' {} a -> s {checkRunStatus = a} :: AuditCheckDetails)

-- | The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
auditCheckDetails_errorCode :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Text)
auditCheckDetails_errorCode = Lens.lens (\AuditCheckDetails' {errorCode} -> errorCode) (\s@AuditCheckDetails' {} a -> s {errorCode = a} :: AuditCheckDetails)

-- | The message associated with any error encountered when this check is
-- performed during this audit.
auditCheckDetails_message :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Text)
auditCheckDetails_message = Lens.lens (\AuditCheckDetails' {message} -> message) (\s@AuditCheckDetails' {} a -> s {message = a} :: AuditCheckDetails)

-- | The number of resources that were found noncompliant during the check.
auditCheckDetails_nonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_nonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {nonCompliantResourcesCount} -> nonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {nonCompliantResourcesCount = a} :: AuditCheckDetails)

-- | Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
auditCheckDetails_suppressedNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_suppressedNonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {suppressedNonCompliantResourcesCount} -> suppressedNonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {suppressedNonCompliantResourcesCount = a} :: AuditCheckDetails)

-- | The number of resources on which the check was performed.
auditCheckDetails_totalResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_totalResourcesCount = Lens.lens (\AuditCheckDetails' {totalResourcesCount} -> totalResourcesCount) (\s@AuditCheckDetails' {} a -> s {totalResourcesCount = a} :: AuditCheckDetails)

instance Data.FromJSON AuditCheckDetails where
  parseJSON =
    Data.withObject
      "AuditCheckDetails"
      ( \x ->
          AuditCheckDetails'
            Prelude.<$> (x Data..:? "checkCompliant")
            Prelude.<*> (x Data..:? "checkRunStatus")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "nonCompliantResourcesCount")
            Prelude.<*> (x Data..:? "suppressedNonCompliantResourcesCount")
            Prelude.<*> (x Data..:? "totalResourcesCount")
      )

instance Prelude.Hashable AuditCheckDetails where
  hashWithSalt _salt AuditCheckDetails' {..} =
    _salt
      `Prelude.hashWithSalt` checkCompliant
      `Prelude.hashWithSalt` checkRunStatus
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` nonCompliantResourcesCount
      `Prelude.hashWithSalt` suppressedNonCompliantResourcesCount
      `Prelude.hashWithSalt` totalResourcesCount

instance Prelude.NFData AuditCheckDetails where
  rnf AuditCheckDetails' {..} =
    Prelude.rnf checkCompliant
      `Prelude.seq` Prelude.rnf checkRunStatus
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf nonCompliantResourcesCount
      `Prelude.seq` Prelude.rnf suppressedNonCompliantResourcesCount
      `Prelude.seq` Prelude.rnf totalResourcesCount
