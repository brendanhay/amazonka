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
-- Module      : Network.AWS.IoT.Types.AuditCheckDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckDetails where

import Network.AWS.IoT.Types.AuditCheckRunStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the audit check.
--
-- /See:/ 'newAuditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { -- | True if the check is complete and found all resources compliant.
    checkCompliant :: Prelude.Maybe Prelude.Bool,
    -- | The message associated with any error encountered when this check is
    -- performed during this audit.
    message :: Prelude.Maybe Prelude.Text,
    -- | Describes how many of the non-compliant resources created during the
    -- evaluation of an audit check were marked as suppressed.
    suppressedNonCompliantResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | The completion status of this check. One of \"IN_PROGRESS\",
    -- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
    -- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
    checkRunStatus :: Prelude.Maybe AuditCheckRunStatus,
    -- | The number of resources on which the check was performed.
    totalResourcesCount :: Prelude.Maybe Prelude.Integer,
    -- | The code of any error encountered when this check is performed during
    -- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
    -- \"AUDIT_CHECK_DISABLED\".
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The number of resources that were found noncompliant during the check.
    nonCompliantResourcesCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { checkCompliant =
        Prelude.Nothing,
      message = Prelude.Nothing,
      suppressedNonCompliantResourcesCount =
        Prelude.Nothing,
      checkRunStatus = Prelude.Nothing,
      totalResourcesCount = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      nonCompliantResourcesCount = Prelude.Nothing
    }

-- | True if the check is complete and found all resources compliant.
auditCheckDetails_checkCompliant :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Bool)
auditCheckDetails_checkCompliant = Lens.lens (\AuditCheckDetails' {checkCompliant} -> checkCompliant) (\s@AuditCheckDetails' {} a -> s {checkCompliant = a} :: AuditCheckDetails)

-- | The message associated with any error encountered when this check is
-- performed during this audit.
auditCheckDetails_message :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Text)
auditCheckDetails_message = Lens.lens (\AuditCheckDetails' {message} -> message) (\s@AuditCheckDetails' {} a -> s {message = a} :: AuditCheckDetails)

-- | Describes how many of the non-compliant resources created during the
-- evaluation of an audit check were marked as suppressed.
auditCheckDetails_suppressedNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_suppressedNonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {suppressedNonCompliantResourcesCount} -> suppressedNonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {suppressedNonCompliantResourcesCount = a} :: AuditCheckDetails)

-- | The completion status of this check. One of \"IN_PROGRESS\",
-- \"WAITING_FOR_DATA_COLLECTION\", \"CANCELED\", \"COMPLETED_COMPLIANT\",
-- \"COMPLETED_NON_COMPLIANT\", or \"FAILED\".
auditCheckDetails_checkRunStatus :: Lens.Lens' AuditCheckDetails (Prelude.Maybe AuditCheckRunStatus)
auditCheckDetails_checkRunStatus = Lens.lens (\AuditCheckDetails' {checkRunStatus} -> checkRunStatus) (\s@AuditCheckDetails' {} a -> s {checkRunStatus = a} :: AuditCheckDetails)

-- | The number of resources on which the check was performed.
auditCheckDetails_totalResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_totalResourcesCount = Lens.lens (\AuditCheckDetails' {totalResourcesCount} -> totalResourcesCount) (\s@AuditCheckDetails' {} a -> s {totalResourcesCount = a} :: AuditCheckDetails)

-- | The code of any error encountered when this check is performed during
-- this audit. One of \"INSUFFICIENT_PERMISSIONS\" or
-- \"AUDIT_CHECK_DISABLED\".
auditCheckDetails_errorCode :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Text)
auditCheckDetails_errorCode = Lens.lens (\AuditCheckDetails' {errorCode} -> errorCode) (\s@AuditCheckDetails' {} a -> s {errorCode = a} :: AuditCheckDetails)

-- | The number of resources that were found noncompliant during the check.
auditCheckDetails_nonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Prelude.Maybe Prelude.Integer)
auditCheckDetails_nonCompliantResourcesCount = Lens.lens (\AuditCheckDetails' {nonCompliantResourcesCount} -> nonCompliantResourcesCount) (\s@AuditCheckDetails' {} a -> s {nonCompliantResourcesCount = a} :: AuditCheckDetails)

instance Prelude.FromJSON AuditCheckDetails where
  parseJSON =
    Prelude.withObject
      "AuditCheckDetails"
      ( \x ->
          AuditCheckDetails'
            Prelude.<$> (x Prelude..:? "checkCompliant")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> ( x
                            Prelude..:? "suppressedNonCompliantResourcesCount"
                        )
            Prelude.<*> (x Prelude..:? "checkRunStatus")
            Prelude.<*> (x Prelude..:? "totalResourcesCount")
            Prelude.<*> (x Prelude..:? "errorCode")
            Prelude.<*> (x Prelude..:? "nonCompliantResourcesCount")
      )

instance Prelude.Hashable AuditCheckDetails

instance Prelude.NFData AuditCheckDetails
