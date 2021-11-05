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
-- Module      : Amazonka.ComputeOptimizer.Types.AccountEnrollmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.AccountEnrollmentStatus where

import Amazonka.ComputeOptimizer.Types.Status
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the enrollment status of an organization\'s member accounts in
-- Compute Optimizer.
--
-- /See:/ 'newAccountEnrollmentStatus' smart constructor.
data AccountEnrollmentStatus = AccountEnrollmentStatus'
  { -- | The account enrollment status.
    status :: Prelude.Maybe Status,
    -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The reason for the account enrollment status.
    --
    -- For example, an account might show a status of @Pending@ because member
    -- accounts of an organization require more time to be enrolled in the
    -- service.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp, in seconds, of when the account enrollment
    -- status was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountEnrollmentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'accountEnrollmentStatus_status' - The account enrollment status.
--
-- 'accountId', 'accountEnrollmentStatus_accountId' - The Amazon Web Services account ID.
--
-- 'statusReason', 'accountEnrollmentStatus_statusReason' - The reason for the account enrollment status.
--
-- For example, an account might show a status of @Pending@ because member
-- accounts of an organization require more time to be enrolled in the
-- service.
--
-- 'lastUpdatedTimestamp', 'accountEnrollmentStatus_lastUpdatedTimestamp' - The Unix epoch timestamp, in seconds, of when the account enrollment
-- status was last updated.
newAccountEnrollmentStatus ::
  AccountEnrollmentStatus
newAccountEnrollmentStatus =
  AccountEnrollmentStatus'
    { status = Prelude.Nothing,
      accountId = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing
    }

-- | The account enrollment status.
accountEnrollmentStatus_status :: Lens.Lens' AccountEnrollmentStatus (Prelude.Maybe Status)
accountEnrollmentStatus_status = Lens.lens (\AccountEnrollmentStatus' {status} -> status) (\s@AccountEnrollmentStatus' {} a -> s {status = a} :: AccountEnrollmentStatus)

-- | The Amazon Web Services account ID.
accountEnrollmentStatus_accountId :: Lens.Lens' AccountEnrollmentStatus (Prelude.Maybe Prelude.Text)
accountEnrollmentStatus_accountId = Lens.lens (\AccountEnrollmentStatus' {accountId} -> accountId) (\s@AccountEnrollmentStatus' {} a -> s {accountId = a} :: AccountEnrollmentStatus)

-- | The reason for the account enrollment status.
--
-- For example, an account might show a status of @Pending@ because member
-- accounts of an organization require more time to be enrolled in the
-- service.
accountEnrollmentStatus_statusReason :: Lens.Lens' AccountEnrollmentStatus (Prelude.Maybe Prelude.Text)
accountEnrollmentStatus_statusReason = Lens.lens (\AccountEnrollmentStatus' {statusReason} -> statusReason) (\s@AccountEnrollmentStatus' {} a -> s {statusReason = a} :: AccountEnrollmentStatus)

-- | The Unix epoch timestamp, in seconds, of when the account enrollment
-- status was last updated.
accountEnrollmentStatus_lastUpdatedTimestamp :: Lens.Lens' AccountEnrollmentStatus (Prelude.Maybe Prelude.UTCTime)
accountEnrollmentStatus_lastUpdatedTimestamp = Lens.lens (\AccountEnrollmentStatus' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@AccountEnrollmentStatus' {} a -> s {lastUpdatedTimestamp = a} :: AccountEnrollmentStatus) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AccountEnrollmentStatus where
  parseJSON =
    Core.withObject
      "AccountEnrollmentStatus"
      ( \x ->
          AccountEnrollmentStatus'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "lastUpdatedTimestamp")
      )

instance Prelude.Hashable AccountEnrollmentStatus

instance Prelude.NFData AccountEnrollmentStatus
