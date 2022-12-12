{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComputeOptimizer.GetEnrollmentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the enrollment (opt in) status of an account to the Compute
-- Optimizer service.
--
-- If the account is the management account of an organization, this action
-- also confirms the enrollment status of member accounts of the
-- organization. Use the GetEnrollmentStatusesForOrganization action to get
-- detailed information about the enrollment status of member accounts of
-- an organization.
module Amazonka.ComputeOptimizer.GetEnrollmentStatus
  ( -- * Creating a Request
    GetEnrollmentStatus (..),
    newGetEnrollmentStatus,

    -- * Destructuring the Response
    GetEnrollmentStatusResponse (..),
    newGetEnrollmentStatusResponse,

    -- * Response Lenses
    getEnrollmentStatusResponse_lastUpdatedTimestamp,
    getEnrollmentStatusResponse_memberAccountsEnrolled,
    getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn,
    getEnrollmentStatusResponse_status,
    getEnrollmentStatusResponse_statusReason,
    getEnrollmentStatusResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnrollmentStatus' smart constructor.
data GetEnrollmentStatus = GetEnrollmentStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnrollmentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetEnrollmentStatus ::
  GetEnrollmentStatus
newGetEnrollmentStatus = GetEnrollmentStatus'

instance Core.AWSRequest GetEnrollmentStatus where
  type
    AWSResponse GetEnrollmentStatus =
      GetEnrollmentStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnrollmentStatusResponse'
            Prelude.<$> (x Data..?> "lastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "memberAccountsEnrolled")
            Prelude.<*> (x Data..?> "numberOfMemberAccountsOptedIn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEnrollmentStatus where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetEnrollmentStatus where
  rnf _ = ()

instance Data.ToHeaders GetEnrollmentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetEnrollmentStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEnrollmentStatus where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetEnrollmentStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEnrollmentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnrollmentStatusResponse' smart constructor.
data GetEnrollmentStatusResponse = GetEnrollmentStatusResponse'
  { -- | The Unix epoch timestamp, in seconds, of when the account enrollment
    -- status was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Confirms the enrollment status of member accounts of the organization,
    -- if the account is a management account of an organization.
    memberAccountsEnrolled :: Prelude.Maybe Prelude.Bool,
    -- | The count of organization member accounts that are opted in to the
    -- service, if your account is an organization management account.
    numberOfMemberAccountsOptedIn :: Prelude.Maybe Prelude.Int,
    -- | The enrollment status of the account.
    status :: Prelude.Maybe Status,
    -- | The reason for the enrollment status of the account.
    --
    -- For example, an account might show a status of @Pending@ because member
    -- accounts of an organization require more time to be enrolled in the
    -- service.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnrollmentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'getEnrollmentStatusResponse_lastUpdatedTimestamp' - The Unix epoch timestamp, in seconds, of when the account enrollment
-- status was last updated.
--
-- 'memberAccountsEnrolled', 'getEnrollmentStatusResponse_memberAccountsEnrolled' - Confirms the enrollment status of member accounts of the organization,
-- if the account is a management account of an organization.
--
-- 'numberOfMemberAccountsOptedIn', 'getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn' - The count of organization member accounts that are opted in to the
-- service, if your account is an organization management account.
--
-- 'status', 'getEnrollmentStatusResponse_status' - The enrollment status of the account.
--
-- 'statusReason', 'getEnrollmentStatusResponse_statusReason' - The reason for the enrollment status of the account.
--
-- For example, an account might show a status of @Pending@ because member
-- accounts of an organization require more time to be enrolled in the
-- service.
--
-- 'httpStatus', 'getEnrollmentStatusResponse_httpStatus' - The response's http status code.
newGetEnrollmentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEnrollmentStatusResponse
newGetEnrollmentStatusResponse pHttpStatus_ =
  GetEnrollmentStatusResponse'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      memberAccountsEnrolled = Prelude.Nothing,
      numberOfMemberAccountsOptedIn =
        Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Unix epoch timestamp, in seconds, of when the account enrollment
-- status was last updated.
getEnrollmentStatusResponse_lastUpdatedTimestamp :: Lens.Lens' GetEnrollmentStatusResponse (Prelude.Maybe Prelude.UTCTime)
getEnrollmentStatusResponse_lastUpdatedTimestamp = Lens.lens (\GetEnrollmentStatusResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetEnrollmentStatusResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetEnrollmentStatusResponse) Prelude.. Lens.mapping Data._Time

-- | Confirms the enrollment status of member accounts of the organization,
-- if the account is a management account of an organization.
getEnrollmentStatusResponse_memberAccountsEnrolled :: Lens.Lens' GetEnrollmentStatusResponse (Prelude.Maybe Prelude.Bool)
getEnrollmentStatusResponse_memberAccountsEnrolled = Lens.lens (\GetEnrollmentStatusResponse' {memberAccountsEnrolled} -> memberAccountsEnrolled) (\s@GetEnrollmentStatusResponse' {} a -> s {memberAccountsEnrolled = a} :: GetEnrollmentStatusResponse)

-- | The count of organization member accounts that are opted in to the
-- service, if your account is an organization management account.
getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn :: Lens.Lens' GetEnrollmentStatusResponse (Prelude.Maybe Prelude.Int)
getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn = Lens.lens (\GetEnrollmentStatusResponse' {numberOfMemberAccountsOptedIn} -> numberOfMemberAccountsOptedIn) (\s@GetEnrollmentStatusResponse' {} a -> s {numberOfMemberAccountsOptedIn = a} :: GetEnrollmentStatusResponse)

-- | The enrollment status of the account.
getEnrollmentStatusResponse_status :: Lens.Lens' GetEnrollmentStatusResponse (Prelude.Maybe Status)
getEnrollmentStatusResponse_status = Lens.lens (\GetEnrollmentStatusResponse' {status} -> status) (\s@GetEnrollmentStatusResponse' {} a -> s {status = a} :: GetEnrollmentStatusResponse)

-- | The reason for the enrollment status of the account.
--
-- For example, an account might show a status of @Pending@ because member
-- accounts of an organization require more time to be enrolled in the
-- service.
getEnrollmentStatusResponse_statusReason :: Lens.Lens' GetEnrollmentStatusResponse (Prelude.Maybe Prelude.Text)
getEnrollmentStatusResponse_statusReason = Lens.lens (\GetEnrollmentStatusResponse' {statusReason} -> statusReason) (\s@GetEnrollmentStatusResponse' {} a -> s {statusReason = a} :: GetEnrollmentStatusResponse)

-- | The response's http status code.
getEnrollmentStatusResponse_httpStatus :: Lens.Lens' GetEnrollmentStatusResponse Prelude.Int
getEnrollmentStatusResponse_httpStatus = Lens.lens (\GetEnrollmentStatusResponse' {httpStatus} -> httpStatus) (\s@GetEnrollmentStatusResponse' {} a -> s {httpStatus = a} :: GetEnrollmentStatusResponse)

instance Prelude.NFData GetEnrollmentStatusResponse where
  rnf GetEnrollmentStatusResponse' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf memberAccountsEnrolled
      `Prelude.seq` Prelude.rnf numberOfMemberAccountsOptedIn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
