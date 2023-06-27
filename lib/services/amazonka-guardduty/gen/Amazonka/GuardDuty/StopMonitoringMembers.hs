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
-- Module      : Amazonka.GuardDuty.StopMonitoringMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops GuardDuty monitoring for the specified member accounts. Use the
-- @StartMonitoringMembers@ operation to restart monitoring for those
-- accounts.
--
-- With @autoEnableOrganizationMembers@ configuration for your organization
-- set to @ALL@, you\'ll receive an error if you attempt to stop monitoring
-- the member accounts in your organization.
module Amazonka.GuardDuty.StopMonitoringMembers
  ( -- * Creating a Request
    StopMonitoringMembers (..),
    newStopMonitoringMembers,

    -- * Request Lenses
    stopMonitoringMembers_detectorId,
    stopMonitoringMembers_accountIds,

    -- * Destructuring the Response
    StopMonitoringMembersResponse (..),
    newStopMonitoringMembersResponse,

    -- * Response Lenses
    stopMonitoringMembersResponse_httpStatus,
    stopMonitoringMembersResponse_unprocessedAccounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopMonitoringMembers' smart constructor.
data StopMonitoringMembers = StopMonitoringMembers'
  { -- | The unique ID of the detector associated with the GuardDuty
    -- administrator account that is monitoring member accounts.
    detectorId :: Prelude.Text,
    -- | A list of account IDs for the member accounts to stop monitoring.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopMonitoringMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'stopMonitoringMembers_detectorId' - The unique ID of the detector associated with the GuardDuty
-- administrator account that is monitoring member accounts.
--
-- 'accountIds', 'stopMonitoringMembers_accountIds' - A list of account IDs for the member accounts to stop monitoring.
newStopMonitoringMembers ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  StopMonitoringMembers
newStopMonitoringMembers pDetectorId_ pAccountIds_ =
  StopMonitoringMembers'
    { detectorId = pDetectorId_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | The unique ID of the detector associated with the GuardDuty
-- administrator account that is monitoring member accounts.
stopMonitoringMembers_detectorId :: Lens.Lens' StopMonitoringMembers Prelude.Text
stopMonitoringMembers_detectorId = Lens.lens (\StopMonitoringMembers' {detectorId} -> detectorId) (\s@StopMonitoringMembers' {} a -> s {detectorId = a} :: StopMonitoringMembers)

-- | A list of account IDs for the member accounts to stop monitoring.
stopMonitoringMembers_accountIds :: Lens.Lens' StopMonitoringMembers (Prelude.NonEmpty Prelude.Text)
stopMonitoringMembers_accountIds = Lens.lens (\StopMonitoringMembers' {accountIds} -> accountIds) (\s@StopMonitoringMembers' {} a -> s {accountIds = a} :: StopMonitoringMembers) Prelude.. Lens.coerced

instance Core.AWSRequest StopMonitoringMembers where
  type
    AWSResponse StopMonitoringMembers =
      StopMonitoringMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopMonitoringMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable StopMonitoringMembers where
  hashWithSalt _salt StopMonitoringMembers' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData StopMonitoringMembers where
  rnf StopMonitoringMembers' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf accountIds

instance Data.ToHeaders StopMonitoringMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopMonitoringMembers where
  toJSON StopMonitoringMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Data..= accountIds)]
      )

instance Data.ToPath StopMonitoringMembers where
  toPath StopMonitoringMembers' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId, "/member/stop"]

instance Data.ToQuery StopMonitoringMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopMonitoringMembersResponse' smart constructor.
data StopMonitoringMembersResponse = StopMonitoringMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects that contain an accountId for each account that could
    -- not be processed, and a result string that indicates why the account was
    -- not processed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopMonitoringMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopMonitoringMembersResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'stopMonitoringMembersResponse_unprocessedAccounts' - A list of objects that contain an accountId for each account that could
-- not be processed, and a result string that indicates why the account was
-- not processed.
newStopMonitoringMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopMonitoringMembersResponse
newStopMonitoringMembersResponse pHttpStatus_ =
  StopMonitoringMembersResponse'
    { httpStatus =
        pHttpStatus_,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
stopMonitoringMembersResponse_httpStatus :: Lens.Lens' StopMonitoringMembersResponse Prelude.Int
stopMonitoringMembersResponse_httpStatus = Lens.lens (\StopMonitoringMembersResponse' {httpStatus} -> httpStatus) (\s@StopMonitoringMembersResponse' {} a -> s {httpStatus = a} :: StopMonitoringMembersResponse)

-- | A list of objects that contain an accountId for each account that could
-- not be processed, and a result string that indicates why the account was
-- not processed.
stopMonitoringMembersResponse_unprocessedAccounts :: Lens.Lens' StopMonitoringMembersResponse [UnprocessedAccount]
stopMonitoringMembersResponse_unprocessedAccounts = Lens.lens (\StopMonitoringMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@StopMonitoringMembersResponse' {} a -> s {unprocessedAccounts = a} :: StopMonitoringMembersResponse) Prelude.. Lens.coerced

instance Prelude.NFData StopMonitoringMembersResponse where
  rnf StopMonitoringMembersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf unprocessedAccounts
