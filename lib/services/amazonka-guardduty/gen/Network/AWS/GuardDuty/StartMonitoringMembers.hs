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
-- Module      : Network.AWS.GuardDuty.StartMonitoringMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on GuardDuty monitoring of the specified member accounts. Use this
-- operation to restart monitoring of accounts that you stopped monitoring
-- with the @StopMonitoringMembers@ operation.
module Network.AWS.GuardDuty.StartMonitoringMembers
  ( -- * Creating a Request
    StartMonitoringMembers (..),
    newStartMonitoringMembers,

    -- * Request Lenses
    startMonitoringMembers_detectorId,
    startMonitoringMembers_accountIds,

    -- * Destructuring the Response
    StartMonitoringMembersResponse (..),
    newStartMonitoringMembersResponse,

    -- * Response Lenses
    startMonitoringMembersResponse_httpStatus,
    startMonitoringMembersResponse_unprocessedAccounts,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartMonitoringMembers' smart constructor.
data StartMonitoringMembers = StartMonitoringMembers'
  { -- | The unique ID of the detector of the GuardDuty administrator account
    -- associated with the member accounts to monitor.
    detectorId :: Prelude.Text,
    -- | A list of account IDs of the GuardDuty member accounts to start
    -- monitoring.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMonitoringMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'startMonitoringMembers_detectorId' - The unique ID of the detector of the GuardDuty administrator account
-- associated with the member accounts to monitor.
--
-- 'accountIds', 'startMonitoringMembers_accountIds' - A list of account IDs of the GuardDuty member accounts to start
-- monitoring.
newStartMonitoringMembers ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  StartMonitoringMembers
newStartMonitoringMembers pDetectorId_ pAccountIds_ =
  StartMonitoringMembers'
    { detectorId = pDetectorId_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty administrator account
-- associated with the member accounts to monitor.
startMonitoringMembers_detectorId :: Lens.Lens' StartMonitoringMembers Prelude.Text
startMonitoringMembers_detectorId = Lens.lens (\StartMonitoringMembers' {detectorId} -> detectorId) (\s@StartMonitoringMembers' {} a -> s {detectorId = a} :: StartMonitoringMembers)

-- | A list of account IDs of the GuardDuty member accounts to start
-- monitoring.
startMonitoringMembers_accountIds :: Lens.Lens' StartMonitoringMembers (Prelude.NonEmpty Prelude.Text)
startMonitoringMembers_accountIds = Lens.lens (\StartMonitoringMembers' {accountIds} -> accountIds) (\s@StartMonitoringMembers' {} a -> s {accountIds = a} :: StartMonitoringMembers) Prelude.. Lens.coerced

instance Core.AWSRequest StartMonitoringMembers where
  type
    AWSResponse StartMonitoringMembers =
      StartMonitoringMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMonitoringMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable StartMonitoringMembers

instance Prelude.NFData StartMonitoringMembers

instance Core.ToHeaders StartMonitoringMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartMonitoringMembers where
  toJSON StartMonitoringMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Core..= accountIds)]
      )

instance Core.ToPath StartMonitoringMembers where
  toPath StartMonitoringMembers' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/member/start"]

instance Core.ToQuery StartMonitoringMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMonitoringMembersResponse' smart constructor.
data StartMonitoringMembersResponse = StartMonitoringMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMonitoringMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startMonitoringMembersResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'startMonitoringMembersResponse_unprocessedAccounts' - A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
newStartMonitoringMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMonitoringMembersResponse
newStartMonitoringMembersResponse pHttpStatus_ =
  StartMonitoringMembersResponse'
    { httpStatus =
        pHttpStatus_,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
startMonitoringMembersResponse_httpStatus :: Lens.Lens' StartMonitoringMembersResponse Prelude.Int
startMonitoringMembersResponse_httpStatus = Lens.lens (\StartMonitoringMembersResponse' {httpStatus} -> httpStatus) (\s@StartMonitoringMembersResponse' {} a -> s {httpStatus = a} :: StartMonitoringMembersResponse)

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
startMonitoringMembersResponse_unprocessedAccounts :: Lens.Lens' StartMonitoringMembersResponse [UnprocessedAccount]
startMonitoringMembersResponse_unprocessedAccounts = Lens.lens (\StartMonitoringMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@StartMonitoringMembersResponse' {} a -> s {unprocessedAccounts = a} :: StartMonitoringMembersResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    StartMonitoringMembersResponse
