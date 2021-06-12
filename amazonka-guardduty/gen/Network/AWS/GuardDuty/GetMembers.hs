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
-- Module      : Network.AWS.GuardDuty.GetMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves GuardDuty member accounts (of the current GuardDuty
-- administrator account) specified by the account IDs.
module Network.AWS.GuardDuty.GetMembers
  ( -- * Creating a Request
    GetMembers (..),
    newGetMembers,

    -- * Request Lenses
    getMembers_detectorId,
    getMembers_accountIds,

    -- * Destructuring the Response
    GetMembersResponse (..),
    newGetMembersResponse,

    -- * Response Lenses
    getMembersResponse_httpStatus,
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMembers' smart constructor.
data GetMembers = GetMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you
    -- want to retrieve.
    detectorId :: Core.Text,
    -- | A list of account IDs of the GuardDuty member accounts that you want to
    -- describe.
    accountIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getMembers_detectorId' - The unique ID of the detector of the GuardDuty account whose members you
-- want to retrieve.
--
-- 'accountIds', 'getMembers_accountIds' - A list of account IDs of the GuardDuty member accounts that you want to
-- describe.
newGetMembers ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'accountIds'
  Core.NonEmpty Core.Text ->
  GetMembers
newGetMembers pDetectorId_ pAccountIds_ =
  GetMembers'
    { detectorId = pDetectorId_,
      accountIds = Lens._Coerce Lens.# pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you
-- want to retrieve.
getMembers_detectorId :: Lens.Lens' GetMembers Core.Text
getMembers_detectorId = Lens.lens (\GetMembers' {detectorId} -> detectorId) (\s@GetMembers' {} a -> s {detectorId = a} :: GetMembers)

-- | A list of account IDs of the GuardDuty member accounts that you want to
-- describe.
getMembers_accountIds :: Lens.Lens' GetMembers (Core.NonEmpty Core.Text)
getMembers_accountIds = Lens.lens (\GetMembers' {accountIds} -> accountIds) (\s@GetMembers' {} a -> s {accountIds = a} :: GetMembers) Core.. Lens._Coerce

instance Core.AWSRequest GetMembers where
  type AWSResponse GetMembers = GetMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMembersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "members" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "unprocessedAccounts"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable GetMembers

instance Core.NFData GetMembers

instance Core.ToHeaders GetMembers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMembers where
  toJSON GetMembers' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("accountIds" Core..= accountIds)]
      )

instance Core.ToPath GetMembers where
  toPath GetMembers' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/member/get"]

instance Core.ToQuery GetMembers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of members.
    members :: [Member],
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMembersResponse_httpStatus' - The response's http status code.
--
-- 'members', 'getMembersResponse_members' - A list of members.
--
-- 'unprocessedAccounts', 'getMembersResponse_unprocessedAccounts' - A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
newGetMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMembersResponse
newGetMembersResponse pHttpStatus_ =
  GetMembersResponse'
    { httpStatus = pHttpStatus_,
      members = Core.mempty,
      unprocessedAccounts = Core.mempty
    }

-- | The response's http status code.
getMembersResponse_httpStatus :: Lens.Lens' GetMembersResponse Core.Int
getMembersResponse_httpStatus = Lens.lens (\GetMembersResponse' {httpStatus} -> httpStatus) (\s@GetMembersResponse' {} a -> s {httpStatus = a} :: GetMembersResponse)

-- | A list of members.
getMembersResponse_members :: Lens.Lens' GetMembersResponse [Member]
getMembersResponse_members = Lens.lens (\GetMembersResponse' {members} -> members) (\s@GetMembersResponse' {} a -> s {members = a} :: GetMembersResponse) Core.. Lens._Coerce

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
getMembersResponse_unprocessedAccounts :: Lens.Lens' GetMembersResponse [UnprocessedAccount]
getMembersResponse_unprocessedAccounts = Lens.lens (\GetMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@GetMembersResponse' {} a -> s {unprocessedAccounts = a} :: GetMembersResponse) Core.. Lens._Coerce

instance Core.NFData GetMembersResponse
