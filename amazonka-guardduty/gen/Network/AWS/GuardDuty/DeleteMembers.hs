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
-- Module      : Network.AWS.GuardDuty.DeleteMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes GuardDuty member accounts (to the current GuardDuty
-- administrator account) specified by the account IDs.
module Network.AWS.GuardDuty.DeleteMembers
  ( -- * Creating a Request
    DeleteMembers (..),
    newDeleteMembers,

    -- * Request Lenses
    deleteMembers_detectorId,
    deleteMembers_accountIds,

    -- * Destructuring the Response
    DeleteMembersResponse (..),
    newDeleteMembersResponse,

    -- * Response Lenses
    deleteMembersResponse_httpStatus,
    deleteMembersResponse_unprocessedAccounts,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you
    -- want to delete.
    detectorId :: Core.Text,
    -- | A list of account IDs of the GuardDuty member accounts that you want to
    -- delete.
    accountIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deleteMembers_detectorId' - The unique ID of the detector of the GuardDuty account whose members you
-- want to delete.
--
-- 'accountIds', 'deleteMembers_accountIds' - A list of account IDs of the GuardDuty member accounts that you want to
-- delete.
newDeleteMembers ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'accountIds'
  Core.NonEmpty Core.Text ->
  DeleteMembers
newDeleteMembers pDetectorId_ pAccountIds_ =
  DeleteMembers'
    { detectorId = pDetectorId_,
      accountIds = Lens._Coerce Lens.# pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you
-- want to delete.
deleteMembers_detectorId :: Lens.Lens' DeleteMembers Core.Text
deleteMembers_detectorId = Lens.lens (\DeleteMembers' {detectorId} -> detectorId) (\s@DeleteMembers' {} a -> s {detectorId = a} :: DeleteMembers)

-- | A list of account IDs of the GuardDuty member accounts that you want to
-- delete.
deleteMembers_accountIds :: Lens.Lens' DeleteMembers (Core.NonEmpty Core.Text)
deleteMembers_accountIds = Lens.lens (\DeleteMembers' {accountIds} -> accountIds) (\s@DeleteMembers' {} a -> s {accountIds = a} :: DeleteMembers) Core.. Lens._Coerce

instance Core.AWSRequest DeleteMembers where
  type
    AWSResponse DeleteMembers =
      DeleteMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMembersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "unprocessedAccounts"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable DeleteMembers

instance Core.NFData DeleteMembers

instance Core.ToHeaders DeleteMembers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteMembers where
  toJSON DeleteMembers' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("accountIds" Core..= accountIds)]
      )

instance Core.ToPath DeleteMembers where
  toPath DeleteMembers' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/member/delete"
      ]

instance Core.ToQuery DeleteMembers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The accounts that could not be processed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMembersResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'deleteMembersResponse_unprocessedAccounts' - The accounts that could not be processed.
newDeleteMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteMembersResponse
newDeleteMembersResponse pHttpStatus_ =
  DeleteMembersResponse'
    { httpStatus = pHttpStatus_,
      unprocessedAccounts = Core.mempty
    }

-- | The response's http status code.
deleteMembersResponse_httpStatus :: Lens.Lens' DeleteMembersResponse Core.Int
deleteMembersResponse_httpStatus = Lens.lens (\DeleteMembersResponse' {httpStatus} -> httpStatus) (\s@DeleteMembersResponse' {} a -> s {httpStatus = a} :: DeleteMembersResponse)

-- | The accounts that could not be processed.
deleteMembersResponse_unprocessedAccounts :: Lens.Lens' DeleteMembersResponse [UnprocessedAccount]
deleteMembersResponse_unprocessedAccounts = Lens.lens (\DeleteMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DeleteMembersResponse' {} a -> s {unprocessedAccounts = a} :: DeleteMembersResponse) Core.. Lens._Coerce

instance Core.NFData DeleteMembersResponse
