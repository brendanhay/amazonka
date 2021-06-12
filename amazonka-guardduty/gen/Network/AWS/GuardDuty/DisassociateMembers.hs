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
-- Module      : Network.AWS.GuardDuty.DisassociateMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates GuardDuty member accounts (to the current GuardDuty
-- administrator account) specified by the account IDs.
module Network.AWS.GuardDuty.DisassociateMembers
  ( -- * Creating a Request
    DisassociateMembers (..),
    newDisassociateMembers,

    -- * Request Lenses
    disassociateMembers_detectorId,
    disassociateMembers_accountIds,

    -- * Destructuring the Response
    DisassociateMembersResponse (..),
    newDisassociateMembersResponse,

    -- * Response Lenses
    disassociateMembersResponse_httpStatus,
    disassociateMembersResponse_unprocessedAccounts,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you
    -- want to disassociate from the administrator account.
    detectorId :: Core.Text,
    -- | A list of account IDs of the GuardDuty member accounts that you want to
    -- disassociate from the administrator account.
    accountIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'disassociateMembers_detectorId' - The unique ID of the detector of the GuardDuty account whose members you
-- want to disassociate from the administrator account.
--
-- 'accountIds', 'disassociateMembers_accountIds' - A list of account IDs of the GuardDuty member accounts that you want to
-- disassociate from the administrator account.
newDisassociateMembers ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'accountIds'
  Core.NonEmpty Core.Text ->
  DisassociateMembers
newDisassociateMembers pDetectorId_ pAccountIds_ =
  DisassociateMembers'
    { detectorId = pDetectorId_,
      accountIds = Lens._Coerce Lens.# pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you
-- want to disassociate from the administrator account.
disassociateMembers_detectorId :: Lens.Lens' DisassociateMembers Core.Text
disassociateMembers_detectorId = Lens.lens (\DisassociateMembers' {detectorId} -> detectorId) (\s@DisassociateMembers' {} a -> s {detectorId = a} :: DisassociateMembers)

-- | A list of account IDs of the GuardDuty member accounts that you want to
-- disassociate from the administrator account.
disassociateMembers_accountIds :: Lens.Lens' DisassociateMembers (Core.NonEmpty Core.Text)
disassociateMembers_accountIds = Lens.lens (\DisassociateMembers' {accountIds} -> accountIds) (\s@DisassociateMembers' {} a -> s {accountIds = a} :: DisassociateMembers) Core.. Lens._Coerce

instance Core.AWSRequest DisassociateMembers where
  type
    AWSResponse DisassociateMembers =
      DisassociateMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateMembersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "unprocessedAccounts"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable DisassociateMembers

instance Core.NFData DisassociateMembers

instance Core.ToHeaders DisassociateMembers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateMembers where
  toJSON DisassociateMembers' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("accountIds" Core..= accountIds)]
      )

instance Core.ToPath DisassociateMembers where
  toPath DisassociateMembers' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/member/disassociate"
      ]

instance Core.ToQuery DisassociateMembers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateMembersResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'disassociateMembersResponse_unprocessedAccounts' - A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
newDisassociateMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateMembersResponse
newDisassociateMembersResponse pHttpStatus_ =
  DisassociateMembersResponse'
    { httpStatus =
        pHttpStatus_,
      unprocessedAccounts = Core.mempty
    }

-- | The response's http status code.
disassociateMembersResponse_httpStatus :: Lens.Lens' DisassociateMembersResponse Core.Int
disassociateMembersResponse_httpStatus = Lens.lens (\DisassociateMembersResponse' {httpStatus} -> httpStatus) (\s@DisassociateMembersResponse' {} a -> s {httpStatus = a} :: DisassociateMembersResponse)

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
disassociateMembersResponse_unprocessedAccounts :: Lens.Lens' DisassociateMembersResponse [UnprocessedAccount]
disassociateMembersResponse_unprocessedAccounts = Lens.lens (\DisassociateMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DisassociateMembersResponse' {} a -> s {unprocessedAccounts = a} :: DisassociateMembersResponse) Core.. Lens._Coerce

instance Core.NFData DisassociateMembersResponse
