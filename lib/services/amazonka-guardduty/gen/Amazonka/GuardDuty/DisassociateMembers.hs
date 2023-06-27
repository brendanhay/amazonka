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
-- Module      : Amazonka.GuardDuty.DisassociateMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates GuardDuty member accounts (from the current administrator
-- account) specified by the account IDs.
--
-- When you disassociate an invited member from a GuardDuty delegated
-- administrator, the member account details obtained from the
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_CreateMembers.html CreateMembers>
-- API, including the associated email addresses, are retained. This is
-- done so that the delegated administrator can invoke the
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_InviteMembers.html InviteMembers>
-- API without the need to invoke the CreateMembers API again. To remove
-- the details associated with a member account, the delegated
-- administrator must invoke the
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_DeleteMembers.html DeleteMembers>
-- API.
--
-- With @autoEnableOrganizationMembers@ configuration for your organization
-- set to @ALL@, you\'ll receive an error if you attempt to disassociate a
-- member account before removing them from your Amazon Web Services
-- organization.
module Amazonka.GuardDuty.DisassociateMembers
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you
    -- want to disassociate from the administrator account.
    detectorId :: Prelude.Text,
    -- | A list of account IDs of the GuardDuty member accounts that you want to
    -- disassociate from the administrator account.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateMembers
newDisassociateMembers pDetectorId_ pAccountIds_ =
  DisassociateMembers'
    { detectorId = pDetectorId_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you
-- want to disassociate from the administrator account.
disassociateMembers_detectorId :: Lens.Lens' DisassociateMembers Prelude.Text
disassociateMembers_detectorId = Lens.lens (\DisassociateMembers' {detectorId} -> detectorId) (\s@DisassociateMembers' {} a -> s {detectorId = a} :: DisassociateMembers)

-- | A list of account IDs of the GuardDuty member accounts that you want to
-- disassociate from the administrator account.
disassociateMembers_accountIds :: Lens.Lens' DisassociateMembers (Prelude.NonEmpty Prelude.Text)
disassociateMembers_accountIds = Lens.lens (\DisassociateMembers' {accountIds} -> accountIds) (\s@DisassociateMembers' {} a -> s {accountIds = a} :: DisassociateMembers) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateMembers where
  type
    AWSResponse DisassociateMembers =
      DisassociateMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DisassociateMembers where
  hashWithSalt _salt DisassociateMembers' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData DisassociateMembers where
  rnf DisassociateMembers' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf accountIds

instance Data.ToHeaders DisassociateMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateMembers where
  toJSON DisassociateMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Data..= accountIds)]
      )

instance Data.ToPath DisassociateMembers where
  toPath DisassociateMembers' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/member/disassociate"
      ]

instance Data.ToQuery DisassociateMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateMembersResponse
newDisassociateMembersResponse pHttpStatus_ =
  DisassociateMembersResponse'
    { httpStatus =
        pHttpStatus_,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
disassociateMembersResponse_httpStatus :: Lens.Lens' DisassociateMembersResponse Prelude.Int
disassociateMembersResponse_httpStatus = Lens.lens (\DisassociateMembersResponse' {httpStatus} -> httpStatus) (\s@DisassociateMembersResponse' {} a -> s {httpStatus = a} :: DisassociateMembersResponse)

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
disassociateMembersResponse_unprocessedAccounts :: Lens.Lens' DisassociateMembersResponse [UnprocessedAccount]
disassociateMembersResponse_unprocessedAccounts = Lens.lens (\DisassociateMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DisassociateMembersResponse' {} a -> s {unprocessedAccounts = a} :: DisassociateMembersResponse) Prelude.. Lens.coerced

instance Prelude.NFData DisassociateMembersResponse where
  rnf DisassociateMembersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf unprocessedAccounts
