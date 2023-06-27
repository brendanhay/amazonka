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
-- Module      : Amazonka.GuardDuty.InviteMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invites Amazon Web Services accounts to become members of an
-- organization administered by the Amazon Web Services account that
-- invokes this API. If you are using Amazon Web Services Organizations to
-- manager your GuardDuty environment, this step is not needed. For more
-- information, see
-- <https://docs.aws.amazon.com/guardduty/latest/ug/guardduty_organizations.html Managing accounts with Amazon Web Services Organizations>.
--
-- To invite Amazon Web Services accounts, the first step is to ensure that
-- GuardDuty has been enabled in the potential member accounts. You can now
-- invoke this API to add accounts by invitation. The invited accounts can
-- either accept or decline the invitation from their GuardDuty accounts.
-- Each invited Amazon Web Services account can choose to accept the
-- invitation from only one Amazon Web Services account. For more
-- information, see
-- <https://docs.aws.amazon.com/guardduty/latest/ug/guardduty_invitations.html Managing GuardDuty accounts by invitation>.
--
-- After the invite has been accepted and you choose to disassociate a
-- member account (by using
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_DisassociateMembers.html DisassociateMembers>)
-- from your account, the details of the member account obtained by
-- invoking
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_CreateMembers.html CreateMembers>,
-- including the associated email addresses, will be retained. This is done
-- so that you can invoke InviteMembers without the need to invoke
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_CreateMembers.html CreateMembers>
-- again. To remove the details associated with a member account, you must
-- also invoke
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_DeleteMembers.html DeleteMembers>.
module Amazonka.GuardDuty.InviteMembers
  ( -- * Creating a Request
    InviteMembers (..),
    newInviteMembers,

    -- * Request Lenses
    inviteMembers_disableEmailNotification,
    inviteMembers_message,
    inviteMembers_detectorId,
    inviteMembers_accountIds,

    -- * Destructuring the Response
    InviteMembersResponse (..),
    newInviteMembersResponse,

    -- * Response Lenses
    inviteMembersResponse_httpStatus,
    inviteMembersResponse_unprocessedAccounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInviteMembers' smart constructor.
data InviteMembers = InviteMembers'
  { -- | A Boolean value that specifies whether you want to disable email
    -- notification to the accounts that you are inviting to GuardDuty as
    -- members.
    disableEmailNotification :: Prelude.Maybe Prelude.Bool,
    -- | The invitation message that you want to send to the accounts that
    -- you\'re inviting to GuardDuty as members.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the detector of the GuardDuty account that you want to
    -- invite members with.
    detectorId :: Prelude.Text,
    -- | A list of account IDs of the accounts that you want to invite to
    -- GuardDuty as members.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InviteMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableEmailNotification', 'inviteMembers_disableEmailNotification' - A Boolean value that specifies whether you want to disable email
-- notification to the accounts that you are inviting to GuardDuty as
-- members.
--
-- 'message', 'inviteMembers_message' - The invitation message that you want to send to the accounts that
-- you\'re inviting to GuardDuty as members.
--
-- 'detectorId', 'inviteMembers_detectorId' - The unique ID of the detector of the GuardDuty account that you want to
-- invite members with.
--
-- 'accountIds', 'inviteMembers_accountIds' - A list of account IDs of the accounts that you want to invite to
-- GuardDuty as members.
newInviteMembers ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  InviteMembers
newInviteMembers pDetectorId_ pAccountIds_ =
  InviteMembers'
    { disableEmailNotification =
        Prelude.Nothing,
      message = Prelude.Nothing,
      detectorId = pDetectorId_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | A Boolean value that specifies whether you want to disable email
-- notification to the accounts that you are inviting to GuardDuty as
-- members.
inviteMembers_disableEmailNotification :: Lens.Lens' InviteMembers (Prelude.Maybe Prelude.Bool)
inviteMembers_disableEmailNotification = Lens.lens (\InviteMembers' {disableEmailNotification} -> disableEmailNotification) (\s@InviteMembers' {} a -> s {disableEmailNotification = a} :: InviteMembers)

-- | The invitation message that you want to send to the accounts that
-- you\'re inviting to GuardDuty as members.
inviteMembers_message :: Lens.Lens' InviteMembers (Prelude.Maybe Prelude.Text)
inviteMembers_message = Lens.lens (\InviteMembers' {message} -> message) (\s@InviteMembers' {} a -> s {message = a} :: InviteMembers)

-- | The unique ID of the detector of the GuardDuty account that you want to
-- invite members with.
inviteMembers_detectorId :: Lens.Lens' InviteMembers Prelude.Text
inviteMembers_detectorId = Lens.lens (\InviteMembers' {detectorId} -> detectorId) (\s@InviteMembers' {} a -> s {detectorId = a} :: InviteMembers)

-- | A list of account IDs of the accounts that you want to invite to
-- GuardDuty as members.
inviteMembers_accountIds :: Lens.Lens' InviteMembers (Prelude.NonEmpty Prelude.Text)
inviteMembers_accountIds = Lens.lens (\InviteMembers' {accountIds} -> accountIds) (\s@InviteMembers' {} a -> s {accountIds = a} :: InviteMembers) Prelude.. Lens.coerced

instance Core.AWSRequest InviteMembers where
  type
    AWSResponse InviteMembers =
      InviteMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InviteMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable InviteMembers where
  hashWithSalt _salt InviteMembers' {..} =
    _salt
      `Prelude.hashWithSalt` disableEmailNotification
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData InviteMembers where
  rnf InviteMembers' {..} =
    Prelude.rnf disableEmailNotification
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf accountIds

instance Data.ToHeaders InviteMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InviteMembers where
  toJSON InviteMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("disableEmailNotification" Data..=)
              Prelude.<$> disableEmailNotification,
            ("message" Data..=) Prelude.<$> message,
            Prelude.Just ("accountIds" Data..= accountIds)
          ]
      )

instance Data.ToPath InviteMembers where
  toPath InviteMembers' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/member/invite"
      ]

instance Data.ToQuery InviteMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInviteMembersResponse' smart constructor.
data InviteMembersResponse = InviteMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InviteMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'inviteMembersResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'inviteMembersResponse_unprocessedAccounts' - A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
newInviteMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InviteMembersResponse
newInviteMembersResponse pHttpStatus_ =
  InviteMembersResponse'
    { httpStatus = pHttpStatus_,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
inviteMembersResponse_httpStatus :: Lens.Lens' InviteMembersResponse Prelude.Int
inviteMembersResponse_httpStatus = Lens.lens (\InviteMembersResponse' {httpStatus} -> httpStatus) (\s@InviteMembersResponse' {} a -> s {httpStatus = a} :: InviteMembersResponse)

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
inviteMembersResponse_unprocessedAccounts :: Lens.Lens' InviteMembersResponse [UnprocessedAccount]
inviteMembersResponse_unprocessedAccounts = Lens.lens (\InviteMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@InviteMembersResponse' {} a -> s {unprocessedAccounts = a} :: InviteMembersResponse) Prelude.. Lens.coerced

instance Prelude.NFData InviteMembersResponse where
  rnf InviteMembersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf unprocessedAccounts
