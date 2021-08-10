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
-- Module      : Network.AWS.GuardDuty.InviteMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invites other AWS accounts (created as members of the current AWS
-- account by CreateMembers) to enable GuardDuty, and allow the current AWS
-- account to view and manage these accounts\' findings on their behalf as
-- the GuardDuty administrator account.
module Network.AWS.GuardDuty.InviteMembers
  ( -- * Creating a Request
    InviteMembers (..),
    newInviteMembers,

    -- * Request Lenses
    inviteMembers_message,
    inviteMembers_disableEmailNotification,
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

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newInviteMembers' smart constructor.
data InviteMembers = InviteMembers'
  { -- | The invitation message that you want to send to the accounts that
    -- you\'re inviting to GuardDuty as members.
    message :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that specifies whether you want to disable email
    -- notification to the accounts that you are inviting to GuardDuty as
    -- members.
    disableEmailNotification :: Prelude.Maybe Prelude.Bool,
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
-- 'message', 'inviteMembers_message' - The invitation message that you want to send to the accounts that
-- you\'re inviting to GuardDuty as members.
--
-- 'disableEmailNotification', 'inviteMembers_disableEmailNotification' - A Boolean value that specifies whether you want to disable email
-- notification to the accounts that you are inviting to GuardDuty as
-- members.
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
    { message = Prelude.Nothing,
      disableEmailNotification = Prelude.Nothing,
      detectorId = pDetectorId_,
      accountIds = Lens._Coerce Lens.# pAccountIds_
    }

-- | The invitation message that you want to send to the accounts that
-- you\'re inviting to GuardDuty as members.
inviteMembers_message :: Lens.Lens' InviteMembers (Prelude.Maybe Prelude.Text)
inviteMembers_message = Lens.lens (\InviteMembers' {message} -> message) (\s@InviteMembers' {} a -> s {message = a} :: InviteMembers)

-- | A Boolean value that specifies whether you want to disable email
-- notification to the accounts that you are inviting to GuardDuty as
-- members.
inviteMembers_disableEmailNotification :: Lens.Lens' InviteMembers (Prelude.Maybe Prelude.Bool)
inviteMembers_disableEmailNotification = Lens.lens (\InviteMembers' {disableEmailNotification} -> disableEmailNotification) (\s@InviteMembers' {} a -> s {disableEmailNotification = a} :: InviteMembers)

-- | The unique ID of the detector of the GuardDuty account that you want to
-- invite members with.
inviteMembers_detectorId :: Lens.Lens' InviteMembers Prelude.Text
inviteMembers_detectorId = Lens.lens (\InviteMembers' {detectorId} -> detectorId) (\s@InviteMembers' {} a -> s {detectorId = a} :: InviteMembers)

-- | A list of account IDs of the accounts that you want to invite to
-- GuardDuty as members.
inviteMembers_accountIds :: Lens.Lens' InviteMembers (Prelude.NonEmpty Prelude.Text)
inviteMembers_accountIds = Lens.lens (\InviteMembers' {accountIds} -> accountIds) (\s@InviteMembers' {} a -> s {accountIds = a} :: InviteMembers) Prelude.. Lens._Coerce

instance Core.AWSRequest InviteMembers where
  type
    AWSResponse InviteMembers =
      InviteMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InviteMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable InviteMembers

instance Prelude.NFData InviteMembers

instance Core.ToHeaders InviteMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON InviteMembers where
  toJSON InviteMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("message" Core..=) Prelude.<$> message,
            ("disableEmailNotification" Core..=)
              Prelude.<$> disableEmailNotification,
            Prelude.Just ("accountIds" Core..= accountIds)
          ]
      )

instance Core.ToPath InviteMembers where
  toPath InviteMembers' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/member/invite"
      ]

instance Core.ToQuery InviteMembers where
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
inviteMembersResponse_unprocessedAccounts = Lens.lens (\InviteMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@InviteMembersResponse' {} a -> s {unprocessedAccounts = a} :: InviteMembersResponse) Prelude.. Lens._Coerce

instance Prelude.NFData InviteMembersResponse
