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
-- Module      : Amazonka.Detective.CreateMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @CreateMembers@ is used to send invitations to accounts. For the
-- organization behavior graph, the Detective administrator account uses
-- @CreateMembers@ to enable organization accounts as member accounts.
--
-- For invited accounts, @CreateMembers@ sends a request to invite the
-- specified Amazon Web Services accounts to be member accounts in the
-- behavior graph. This operation can only be called by the administrator
-- account for a behavior graph.
--
-- @CreateMembers@ verifies the accounts and then invites the verified
-- accounts. The administrator can optionally specify to not send
-- invitation emails to the member accounts. This would be used when the
-- administrator manages their member accounts centrally.
--
-- For organization accounts in the organization behavior graph,
-- @CreateMembers@ attempts to enable the accounts. The organization
-- accounts do not receive invitations.
--
-- The request provides the behavior graph ARN and the list of accounts to
-- invite or to enable.
--
-- The response separates the requested accounts into two lists:
--
-- -   The accounts that @CreateMembers@ was able to process. For invited
--     accounts, includes member accounts that are being verified, that
--     have passed verification and are to be invited, and that have failed
--     verification. For organization accounts in the organization behavior
--     graph, includes accounts that can be enabled and that cannot be
--     enabled.
--
-- -   The accounts that @CreateMembers@ was unable to process. This list
--     includes accounts that were already invited to be member accounts in
--     the behavior graph.
module Amazonka.Detective.CreateMembers
  ( -- * Creating a Request
    CreateMembers (..),
    newCreateMembers,

    -- * Request Lenses
    createMembers_message,
    createMembers_disableEmailNotification,
    createMembers_graphArn,
    createMembers_accounts,

    -- * Destructuring the Response
    CreateMembersResponse (..),
    newCreateMembersResponse,

    -- * Response Lenses
    createMembersResponse_unprocessedAccounts,
    createMembersResponse_members,
    createMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMembers' smart constructor.
data CreateMembers = CreateMembers'
  { -- | Customized message text to include in the invitation email message to
    -- the invited member accounts.
    message :: Prelude.Maybe Prelude.Text,
    -- | if set to @true@, then the invited accounts do not receive email
    -- notifications. By default, this is set to @false@, and the invited
    -- accounts receive email notifications.
    --
    -- Organization accounts in the organization behavior graph do not receive
    -- email notifications.
    disableEmailNotification :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the behavior graph.
    graphArn :: Prelude.Text,
    -- | The list of Amazon Web Services accounts to invite or to enable. You can
    -- invite or enable up to 50 accounts at a time. For each invited account,
    -- the account list contains the account identifier and the Amazon Web
    -- Services account root user email address. For organization accounts in
    -- the organization behavior graph, the email address is not required.
    accounts :: Prelude.NonEmpty Account
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'createMembers_message' - Customized message text to include in the invitation email message to
-- the invited member accounts.
--
-- 'disableEmailNotification', 'createMembers_disableEmailNotification' - if set to @true@, then the invited accounts do not receive email
-- notifications. By default, this is set to @false@, and the invited
-- accounts receive email notifications.
--
-- Organization accounts in the organization behavior graph do not receive
-- email notifications.
--
-- 'graphArn', 'createMembers_graphArn' - The ARN of the behavior graph.
--
-- 'accounts', 'createMembers_accounts' - The list of Amazon Web Services accounts to invite or to enable. You can
-- invite or enable up to 50 accounts at a time. For each invited account,
-- the account list contains the account identifier and the Amazon Web
-- Services account root user email address. For organization accounts in
-- the organization behavior graph, the email address is not required.
newCreateMembers ::
  -- | 'graphArn'
  Prelude.Text ->
  -- | 'accounts'
  Prelude.NonEmpty Account ->
  CreateMembers
newCreateMembers pGraphArn_ pAccounts_ =
  CreateMembers'
    { message = Prelude.Nothing,
      disableEmailNotification = Prelude.Nothing,
      graphArn = pGraphArn_,
      accounts = Lens.coerced Lens.# pAccounts_
    }

-- | Customized message text to include in the invitation email message to
-- the invited member accounts.
createMembers_message :: Lens.Lens' CreateMembers (Prelude.Maybe Prelude.Text)
createMembers_message = Lens.lens (\CreateMembers' {message} -> message) (\s@CreateMembers' {} a -> s {message = a} :: CreateMembers)

-- | if set to @true@, then the invited accounts do not receive email
-- notifications. By default, this is set to @false@, and the invited
-- accounts receive email notifications.
--
-- Organization accounts in the organization behavior graph do not receive
-- email notifications.
createMembers_disableEmailNotification :: Lens.Lens' CreateMembers (Prelude.Maybe Prelude.Bool)
createMembers_disableEmailNotification = Lens.lens (\CreateMembers' {disableEmailNotification} -> disableEmailNotification) (\s@CreateMembers' {} a -> s {disableEmailNotification = a} :: CreateMembers)

-- | The ARN of the behavior graph.
createMembers_graphArn :: Lens.Lens' CreateMembers Prelude.Text
createMembers_graphArn = Lens.lens (\CreateMembers' {graphArn} -> graphArn) (\s@CreateMembers' {} a -> s {graphArn = a} :: CreateMembers)

-- | The list of Amazon Web Services accounts to invite or to enable. You can
-- invite or enable up to 50 accounts at a time. For each invited account,
-- the account list contains the account identifier and the Amazon Web
-- Services account root user email address. For organization accounts in
-- the organization behavior graph, the email address is not required.
createMembers_accounts :: Lens.Lens' CreateMembers (Prelude.NonEmpty Account)
createMembers_accounts = Lens.lens (\CreateMembers' {accounts} -> accounts) (\s@CreateMembers' {} a -> s {accounts = a} :: CreateMembers) Prelude.. Lens.coerced

instance Core.AWSRequest CreateMembers where
  type
    AWSResponse CreateMembers =
      CreateMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMembersResponse'
            Prelude.<$> ( x Core..?> "UnprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Members" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMembers where
  hashWithSalt _salt CreateMembers' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` disableEmailNotification
      `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` accounts

instance Prelude.NFData CreateMembers where
  rnf CreateMembers' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf disableEmailNotification
      `Prelude.seq` Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf accounts

instance Core.ToHeaders CreateMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMembers where
  toJSON CreateMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Message" Core..=) Prelude.<$> message,
            ("DisableEmailNotification" Core..=)
              Prelude.<$> disableEmailNotification,
            Prelude.Just ("GraphArn" Core..= graphArn),
            Prelude.Just ("Accounts" Core..= accounts)
          ]
      )

instance Core.ToPath CreateMembers where
  toPath = Prelude.const "/graph/members"

instance Core.ToQuery CreateMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMembersResponse' smart constructor.
data CreateMembersResponse = CreateMembersResponse'
  { -- | The list of accounts for which Detective was unable to process the
    -- invitation or enablement request. For each account, the list provides
    -- the reason why the request could not be processed. The list includes
    -- accounts that are already member accounts in the behavior graph.
    unprocessedAccounts :: Prelude.Maybe [UnprocessedAccount],
    -- | The set of member account invitation or enablement requests that
    -- Detective was able to process. This includes accounts that are being
    -- verified, that failed verification, and that passed verification and are
    -- being sent an invitation or are being enabled.
    members :: Prelude.Maybe [MemberDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAccounts', 'createMembersResponse_unprocessedAccounts' - The list of accounts for which Detective was unable to process the
-- invitation or enablement request. For each account, the list provides
-- the reason why the request could not be processed. The list includes
-- accounts that are already member accounts in the behavior graph.
--
-- 'members', 'createMembersResponse_members' - The set of member account invitation or enablement requests that
-- Detective was able to process. This includes accounts that are being
-- verified, that failed verification, and that passed verification and are
-- being sent an invitation or are being enabled.
--
-- 'httpStatus', 'createMembersResponse_httpStatus' - The response's http status code.
newCreateMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMembersResponse
newCreateMembersResponse pHttpStatus_ =
  CreateMembersResponse'
    { unprocessedAccounts =
        Prelude.Nothing,
      members = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of accounts for which Detective was unable to process the
-- invitation or enablement request. For each account, the list provides
-- the reason why the request could not be processed. The list includes
-- accounts that are already member accounts in the behavior graph.
createMembersResponse_unprocessedAccounts :: Lens.Lens' CreateMembersResponse (Prelude.Maybe [UnprocessedAccount])
createMembersResponse_unprocessedAccounts = Lens.lens (\CreateMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@CreateMembersResponse' {} a -> s {unprocessedAccounts = a} :: CreateMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The set of member account invitation or enablement requests that
-- Detective was able to process. This includes accounts that are being
-- verified, that failed verification, and that passed verification and are
-- being sent an invitation or are being enabled.
createMembersResponse_members :: Lens.Lens' CreateMembersResponse (Prelude.Maybe [MemberDetail])
createMembersResponse_members = Lens.lens (\CreateMembersResponse' {members} -> members) (\s@CreateMembersResponse' {} a -> s {members = a} :: CreateMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createMembersResponse_httpStatus :: Lens.Lens' CreateMembersResponse Prelude.Int
createMembersResponse_httpStatus = Lens.lens (\CreateMembersResponse' {httpStatus} -> httpStatus) (\s@CreateMembersResponse' {} a -> s {httpStatus = a} :: CreateMembersResponse)

instance Prelude.NFData CreateMembersResponse where
  rnf CreateMembersResponse' {..} =
    Prelude.rnf unprocessedAccounts
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf httpStatus
