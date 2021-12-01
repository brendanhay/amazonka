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
-- Module      : Amazonka.MacieV2.CreateInvitations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an Amazon Macie membership invitation to one or more accounts.
module Amazonka.MacieV2.CreateInvitations
  ( -- * Creating a Request
    CreateInvitations (..),
    newCreateInvitations,

    -- * Request Lenses
    createInvitations_disableEmailNotification,
    createInvitations_message,
    createInvitations_accountIds,

    -- * Destructuring the Response
    CreateInvitationsResponse (..),
    newCreateInvitationsResponse,

    -- * Response Lenses
    createInvitationsResponse_unprocessedAccounts,
    createInvitationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInvitations' smart constructor.
data CreateInvitations = CreateInvitations'
  { -- | Specifies whether to send an email notification to the root user of each
    -- account that the invitation will be sent to. This notification is in
    -- addition to an alert that the root user receives in Personal Health
    -- Dashboard. To send an email notification to the root user of each
    -- account, set this value to true.
    disableEmailNotification :: Prelude.Maybe Prelude.Bool,
    -- | A custom message to include in the invitation. Amazon Macie adds this
    -- message to the standard content that it sends for an invitation.
    message :: Prelude.Maybe Prelude.Text,
    -- | An array that lists Amazon Web Services account IDs, one for each
    -- account to send the invitation to.
    accountIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInvitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableEmailNotification', 'createInvitations_disableEmailNotification' - Specifies whether to send an email notification to the root user of each
-- account that the invitation will be sent to. This notification is in
-- addition to an alert that the root user receives in Personal Health
-- Dashboard. To send an email notification to the root user of each
-- account, set this value to true.
--
-- 'message', 'createInvitations_message' - A custom message to include in the invitation. Amazon Macie adds this
-- message to the standard content that it sends for an invitation.
--
-- 'accountIds', 'createInvitations_accountIds' - An array that lists Amazon Web Services account IDs, one for each
-- account to send the invitation to.
newCreateInvitations ::
  CreateInvitations
newCreateInvitations =
  CreateInvitations'
    { disableEmailNotification =
        Prelude.Nothing,
      message = Prelude.Nothing,
      accountIds = Prelude.mempty
    }

-- | Specifies whether to send an email notification to the root user of each
-- account that the invitation will be sent to. This notification is in
-- addition to an alert that the root user receives in Personal Health
-- Dashboard. To send an email notification to the root user of each
-- account, set this value to true.
createInvitations_disableEmailNotification :: Lens.Lens' CreateInvitations (Prelude.Maybe Prelude.Bool)
createInvitations_disableEmailNotification = Lens.lens (\CreateInvitations' {disableEmailNotification} -> disableEmailNotification) (\s@CreateInvitations' {} a -> s {disableEmailNotification = a} :: CreateInvitations)

-- | A custom message to include in the invitation. Amazon Macie adds this
-- message to the standard content that it sends for an invitation.
createInvitations_message :: Lens.Lens' CreateInvitations (Prelude.Maybe Prelude.Text)
createInvitations_message = Lens.lens (\CreateInvitations' {message} -> message) (\s@CreateInvitations' {} a -> s {message = a} :: CreateInvitations)

-- | An array that lists Amazon Web Services account IDs, one for each
-- account to send the invitation to.
createInvitations_accountIds :: Lens.Lens' CreateInvitations [Prelude.Text]
createInvitations_accountIds = Lens.lens (\CreateInvitations' {accountIds} -> accountIds) (\s@CreateInvitations' {} a -> s {accountIds = a} :: CreateInvitations) Prelude.. Lens.coerced

instance Core.AWSRequest CreateInvitations where
  type
    AWSResponse CreateInvitations =
      CreateInvitationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInvitationsResponse'
            Prelude.<$> ( x Core..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInvitations where
  hashWithSalt salt' CreateInvitations' {..} =
    salt' `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` disableEmailNotification

instance Prelude.NFData CreateInvitations where
  rnf CreateInvitations' {..} =
    Prelude.rnf disableEmailNotification
      `Prelude.seq` Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf message

instance Core.ToHeaders CreateInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateInvitations where
  toJSON CreateInvitations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("disableEmailNotification" Core..=)
              Prelude.<$> disableEmailNotification,
            ("message" Core..=) Prelude.<$> message,
            Prelude.Just ("accountIds" Core..= accountIds)
          ]
      )

instance Core.ToPath CreateInvitations where
  toPath = Prelude.const "/invitations"

instance Core.ToQuery CreateInvitations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInvitationsResponse' smart constructor.
data CreateInvitationsResponse = CreateInvitationsResponse'
  { -- | An array of objects, one for each account whose invitation hasn\'t been
    -- processed. Each object identifies the account and explains why the
    -- invitation hasn\'t been processed for the account.
    unprocessedAccounts :: Prelude.Maybe [UnprocessedAccount],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInvitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAccounts', 'createInvitationsResponse_unprocessedAccounts' - An array of objects, one for each account whose invitation hasn\'t been
-- processed. Each object identifies the account and explains why the
-- invitation hasn\'t been processed for the account.
--
-- 'httpStatus', 'createInvitationsResponse_httpStatus' - The response's http status code.
newCreateInvitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInvitationsResponse
newCreateInvitationsResponse pHttpStatus_ =
  CreateInvitationsResponse'
    { unprocessedAccounts =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each account whose invitation hasn\'t been
-- processed. Each object identifies the account and explains why the
-- invitation hasn\'t been processed for the account.
createInvitationsResponse_unprocessedAccounts :: Lens.Lens' CreateInvitationsResponse (Prelude.Maybe [UnprocessedAccount])
createInvitationsResponse_unprocessedAccounts = Lens.lens (\CreateInvitationsResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@CreateInvitationsResponse' {} a -> s {unprocessedAccounts = a} :: CreateInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createInvitationsResponse_httpStatus :: Lens.Lens' CreateInvitationsResponse Prelude.Int
createInvitationsResponse_httpStatus = Lens.lens (\CreateInvitationsResponse' {httpStatus} -> httpStatus) (\s@CreateInvitationsResponse' {} a -> s {httpStatus = a} :: CreateInvitationsResponse)

instance Prelude.NFData CreateInvitationsResponse where
  rnf CreateInvitationsResponse' {..} =
    Prelude.rnf unprocessedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
