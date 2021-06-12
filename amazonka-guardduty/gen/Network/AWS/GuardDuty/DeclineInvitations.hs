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
-- Module      : Network.AWS.GuardDuty.DeclineInvitations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines invitations sent to the current member account by AWS accounts
-- specified by their account IDs.
module Network.AWS.GuardDuty.DeclineInvitations
  ( -- * Creating a Request
    DeclineInvitations (..),
    newDeclineInvitations,

    -- * Request Lenses
    declineInvitations_accountIds,

    -- * Destructuring the Response
    DeclineInvitationsResponse (..),
    newDeclineInvitationsResponse,

    -- * Response Lenses
    declineInvitationsResponse_httpStatus,
    declineInvitationsResponse_unprocessedAccounts,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeclineInvitations' smart constructor.
data DeclineInvitations = DeclineInvitations'
  { -- | A list of account IDs of the AWS accounts that sent invitations to the
    -- current member account that you want to decline invitations from.
    accountIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeclineInvitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'declineInvitations_accountIds' - A list of account IDs of the AWS accounts that sent invitations to the
-- current member account that you want to decline invitations from.
newDeclineInvitations ::
  -- | 'accountIds'
  Core.NonEmpty Core.Text ->
  DeclineInvitations
newDeclineInvitations pAccountIds_ =
  DeclineInvitations'
    { accountIds =
        Lens._Coerce Lens.# pAccountIds_
    }

-- | A list of account IDs of the AWS accounts that sent invitations to the
-- current member account that you want to decline invitations from.
declineInvitations_accountIds :: Lens.Lens' DeclineInvitations (Core.NonEmpty Core.Text)
declineInvitations_accountIds = Lens.lens (\DeclineInvitations' {accountIds} -> accountIds) (\s@DeclineInvitations' {} a -> s {accountIds = a} :: DeclineInvitations) Core.. Lens._Coerce

instance Core.AWSRequest DeclineInvitations where
  type
    AWSResponse DeclineInvitations =
      DeclineInvitationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeclineInvitationsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "unprocessedAccounts"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable DeclineInvitations

instance Core.NFData DeclineInvitations

instance Core.ToHeaders DeclineInvitations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeclineInvitations where
  toJSON DeclineInvitations' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("accountIds" Core..= accountIds)]
      )

instance Core.ToPath DeclineInvitations where
  toPath = Core.const "/invitation/decline"

instance Core.ToQuery DeclineInvitations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeclineInvitationsResponse' smart constructor.
data DeclineInvitationsResponse = DeclineInvitationsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeclineInvitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'declineInvitationsResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'declineInvitationsResponse_unprocessedAccounts' - A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
newDeclineInvitationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeclineInvitationsResponse
newDeclineInvitationsResponse pHttpStatus_ =
  DeclineInvitationsResponse'
    { httpStatus =
        pHttpStatus_,
      unprocessedAccounts = Core.mempty
    }

-- | The response's http status code.
declineInvitationsResponse_httpStatus :: Lens.Lens' DeclineInvitationsResponse Core.Int
declineInvitationsResponse_httpStatus = Lens.lens (\DeclineInvitationsResponse' {httpStatus} -> httpStatus) (\s@DeclineInvitationsResponse' {} a -> s {httpStatus = a} :: DeclineInvitationsResponse)

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
declineInvitationsResponse_unprocessedAccounts :: Lens.Lens' DeclineInvitationsResponse [UnprocessedAccount]
declineInvitationsResponse_unprocessedAccounts = Lens.lens (\DeclineInvitationsResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DeclineInvitationsResponse' {} a -> s {unprocessedAccounts = a} :: DeclineInvitationsResponse) Core.. Lens._Coerce

instance Core.NFData DeclineInvitationsResponse
