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
-- Module      : Network.AWS.WorkMail.UpdateMailboxQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user\'s current mailbox quota for a specified organization and
-- user.
module Network.AWS.WorkMail.UpdateMailboxQuota
  ( -- * Creating a Request
    UpdateMailboxQuota (..),
    newUpdateMailboxQuota,

    -- * Request Lenses
    updateMailboxQuota_organizationId,
    updateMailboxQuota_userId,
    updateMailboxQuota_mailboxQuota,

    -- * Destructuring the Response
    UpdateMailboxQuotaResponse (..),
    newUpdateMailboxQuotaResponse,

    -- * Response Lenses
    updateMailboxQuotaResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newUpdateMailboxQuota' smart constructor.
data UpdateMailboxQuota = UpdateMailboxQuota'
  { -- | The identifier for the organization that contains the user for whom to
    -- update the mailbox quota.
    organizationId :: Prelude.Text,
    -- | The identifer for the user for whom to update the mailbox quota.
    userId :: Prelude.Text,
    -- | The updated mailbox quota, in MB, for the specified user.
    mailboxQuota :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMailboxQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'updateMailboxQuota_organizationId' - The identifier for the organization that contains the user for whom to
-- update the mailbox quota.
--
-- 'userId', 'updateMailboxQuota_userId' - The identifer for the user for whom to update the mailbox quota.
--
-- 'mailboxQuota', 'updateMailboxQuota_mailboxQuota' - The updated mailbox quota, in MB, for the specified user.
newUpdateMailboxQuota ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'mailboxQuota'
  Prelude.Natural ->
  UpdateMailboxQuota
newUpdateMailboxQuota
  pOrganizationId_
  pUserId_
  pMailboxQuota_ =
    UpdateMailboxQuota'
      { organizationId =
          pOrganizationId_,
        userId = pUserId_,
        mailboxQuota = pMailboxQuota_
      }

-- | The identifier for the organization that contains the user for whom to
-- update the mailbox quota.
updateMailboxQuota_organizationId :: Lens.Lens' UpdateMailboxQuota Prelude.Text
updateMailboxQuota_organizationId = Lens.lens (\UpdateMailboxQuota' {organizationId} -> organizationId) (\s@UpdateMailboxQuota' {} a -> s {organizationId = a} :: UpdateMailboxQuota)

-- | The identifer for the user for whom to update the mailbox quota.
updateMailboxQuota_userId :: Lens.Lens' UpdateMailboxQuota Prelude.Text
updateMailboxQuota_userId = Lens.lens (\UpdateMailboxQuota' {userId} -> userId) (\s@UpdateMailboxQuota' {} a -> s {userId = a} :: UpdateMailboxQuota)

-- | The updated mailbox quota, in MB, for the specified user.
updateMailboxQuota_mailboxQuota :: Lens.Lens' UpdateMailboxQuota Prelude.Natural
updateMailboxQuota_mailboxQuota = Lens.lens (\UpdateMailboxQuota' {mailboxQuota} -> mailboxQuota) (\s@UpdateMailboxQuota' {} a -> s {mailboxQuota = a} :: UpdateMailboxQuota)

instance Core.AWSRequest UpdateMailboxQuota where
  type
    AWSResponse UpdateMailboxQuota =
      UpdateMailboxQuotaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMailboxQuotaResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMailboxQuota

instance Prelude.NFData UpdateMailboxQuota

instance Core.ToHeaders UpdateMailboxQuota where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.UpdateMailboxQuota" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMailboxQuota where
  toJSON UpdateMailboxQuota' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("UserId" Core..= userId),
            Prelude.Just ("MailboxQuota" Core..= mailboxQuota)
          ]
      )

instance Core.ToPath UpdateMailboxQuota where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateMailboxQuota where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMailboxQuotaResponse' smart constructor.
data UpdateMailboxQuotaResponse = UpdateMailboxQuotaResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMailboxQuotaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMailboxQuotaResponse_httpStatus' - The response's http status code.
newUpdateMailboxQuotaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMailboxQuotaResponse
newUpdateMailboxQuotaResponse pHttpStatus_ =
  UpdateMailboxQuotaResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateMailboxQuotaResponse_httpStatus :: Lens.Lens' UpdateMailboxQuotaResponse Prelude.Int
updateMailboxQuotaResponse_httpStatus = Lens.lens (\UpdateMailboxQuotaResponse' {httpStatus} -> httpStatus) (\s@UpdateMailboxQuotaResponse' {} a -> s {httpStatus = a} :: UpdateMailboxQuotaResponse)

instance Prelude.NFData UpdateMailboxQuotaResponse
