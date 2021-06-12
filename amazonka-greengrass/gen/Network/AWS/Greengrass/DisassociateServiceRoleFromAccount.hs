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
-- Module      : Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the service role from your account. Without a service
-- role, deployments will not work.
module Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
  ( -- * Creating a Request
    DisassociateServiceRoleFromAccount (..),
    newDisassociateServiceRoleFromAccount,

    -- * Destructuring the Response
    DisassociateServiceRoleFromAccountResponse (..),
    newDisassociateServiceRoleFromAccountResponse,

    -- * Response Lenses
    disassociateServiceRoleFromAccountResponse_disassociatedAt,
    disassociateServiceRoleFromAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateServiceRoleFromAccount' smart constructor.
data DisassociateServiceRoleFromAccount = DisassociateServiceRoleFromAccount'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateServiceRoleFromAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateServiceRoleFromAccount ::
  DisassociateServiceRoleFromAccount
newDisassociateServiceRoleFromAccount =
  DisassociateServiceRoleFromAccount'

instance
  Core.AWSRequest
    DisassociateServiceRoleFromAccount
  where
  type
    AWSResponse DisassociateServiceRoleFromAccount =
      DisassociateServiceRoleFromAccountResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateServiceRoleFromAccountResponse'
            Core.<$> (x Core..?> "DisassociatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateServiceRoleFromAccount

instance
  Core.NFData
    DisassociateServiceRoleFromAccount

instance
  Core.ToHeaders
    DisassociateServiceRoleFromAccount
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToPath
    DisassociateServiceRoleFromAccount
  where
  toPath = Core.const "/greengrass/servicerole"

instance
  Core.ToQuery
    DisassociateServiceRoleFromAccount
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateServiceRoleFromAccountResponse' smart constructor.
data DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse'
  { -- | The time when the service role was disassociated from the account.
    disassociatedAt :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateServiceRoleFromAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociatedAt', 'disassociateServiceRoleFromAccountResponse_disassociatedAt' - The time when the service role was disassociated from the account.
--
-- 'httpStatus', 'disassociateServiceRoleFromAccountResponse_httpStatus' - The response's http status code.
newDisassociateServiceRoleFromAccountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateServiceRoleFromAccountResponse
newDisassociateServiceRoleFromAccountResponse
  pHttpStatus_ =
    DisassociateServiceRoleFromAccountResponse'
      { disassociatedAt =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time when the service role was disassociated from the account.
disassociateServiceRoleFromAccountResponse_disassociatedAt :: Lens.Lens' DisassociateServiceRoleFromAccountResponse (Core.Maybe Core.Text)
disassociateServiceRoleFromAccountResponse_disassociatedAt = Lens.lens (\DisassociateServiceRoleFromAccountResponse' {disassociatedAt} -> disassociatedAt) (\s@DisassociateServiceRoleFromAccountResponse' {} a -> s {disassociatedAt = a} :: DisassociateServiceRoleFromAccountResponse)

-- | The response's http status code.
disassociateServiceRoleFromAccountResponse_httpStatus :: Lens.Lens' DisassociateServiceRoleFromAccountResponse Core.Int
disassociateServiceRoleFromAccountResponse_httpStatus = Lens.lens (\DisassociateServiceRoleFromAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateServiceRoleFromAccountResponse' {} a -> s {httpStatus = a} :: DisassociateServiceRoleFromAccountResponse)

instance
  Core.NFData
    DisassociateServiceRoleFromAccountResponse
