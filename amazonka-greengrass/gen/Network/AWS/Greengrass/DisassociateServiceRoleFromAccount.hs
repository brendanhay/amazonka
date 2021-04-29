{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateServiceRoleFromAccount' smart constructor.
data DisassociateServiceRoleFromAccount = DisassociateServiceRoleFromAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateServiceRoleFromAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateServiceRoleFromAccount ::
  DisassociateServiceRoleFromAccount
newDisassociateServiceRoleFromAccount =
  DisassociateServiceRoleFromAccount'

instance
  Prelude.AWSRequest
    DisassociateServiceRoleFromAccount
  where
  type
    Rs DisassociateServiceRoleFromAccount =
      DisassociateServiceRoleFromAccountResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateServiceRoleFromAccountResponse'
            Prelude.<$> (x Prelude..?> "DisassociatedAt")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateServiceRoleFromAccount

instance
  Prelude.NFData
    DisassociateServiceRoleFromAccount

instance
  Prelude.ToHeaders
    DisassociateServiceRoleFromAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToPath
    DisassociateServiceRoleFromAccount
  where
  toPath = Prelude.const "/greengrass/servicerole"

instance
  Prelude.ToQuery
    DisassociateServiceRoleFromAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateServiceRoleFromAccountResponse' smart constructor.
data DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse'
  { -- | The time when the service role was disassociated from the account.
    disassociatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateServiceRoleFromAccountResponse
newDisassociateServiceRoleFromAccountResponse
  pHttpStatus_ =
    DisassociateServiceRoleFromAccountResponse'
      { disassociatedAt =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time when the service role was disassociated from the account.
disassociateServiceRoleFromAccountResponse_disassociatedAt :: Lens.Lens' DisassociateServiceRoleFromAccountResponse (Prelude.Maybe Prelude.Text)
disassociateServiceRoleFromAccountResponse_disassociatedAt = Lens.lens (\DisassociateServiceRoleFromAccountResponse' {disassociatedAt} -> disassociatedAt) (\s@DisassociateServiceRoleFromAccountResponse' {} a -> s {disassociatedAt = a} :: DisassociateServiceRoleFromAccountResponse)

-- | The response's http status code.
disassociateServiceRoleFromAccountResponse_httpStatus :: Lens.Lens' DisassociateServiceRoleFromAccountResponse Prelude.Int
disassociateServiceRoleFromAccountResponse_httpStatus = Lens.lens (\DisassociateServiceRoleFromAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateServiceRoleFromAccountResponse' {} a -> s {httpStatus = a} :: DisassociateServiceRoleFromAccountResponse)

instance
  Prelude.NFData
    DisassociateServiceRoleFromAccountResponse
