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
-- Module      : Amazonka.Greengrass.DisassociateServiceRoleFromAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the service role from your account. Without a service
-- role, deployments will not work.
module Amazonka.Greengrass.DisassociateServiceRoleFromAccount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateServiceRoleFromAccount' smart constructor.
data DisassociateServiceRoleFromAccount = DisassociateServiceRoleFromAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateServiceRoleFromAccountResponse'
            Prelude.<$> (x Data..?> "DisassociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateServiceRoleFromAccount
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DisassociateServiceRoleFromAccount
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DisassociateServiceRoleFromAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DisassociateServiceRoleFromAccount
  where
  toPath = Prelude.const "/greengrass/servicerole"

instance
  Data.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociateServiceRoleFromAccountResponse' {..} =
    Prelude.rnf disassociatedAt `Prelude.seq`
      Prelude.rnf httpStatus
