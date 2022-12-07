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
-- Module      : Amazonka.AuditManager.RegisterAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Audit Manager for the specified Amazon Web Services account.
module Amazonka.AuditManager.RegisterAccount
  ( -- * Creating a Request
    RegisterAccount (..),
    newRegisterAccount,

    -- * Request Lenses
    registerAccount_kmsKey,
    registerAccount_delegatedAdminAccount,

    -- * Destructuring the Response
    RegisterAccountResponse (..),
    newRegisterAccountResponse,

    -- * Response Lenses
    registerAccountResponse_status,
    registerAccountResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterAccount' smart constructor.
data RegisterAccount = RegisterAccount'
  { -- | The KMS key details.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The delegated administrator account for Audit Manager.
    delegatedAdminAccount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'registerAccount_kmsKey' - The KMS key details.
--
-- 'delegatedAdminAccount', 'registerAccount_delegatedAdminAccount' - The delegated administrator account for Audit Manager.
newRegisterAccount ::
  RegisterAccount
newRegisterAccount =
  RegisterAccount'
    { kmsKey = Prelude.Nothing,
      delegatedAdminAccount = Prelude.Nothing
    }

-- | The KMS key details.
registerAccount_kmsKey :: Lens.Lens' RegisterAccount (Prelude.Maybe Prelude.Text)
registerAccount_kmsKey = Lens.lens (\RegisterAccount' {kmsKey} -> kmsKey) (\s@RegisterAccount' {} a -> s {kmsKey = a} :: RegisterAccount)

-- | The delegated administrator account for Audit Manager.
registerAccount_delegatedAdminAccount :: Lens.Lens' RegisterAccount (Prelude.Maybe Prelude.Text)
registerAccount_delegatedAdminAccount = Lens.lens (\RegisterAccount' {delegatedAdminAccount} -> delegatedAdminAccount) (\s@RegisterAccount' {} a -> s {delegatedAdminAccount = a} :: RegisterAccount)

instance Core.AWSRequest RegisterAccount where
  type
    AWSResponse RegisterAccount =
      RegisterAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterAccountResponse'
            Prelude.<$> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterAccount where
  hashWithSalt _salt RegisterAccount' {..} =
    _salt `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` delegatedAdminAccount

instance Prelude.NFData RegisterAccount where
  rnf RegisterAccount' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf delegatedAdminAccount

instance Data.ToHeaders RegisterAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterAccount where
  toJSON RegisterAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKey" Data..=) Prelude.<$> kmsKey,
            ("delegatedAdminAccount" Data..=)
              Prelude.<$> delegatedAdminAccount
          ]
      )

instance Data.ToPath RegisterAccount where
  toPath = Prelude.const "/account/registerAccount"

instance Data.ToQuery RegisterAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterAccountResponse' smart constructor.
data RegisterAccountResponse = RegisterAccountResponse'
  { -- | The status of the account registration request.
    status :: Prelude.Maybe AccountStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'registerAccountResponse_status' - The status of the account registration request.
--
-- 'httpStatus', 'registerAccountResponse_httpStatus' - The response's http status code.
newRegisterAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterAccountResponse
newRegisterAccountResponse pHttpStatus_ =
  RegisterAccountResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the account registration request.
registerAccountResponse_status :: Lens.Lens' RegisterAccountResponse (Prelude.Maybe AccountStatus)
registerAccountResponse_status = Lens.lens (\RegisterAccountResponse' {status} -> status) (\s@RegisterAccountResponse' {} a -> s {status = a} :: RegisterAccountResponse)

-- | The response's http status code.
registerAccountResponse_httpStatus :: Lens.Lens' RegisterAccountResponse Prelude.Int
registerAccountResponse_httpStatus = Lens.lens (\RegisterAccountResponse' {httpStatus} -> httpStatus) (\s@RegisterAccountResponse' {} a -> s {httpStatus = a} :: RegisterAccountResponse)

instance Prelude.NFData RegisterAccountResponse where
  rnf RegisterAccountResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
