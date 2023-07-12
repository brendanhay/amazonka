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
-- Module      : Amazonka.AuditManager.DeregisterAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an account in Audit Manager.
--
-- Before you deregister, you can use the
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_UpdateSettings.html UpdateSettings>
-- API operation to set your preferred data retention policy. By default,
-- Audit Manager retains your data. If you want to delete your data, you
-- can use the @DeregistrationPolicy@ attribute to request the deletion of
-- your data.
--
-- For more information about data retention, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/data-protection.html Data Protection>
-- in the /Audit Manager User Guide/.
module Amazonka.AuditManager.DeregisterAccount
  ( -- * Creating a Request
    DeregisterAccount (..),
    newDeregisterAccount,

    -- * Destructuring the Response
    DeregisterAccountResponse (..),
    newDeregisterAccountResponse,

    -- * Response Lenses
    deregisterAccountResponse_status,
    deregisterAccountResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterAccount' smart constructor.
data DeregisterAccount = DeregisterAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterAccount ::
  DeregisterAccount
newDeregisterAccount = DeregisterAccount'

instance Core.AWSRequest DeregisterAccount where
  type
    AWSResponse DeregisterAccount =
      DeregisterAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterAccountResponse'
            Prelude.<$> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeregisterAccount where
  rnf _ = ()

instance Data.ToHeaders DeregisterAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterAccount where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeregisterAccount where
  toPath = Prelude.const "/account/deregisterAccount"

instance Data.ToQuery DeregisterAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterAccountResponse' smart constructor.
data DeregisterAccountResponse = DeregisterAccountResponse'
  { -- | The registration status of the account.
    status :: Prelude.Maybe AccountStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deregisterAccountResponse_status' - The registration status of the account.
--
-- 'httpStatus', 'deregisterAccountResponse_httpStatus' - The response's http status code.
newDeregisterAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterAccountResponse
newDeregisterAccountResponse pHttpStatus_ =
  DeregisterAccountResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registration status of the account.
deregisterAccountResponse_status :: Lens.Lens' DeregisterAccountResponse (Prelude.Maybe AccountStatus)
deregisterAccountResponse_status = Lens.lens (\DeregisterAccountResponse' {status} -> status) (\s@DeregisterAccountResponse' {} a -> s {status = a} :: DeregisterAccountResponse)

-- | The response's http status code.
deregisterAccountResponse_httpStatus :: Lens.Lens' DeregisterAccountResponse Prelude.Int
deregisterAccountResponse_httpStatus = Lens.lens (\DeregisterAccountResponse' {httpStatus} -> httpStatus) (\s@DeregisterAccountResponse' {} a -> s {httpStatus = a} :: DeregisterAccountResponse)

instance Prelude.NFData DeregisterAccountResponse where
  rnf DeregisterAccountResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
