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
-- Module      : Amazonka.AuditManager.RegisterOrganizationAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an Amazon Web Services account within the organization as the
-- delegated administrator for Audit Manager.
module Amazonka.AuditManager.RegisterOrganizationAdminAccount
  ( -- * Creating a Request
    RegisterOrganizationAdminAccount (..),
    newRegisterOrganizationAdminAccount,

    -- * Request Lenses
    registerOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    RegisterOrganizationAdminAccountResponse (..),
    newRegisterOrganizationAdminAccountResponse,

    -- * Response Lenses
    registerOrganizationAdminAccountResponse_adminAccountId,
    registerOrganizationAdminAccountResponse_organizationId,
    registerOrganizationAdminAccountResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterOrganizationAdminAccount' smart constructor.
data RegisterOrganizationAdminAccount = RegisterOrganizationAdminAccount'
  { -- | The identifier for the delegated administrator account.
    adminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'registerOrganizationAdminAccount_adminAccountId' - The identifier for the delegated administrator account.
newRegisterOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Prelude.Text ->
  RegisterOrganizationAdminAccount
newRegisterOrganizationAdminAccount pAdminAccountId_ =
  RegisterOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The identifier for the delegated administrator account.
registerOrganizationAdminAccount_adminAccountId :: Lens.Lens' RegisterOrganizationAdminAccount Prelude.Text
registerOrganizationAdminAccount_adminAccountId = Lens.lens (\RegisterOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@RegisterOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: RegisterOrganizationAdminAccount)

instance
  Core.AWSRequest
    RegisterOrganizationAdminAccount
  where
  type
    AWSResponse RegisterOrganizationAdminAccount =
      RegisterOrganizationAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterOrganizationAdminAccountResponse'
            Prelude.<$> (x Data..?> "adminAccountId")
            Prelude.<*> (x Data..?> "organizationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    RegisterOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` adminAccountId

instance
  Prelude.NFData
    RegisterOrganizationAdminAccount
  where
  rnf RegisterOrganizationAdminAccount' {..} =
    Prelude.rnf adminAccountId

instance
  Data.ToHeaders
    RegisterOrganizationAdminAccount
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

instance Data.ToJSON RegisterOrganizationAdminAccount where
  toJSON RegisterOrganizationAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("adminAccountId" Data..= adminAccountId)
          ]
      )

instance Data.ToPath RegisterOrganizationAdminAccount where
  toPath =
    Prelude.const
      "/account/registerOrganizationAdminAccount"

instance
  Data.ToQuery
    RegisterOrganizationAdminAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterOrganizationAdminAccountResponse' smart constructor.
data RegisterOrganizationAdminAccountResponse = RegisterOrganizationAdminAccountResponse'
  { -- | The identifier for the delegated administrator account.
    adminAccountId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'registerOrganizationAdminAccountResponse_adminAccountId' - The identifier for the delegated administrator account.
--
-- 'organizationId', 'registerOrganizationAdminAccountResponse_organizationId' - The identifier for the organization.
--
-- 'httpStatus', 'registerOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newRegisterOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterOrganizationAdminAccountResponse
newRegisterOrganizationAdminAccountResponse
  pHttpStatus_ =
    RegisterOrganizationAdminAccountResponse'
      { adminAccountId =
          Prelude.Nothing,
        organizationId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier for the delegated administrator account.
registerOrganizationAdminAccountResponse_adminAccountId :: Lens.Lens' RegisterOrganizationAdminAccountResponse (Prelude.Maybe Prelude.Text)
registerOrganizationAdminAccountResponse_adminAccountId = Lens.lens (\RegisterOrganizationAdminAccountResponse' {adminAccountId} -> adminAccountId) (\s@RegisterOrganizationAdminAccountResponse' {} a -> s {adminAccountId = a} :: RegisterOrganizationAdminAccountResponse)

-- | The identifier for the organization.
registerOrganizationAdminAccountResponse_organizationId :: Lens.Lens' RegisterOrganizationAdminAccountResponse (Prelude.Maybe Prelude.Text)
registerOrganizationAdminAccountResponse_organizationId = Lens.lens (\RegisterOrganizationAdminAccountResponse' {organizationId} -> organizationId) (\s@RegisterOrganizationAdminAccountResponse' {} a -> s {organizationId = a} :: RegisterOrganizationAdminAccountResponse)

-- | The response's http status code.
registerOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' RegisterOrganizationAdminAccountResponse Prelude.Int
registerOrganizationAdminAccountResponse_httpStatus = Lens.lens (\RegisterOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@RegisterOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: RegisterOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    RegisterOrganizationAdminAccountResponse
  where
  rnf RegisterOrganizationAdminAccountResponse' {..} =
    Prelude.rnf adminAccountId `Prelude.seq`
      Prelude.rnf organizationId `Prelude.seq`
        Prelude.rnf httpStatus
