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
-- Module      : Amazonka.SecurityHub.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the Security Hub administrator account for an organization.
-- Can only be called by the organization management account.
module Amazonka.SecurityHub.EnableOrganizationAdminAccount
  ( -- * Creating a Request
    EnableOrganizationAdminAccount (..),
    newEnableOrganizationAdminAccount,

    -- * Request Lenses
    enableOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    EnableOrganizationAdminAccountResponse (..),
    newEnableOrganizationAdminAccountResponse,

    -- * Response Lenses
    enableOrganizationAdminAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newEnableOrganizationAdminAccount' smart constructor.
data EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { -- | The Amazon Web Services account identifier of the account to designate
    -- as the Security Hub administrator account.
    adminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'enableOrganizationAdminAccount_adminAccountId' - The Amazon Web Services account identifier of the account to designate
-- as the Security Hub administrator account.
newEnableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Prelude.Text ->
  EnableOrganizationAdminAccount
newEnableOrganizationAdminAccount pAdminAccountId_ =
  EnableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The Amazon Web Services account identifier of the account to designate
-- as the Security Hub administrator account.
enableOrganizationAdminAccount_adminAccountId :: Lens.Lens' EnableOrganizationAdminAccount Prelude.Text
enableOrganizationAdminAccount_adminAccountId = Lens.lens (\EnableOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@EnableOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: EnableOrganizationAdminAccount)

instance
  Core.AWSRequest
    EnableOrganizationAdminAccount
  where
  type
    AWSResponse EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableOrganizationAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    EnableOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` adminAccountId

instance
  Prelude.NFData
    EnableOrganizationAdminAccount
  where
  rnf EnableOrganizationAdminAccount' {..} =
    Prelude.rnf adminAccountId

instance
  Data.ToHeaders
    EnableOrganizationAdminAccount
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

instance Data.ToJSON EnableOrganizationAdminAccount where
  toJSON EnableOrganizationAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AdminAccountId" Data..= adminAccountId)
          ]
      )

instance Data.ToPath EnableOrganizationAdminAccount where
  toPath = Prelude.const "/organization/admin/enable"

instance Data.ToQuery EnableOrganizationAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableOrganizationAdminAccountResponse' smart constructor.
data EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newEnableOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableOrganizationAdminAccountResponse
newEnableOrganizationAdminAccountResponse
  pHttpStatus_ =
    EnableOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
enableOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' EnableOrganizationAdminAccountResponse Prelude.Int
enableOrganizationAdminAccountResponse_httpStatus = Lens.lens (\EnableOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@EnableOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: EnableOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    EnableOrganizationAdminAccountResponse
  where
  rnf EnableOrganizationAdminAccountResponse' {..} =
    Prelude.rnf httpStatus
