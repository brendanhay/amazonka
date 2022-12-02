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
-- Module      : Amazonka.MacieV2.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates an account as the delegated Amazon Macie administrator
-- account for an organization in Organizations.
module Amazonka.MacieV2.EnableOrganizationAdminAccount
  ( -- * Creating a Request
    EnableOrganizationAdminAccount (..),
    newEnableOrganizationAdminAccount,

    -- * Request Lenses
    enableOrganizationAdminAccount_clientToken,
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
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableOrganizationAdminAccount' smart constructor.
data EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID for the account to designate as the
    -- delegated Amazon Macie administrator account for the organization.
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
-- 'clientToken', 'enableOrganizationAdminAccount_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'adminAccountId', 'enableOrganizationAdminAccount_adminAccountId' - The Amazon Web Services account ID for the account to designate as the
-- delegated Amazon Macie administrator account for the organization.
newEnableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Prelude.Text ->
  EnableOrganizationAdminAccount
newEnableOrganizationAdminAccount pAdminAccountId_ =
  EnableOrganizationAdminAccount'
    { clientToken =
        Prelude.Nothing,
      adminAccountId = pAdminAccountId_
    }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
enableOrganizationAdminAccount_clientToken :: Lens.Lens' EnableOrganizationAdminAccount (Prelude.Maybe Prelude.Text)
enableOrganizationAdminAccount_clientToken = Lens.lens (\EnableOrganizationAdminAccount' {clientToken} -> clientToken) (\s@EnableOrganizationAdminAccount' {} a -> s {clientToken = a} :: EnableOrganizationAdminAccount)

-- | The Amazon Web Services account ID for the account to designate as the
-- delegated Amazon Macie administrator account for the organization.
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
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` adminAccountId

instance
  Prelude.NFData
    EnableOrganizationAdminAccount
  where
  rnf EnableOrganizationAdminAccount' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf adminAccountId

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
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("adminAccountId" Data..= adminAccountId)
          ]
      )

instance Data.ToPath EnableOrganizationAdminAccount where
  toPath = Prelude.const "/admin"

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
