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
-- Module      : Amazonka.SecurityHub.DisableOrganizationAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a Security Hub administrator account. Can only be called by the
-- organization management account.
module Amazonka.SecurityHub.DisableOrganizationAdminAccount
  ( -- * Creating a Request
    DisableOrganizationAdminAccount (..),
    newDisableOrganizationAdminAccount,

    -- * Request Lenses
    disableOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    DisableOrganizationAdminAccountResponse (..),
    newDisableOrganizationAdminAccountResponse,

    -- * Response Lenses
    disableOrganizationAdminAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDisableOrganizationAdminAccount' smart constructor.
data DisableOrganizationAdminAccount = DisableOrganizationAdminAccount'
  { -- | The Amazon Web Services account identifier of the Security Hub
    -- administrator account.
    adminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'disableOrganizationAdminAccount_adminAccountId' - The Amazon Web Services account identifier of the Security Hub
-- administrator account.
newDisableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Prelude.Text ->
  DisableOrganizationAdminAccount
newDisableOrganizationAdminAccount pAdminAccountId_ =
  DisableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The Amazon Web Services account identifier of the Security Hub
-- administrator account.
disableOrganizationAdminAccount_adminAccountId :: Lens.Lens' DisableOrganizationAdminAccount Prelude.Text
disableOrganizationAdminAccount_adminAccountId = Lens.lens (\DisableOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@DisableOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: DisableOrganizationAdminAccount)

instance
  Core.AWSRequest
    DisableOrganizationAdminAccount
  where
  type
    AWSResponse DisableOrganizationAdminAccount =
      DisableOrganizationAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableOrganizationAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    DisableOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` adminAccountId

instance
  Prelude.NFData
    DisableOrganizationAdminAccount
  where
  rnf DisableOrganizationAdminAccount' {..} =
    Prelude.rnf adminAccountId

instance
  Data.ToHeaders
    DisableOrganizationAdminAccount
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

instance Data.ToJSON DisableOrganizationAdminAccount where
  toJSON DisableOrganizationAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AdminAccountId" Data..= adminAccountId)
          ]
      )

instance Data.ToPath DisableOrganizationAdminAccount where
  toPath = Prelude.const "/organization/admin/disable"

instance Data.ToQuery DisableOrganizationAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableOrganizationAdminAccountResponse' smart constructor.
data DisableOrganizationAdminAccountResponse = DisableOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newDisableOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableOrganizationAdminAccountResponse
newDisableOrganizationAdminAccountResponse
  pHttpStatus_ =
    DisableOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disableOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' DisableOrganizationAdminAccountResponse Prelude.Int
disableOrganizationAdminAccountResponse_httpStatus = Lens.lens (\DisableOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@DisableOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: DisableOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    DisableOrganizationAdminAccountResponse
  where
  rnf DisableOrganizationAdminAccountResponse' {..} =
    Prelude.rnf httpStatus
