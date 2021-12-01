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
-- Module      : Amazonka.AuditManager.DeregisterOrganizationAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member Amazon Web Services account as a delegated
-- administrator for Audit Manager.
--
-- When you remove a delegated administrator from your Audit Manager
-- settings, or when you deregister a delegated administrator from
-- Organizations, you continue to have access to the evidence that you
-- previously collected under that account. However, Audit Manager will
-- stop collecting and attaching evidence to that delegated administrator
-- account moving forward.
module Amazonka.AuditManager.DeregisterOrganizationAdminAccount
  ( -- * Creating a Request
    DeregisterOrganizationAdminAccount (..),
    newDeregisterOrganizationAdminAccount,

    -- * Request Lenses
    deregisterOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    DeregisterOrganizationAdminAccountResponse (..),
    newDeregisterOrganizationAdminAccountResponse,

    -- * Response Lenses
    deregisterOrganizationAdminAccountResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterOrganizationAdminAccount' smart constructor.
data DeregisterOrganizationAdminAccount = DeregisterOrganizationAdminAccount'
  { -- | The identifier for the specified administrator account.
    adminAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'deregisterOrganizationAdminAccount_adminAccountId' - The identifier for the specified administrator account.
newDeregisterOrganizationAdminAccount ::
  DeregisterOrganizationAdminAccount
newDeregisterOrganizationAdminAccount =
  DeregisterOrganizationAdminAccount'
    { adminAccountId =
        Prelude.Nothing
    }

-- | The identifier for the specified administrator account.
deregisterOrganizationAdminAccount_adminAccountId :: Lens.Lens' DeregisterOrganizationAdminAccount (Prelude.Maybe Prelude.Text)
deregisterOrganizationAdminAccount_adminAccountId = Lens.lens (\DeregisterOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@DeregisterOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: DeregisterOrganizationAdminAccount)

instance
  Core.AWSRequest
    DeregisterOrganizationAdminAccount
  where
  type
    AWSResponse DeregisterOrganizationAdminAccount =
      DeregisterOrganizationAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterOrganizationAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterOrganizationAdminAccount
  where
  hashWithSalt
    salt'
    DeregisterOrganizationAdminAccount' {..} =
      salt' `Prelude.hashWithSalt` adminAccountId

instance
  Prelude.NFData
    DeregisterOrganizationAdminAccount
  where
  rnf DeregisterOrganizationAdminAccount' {..} =
    Prelude.rnf adminAccountId

instance
  Core.ToHeaders
    DeregisterOrganizationAdminAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeregisterOrganizationAdminAccount
  where
  toJSON DeregisterOrganizationAdminAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("adminAccountId" Core..=)
              Prelude.<$> adminAccountId
          ]
      )

instance
  Core.ToPath
    DeregisterOrganizationAdminAccount
  where
  toPath =
    Prelude.const
      "/account/deregisterOrganizationAdminAccount"

instance
  Core.ToQuery
    DeregisterOrganizationAdminAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterOrganizationAdminAccountResponse' smart constructor.
data DeregisterOrganizationAdminAccountResponse = DeregisterOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newDeregisterOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterOrganizationAdminAccountResponse
newDeregisterOrganizationAdminAccountResponse
  pHttpStatus_ =
    DeregisterOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deregisterOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' DeregisterOrganizationAdminAccountResponse Prelude.Int
deregisterOrganizationAdminAccountResponse_httpStatus = Lens.lens (\DeregisterOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@DeregisterOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: DeregisterOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    DeregisterOrganizationAdminAccountResponse
  where
  rnf DeregisterOrganizationAdminAccountResponse' {..} =
    Prelude.rnf httpStatus
