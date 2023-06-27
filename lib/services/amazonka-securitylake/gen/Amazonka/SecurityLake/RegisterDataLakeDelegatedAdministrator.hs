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
-- Module      : Amazonka.SecurityLake.RegisterDataLakeDelegatedAdministrator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the Amazon Security Lake delegated administrator account for
-- the organization. This API can only be called by the organization
-- management account. The organization management account cannot be the
-- delegated administrator account.
module Amazonka.SecurityLake.RegisterDataLakeDelegatedAdministrator
  ( -- * Creating a Request
    RegisterDataLakeDelegatedAdministrator (..),
    newRegisterDataLakeDelegatedAdministrator,

    -- * Request Lenses
    registerDataLakeDelegatedAdministrator_accountId,

    -- * Destructuring the Response
    RegisterDataLakeDelegatedAdministratorResponse (..),
    newRegisterDataLakeDelegatedAdministratorResponse,

    -- * Response Lenses
    registerDataLakeDelegatedAdministratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newRegisterDataLakeDelegatedAdministrator' smart constructor.
data RegisterDataLakeDelegatedAdministrator = RegisterDataLakeDelegatedAdministrator'
  { -- | The Amazon Web Services account ID of the Security Lake delegated
    -- administrator.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDataLakeDelegatedAdministrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'registerDataLakeDelegatedAdministrator_accountId' - The Amazon Web Services account ID of the Security Lake delegated
-- administrator.
newRegisterDataLakeDelegatedAdministrator ::
  -- | 'accountId'
  Prelude.Text ->
  RegisterDataLakeDelegatedAdministrator
newRegisterDataLakeDelegatedAdministrator pAccountId_ =
  RegisterDataLakeDelegatedAdministrator'
    { accountId =
        pAccountId_
    }

-- | The Amazon Web Services account ID of the Security Lake delegated
-- administrator.
registerDataLakeDelegatedAdministrator_accountId :: Lens.Lens' RegisterDataLakeDelegatedAdministrator Prelude.Text
registerDataLakeDelegatedAdministrator_accountId = Lens.lens (\RegisterDataLakeDelegatedAdministrator' {accountId} -> accountId) (\s@RegisterDataLakeDelegatedAdministrator' {} a -> s {accountId = a} :: RegisterDataLakeDelegatedAdministrator)

instance
  Core.AWSRequest
    RegisterDataLakeDelegatedAdministrator
  where
  type
    AWSResponse
      RegisterDataLakeDelegatedAdministrator =
      RegisterDataLakeDelegatedAdministratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterDataLakeDelegatedAdministratorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterDataLakeDelegatedAdministrator
  where
  hashWithSalt
    _salt
    RegisterDataLakeDelegatedAdministrator' {..} =
      _salt `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    RegisterDataLakeDelegatedAdministrator
  where
  rnf RegisterDataLakeDelegatedAdministrator' {..} =
    Prelude.rnf accountId

instance
  Data.ToHeaders
    RegisterDataLakeDelegatedAdministrator
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
  Data.ToJSON
    RegisterDataLakeDelegatedAdministrator
  where
  toJSON RegisterDataLakeDelegatedAdministrator' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountId" Data..= accountId)]
      )

instance
  Data.ToPath
    RegisterDataLakeDelegatedAdministrator
  where
  toPath = Prelude.const "/v1/datalake/delegate"

instance
  Data.ToQuery
    RegisterDataLakeDelegatedAdministrator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDataLakeDelegatedAdministratorResponse' smart constructor.
data RegisterDataLakeDelegatedAdministratorResponse = RegisterDataLakeDelegatedAdministratorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDataLakeDelegatedAdministratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerDataLakeDelegatedAdministratorResponse_httpStatus' - The response's http status code.
newRegisterDataLakeDelegatedAdministratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterDataLakeDelegatedAdministratorResponse
newRegisterDataLakeDelegatedAdministratorResponse
  pHttpStatus_ =
    RegisterDataLakeDelegatedAdministratorResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
registerDataLakeDelegatedAdministratorResponse_httpStatus :: Lens.Lens' RegisterDataLakeDelegatedAdministratorResponse Prelude.Int
registerDataLakeDelegatedAdministratorResponse_httpStatus = Lens.lens (\RegisterDataLakeDelegatedAdministratorResponse' {httpStatus} -> httpStatus) (\s@RegisterDataLakeDelegatedAdministratorResponse' {} a -> s {httpStatus = a} :: RegisterDataLakeDelegatedAdministratorResponse)

instance
  Prelude.NFData
    RegisterDataLakeDelegatedAdministratorResponse
  where
  rnf
    RegisterDataLakeDelegatedAdministratorResponse' {..} =
      Prelude.rnf httpStatus
