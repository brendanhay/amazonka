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
-- Module      : Amazonka.SecurityLake.CreateDatalakeDelegatedAdmin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the Amazon Security Lake delegated administrator account for
-- the organization. This API can only be called by the organization
-- management account. The organization management account cannot be the
-- delegated administrator account.
module Amazonka.SecurityLake.CreateDatalakeDelegatedAdmin
  ( -- * Creating a Request
    CreateDatalakeDelegatedAdmin (..),
    newCreateDatalakeDelegatedAdmin,

    -- * Request Lenses
    createDatalakeDelegatedAdmin_account,

    -- * Destructuring the Response
    CreateDatalakeDelegatedAdminResponse (..),
    newCreateDatalakeDelegatedAdminResponse,

    -- * Response Lenses
    createDatalakeDelegatedAdminResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateDatalakeDelegatedAdmin' smart constructor.
data CreateDatalakeDelegatedAdmin = CreateDatalakeDelegatedAdmin'
  { -- | The Amazon Web Services account ID of the Security Lake delegated
    -- administrator.
    account :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatalakeDelegatedAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'createDatalakeDelegatedAdmin_account' - The Amazon Web Services account ID of the Security Lake delegated
-- administrator.
newCreateDatalakeDelegatedAdmin ::
  -- | 'account'
  Prelude.Text ->
  CreateDatalakeDelegatedAdmin
newCreateDatalakeDelegatedAdmin pAccount_ =
  CreateDatalakeDelegatedAdmin' {account = pAccount_}

-- | The Amazon Web Services account ID of the Security Lake delegated
-- administrator.
createDatalakeDelegatedAdmin_account :: Lens.Lens' CreateDatalakeDelegatedAdmin Prelude.Text
createDatalakeDelegatedAdmin_account = Lens.lens (\CreateDatalakeDelegatedAdmin' {account} -> account) (\s@CreateDatalakeDelegatedAdmin' {} a -> s {account = a} :: CreateDatalakeDelegatedAdmin)

instance Core.AWSRequest CreateDatalakeDelegatedAdmin where
  type
    AWSResponse CreateDatalakeDelegatedAdmin =
      CreateDatalakeDelegatedAdminResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDatalakeDelegatedAdminResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDatalakeDelegatedAdmin
  where
  hashWithSalt _salt CreateDatalakeDelegatedAdmin' {..} =
    _salt `Prelude.hashWithSalt` account

instance Prelude.NFData CreateDatalakeDelegatedAdmin where
  rnf CreateDatalakeDelegatedAdmin' {..} =
    Prelude.rnf account

instance Data.ToHeaders CreateDatalakeDelegatedAdmin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatalakeDelegatedAdmin where
  toJSON CreateDatalakeDelegatedAdmin' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("account" Data..= account)]
      )

instance Data.ToPath CreateDatalakeDelegatedAdmin where
  toPath = Prelude.const "/v1/datalake/delegate"

instance Data.ToQuery CreateDatalakeDelegatedAdmin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatalakeDelegatedAdminResponse' smart constructor.
data CreateDatalakeDelegatedAdminResponse = CreateDatalakeDelegatedAdminResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatalakeDelegatedAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDatalakeDelegatedAdminResponse_httpStatus' - The response's http status code.
newCreateDatalakeDelegatedAdminResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatalakeDelegatedAdminResponse
newCreateDatalakeDelegatedAdminResponse pHttpStatus_ =
  CreateDatalakeDelegatedAdminResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createDatalakeDelegatedAdminResponse_httpStatus :: Lens.Lens' CreateDatalakeDelegatedAdminResponse Prelude.Int
createDatalakeDelegatedAdminResponse_httpStatus = Lens.lens (\CreateDatalakeDelegatedAdminResponse' {httpStatus} -> httpStatus) (\s@CreateDatalakeDelegatedAdminResponse' {} a -> s {httpStatus = a} :: CreateDatalakeDelegatedAdminResponse)

instance
  Prelude.NFData
    CreateDatalakeDelegatedAdminResponse
  where
  rnf CreateDatalakeDelegatedAdminResponse' {..} =
    Prelude.rnf httpStatus
