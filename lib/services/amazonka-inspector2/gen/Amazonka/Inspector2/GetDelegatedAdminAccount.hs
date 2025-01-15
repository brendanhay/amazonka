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
-- Module      : Amazonka.Inspector2.GetDelegatedAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the Amazon Inspector delegated administrator
-- for your organization.
module Amazonka.Inspector2.GetDelegatedAdminAccount
  ( -- * Creating a Request
    GetDelegatedAdminAccount (..),
    newGetDelegatedAdminAccount,

    -- * Destructuring the Response
    GetDelegatedAdminAccountResponse (..),
    newGetDelegatedAdminAccountResponse,

    -- * Response Lenses
    getDelegatedAdminAccountResponse_delegatedAdmin,
    getDelegatedAdminAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDelegatedAdminAccount' smart constructor.
data GetDelegatedAdminAccount = GetDelegatedAdminAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDelegatedAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDelegatedAdminAccount ::
  GetDelegatedAdminAccount
newGetDelegatedAdminAccount =
  GetDelegatedAdminAccount'

instance Core.AWSRequest GetDelegatedAdminAccount where
  type
    AWSResponse GetDelegatedAdminAccount =
      GetDelegatedAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDelegatedAdminAccountResponse'
            Prelude.<$> (x Data..?> "delegatedAdmin")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDelegatedAdminAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDelegatedAdminAccount where
  rnf _ = ()

instance Data.ToHeaders GetDelegatedAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDelegatedAdminAccount where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetDelegatedAdminAccount where
  toPath = Prelude.const "/delegatedadminaccounts/get"

instance Data.ToQuery GetDelegatedAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDelegatedAdminAccountResponse' smart constructor.
data GetDelegatedAdminAccountResponse = GetDelegatedAdminAccountResponse'
  { -- | The Amazon Web Services account ID of the Amazon Inspector delegated
    -- administrator.
    delegatedAdmin :: Prelude.Maybe DelegatedAdmin,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDelegatedAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegatedAdmin', 'getDelegatedAdminAccountResponse_delegatedAdmin' - The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator.
--
-- 'httpStatus', 'getDelegatedAdminAccountResponse_httpStatus' - The response's http status code.
newGetDelegatedAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDelegatedAdminAccountResponse
newGetDelegatedAdminAccountResponse pHttpStatus_ =
  GetDelegatedAdminAccountResponse'
    { delegatedAdmin =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator.
getDelegatedAdminAccountResponse_delegatedAdmin :: Lens.Lens' GetDelegatedAdminAccountResponse (Prelude.Maybe DelegatedAdmin)
getDelegatedAdminAccountResponse_delegatedAdmin = Lens.lens (\GetDelegatedAdminAccountResponse' {delegatedAdmin} -> delegatedAdmin) (\s@GetDelegatedAdminAccountResponse' {} a -> s {delegatedAdmin = a} :: GetDelegatedAdminAccountResponse)

-- | The response's http status code.
getDelegatedAdminAccountResponse_httpStatus :: Lens.Lens' GetDelegatedAdminAccountResponse Prelude.Int
getDelegatedAdminAccountResponse_httpStatus = Lens.lens (\GetDelegatedAdminAccountResponse' {httpStatus} -> httpStatus) (\s@GetDelegatedAdminAccountResponse' {} a -> s {httpStatus = a} :: GetDelegatedAdminAccountResponse)

instance
  Prelude.NFData
    GetDelegatedAdminAccountResponse
  where
  rnf GetDelegatedAdminAccountResponse' {..} =
    Prelude.rnf delegatedAdmin `Prelude.seq`
      Prelude.rnf httpStatus
