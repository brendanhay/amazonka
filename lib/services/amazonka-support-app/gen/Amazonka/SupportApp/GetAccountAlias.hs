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
-- Module      : Amazonka.SupportApp.GetAccountAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the alias from an Amazon Web Services account ID. The alias
-- appears in the Amazon Web Services Support App page of the Amazon Web
-- Services Support Center. The alias also appears in Slack messages from
-- the Amazon Web Services Support App.
module Amazonka.SupportApp.GetAccountAlias
  ( -- * Creating a Request
    GetAccountAlias (..),
    newGetAccountAlias,

    -- * Destructuring the Response
    GetAccountAliasResponse (..),
    newGetAccountAliasResponse,

    -- * Response Lenses
    getAccountAliasResponse_accountAlias,
    getAccountAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newGetAccountAlias' smart constructor.
data GetAccountAlias = GetAccountAlias'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountAlias ::
  GetAccountAlias
newGetAccountAlias = GetAccountAlias'

instance Core.AWSRequest GetAccountAlias where
  type
    AWSResponse GetAccountAlias =
      GetAccountAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountAliasResponse'
            Prelude.<$> (x Data..?> "accountAlias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountAlias where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountAlias where
  rnf _ = ()

instance Data.ToHeaders GetAccountAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAccountAlias where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetAccountAlias where
  toPath = Prelude.const "/control/get-account-alias"

instance Data.ToQuery GetAccountAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountAliasResponse' smart constructor.
data GetAccountAliasResponse = GetAccountAliasResponse'
  { -- | An alias or short name for an Amazon Web Services account.
    accountAlias :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAlias', 'getAccountAliasResponse_accountAlias' - An alias or short name for an Amazon Web Services account.
--
-- 'httpStatus', 'getAccountAliasResponse_httpStatus' - The response's http status code.
newGetAccountAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountAliasResponse
newGetAccountAliasResponse pHttpStatus_ =
  GetAccountAliasResponse'
    { accountAlias =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An alias or short name for an Amazon Web Services account.
getAccountAliasResponse_accountAlias :: Lens.Lens' GetAccountAliasResponse (Prelude.Maybe Prelude.Text)
getAccountAliasResponse_accountAlias = Lens.lens (\GetAccountAliasResponse' {accountAlias} -> accountAlias) (\s@GetAccountAliasResponse' {} a -> s {accountAlias = a} :: GetAccountAliasResponse)

-- | The response's http status code.
getAccountAliasResponse_httpStatus :: Lens.Lens' GetAccountAliasResponse Prelude.Int
getAccountAliasResponse_httpStatus = Lens.lens (\GetAccountAliasResponse' {httpStatus} -> httpStatus) (\s@GetAccountAliasResponse' {} a -> s {httpStatus = a} :: GetAccountAliasResponse)

instance Prelude.NFData GetAccountAliasResponse where
  rnf GetAccountAliasResponse' {..} =
    Prelude.rnf accountAlias
      `Prelude.seq` Prelude.rnf httpStatus
