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
-- Module      : Amazonka.PaymentCryptography.GetAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Amazon Web Services Payment Cryptography key associated with
-- the alias.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   DeleteAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Amazonka.PaymentCryptography.GetAlias
  ( -- * Creating a Request
    GetAlias (..),
    newGetAlias,

    -- * Request Lenses
    getAlias_aliasName,

    -- * Destructuring the Response
    GetAliasResponse (..),
    newGetAliasResponse,

    -- * Response Lenses
    getAliasResponse_httpStatus,
    getAliasResponse_alias,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAlias' smart constructor.
data GetAlias = GetAlias'
  { -- | The alias of the Amazon Web Services Payment Cryptography key.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'getAlias_aliasName' - The alias of the Amazon Web Services Payment Cryptography key.
newGetAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  GetAlias
newGetAlias pAliasName_ =
  GetAlias' {aliasName = pAliasName_}

-- | The alias of the Amazon Web Services Payment Cryptography key.
getAlias_aliasName :: Lens.Lens' GetAlias Prelude.Text
getAlias_aliasName = Lens.lens (\GetAlias' {aliasName} -> aliasName) (\s@GetAlias' {} a -> s {aliasName = a} :: GetAlias)

instance Core.AWSRequest GetAlias where
  type AWSResponse GetAlias = GetAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Alias")
      )

instance Prelude.Hashable GetAlias where
  hashWithSalt _salt GetAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasName

instance Prelude.NFData GetAlias where
  rnf GetAlias' {..} = Prelude.rnf aliasName

instance Data.ToHeaders GetAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.GetAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAlias where
  toJSON GetAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasName" Data..= aliasName)]
      )

instance Data.ToPath GetAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAliasResponse' smart constructor.
data GetAliasResponse = GetAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The alias of the Amazon Web Services Payment Cryptography key.
    alias :: Alias
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAliasResponse_httpStatus' - The response's http status code.
--
-- 'alias', 'getAliasResponse_alias' - The alias of the Amazon Web Services Payment Cryptography key.
newGetAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'alias'
  Alias ->
  GetAliasResponse
newGetAliasResponse pHttpStatus_ pAlias_ =
  GetAliasResponse'
    { httpStatus = pHttpStatus_,
      alias = pAlias_
    }

-- | The response's http status code.
getAliasResponse_httpStatus :: Lens.Lens' GetAliasResponse Prelude.Int
getAliasResponse_httpStatus = Lens.lens (\GetAliasResponse' {httpStatus} -> httpStatus) (\s@GetAliasResponse' {} a -> s {httpStatus = a} :: GetAliasResponse)

-- | The alias of the Amazon Web Services Payment Cryptography key.
getAliasResponse_alias :: Lens.Lens' GetAliasResponse Alias
getAliasResponse_alias = Lens.lens (\GetAliasResponse' {alias} -> alias) (\s@GetAliasResponse' {} a -> s {alias = a} :: GetAliasResponse)

instance Prelude.NFData GetAliasResponse where
  rnf GetAliasResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf alias
