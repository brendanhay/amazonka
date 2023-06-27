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
-- Module      : Amazonka.PaymentCryptography.UpdateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing Amazon Web Services Payment Cryptography alias
-- with a different key. Each alias is associated with only one Amazon Web
-- Services Payment Cryptography key at a time, although a key can have
-- multiple aliases. The alias and the Amazon Web Services Payment
-- Cryptography key must be in the same Amazon Web Services account and
-- Amazon Web Services Region
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
-- -   GetAlias
--
-- -   ListAliases
module Amazonka.PaymentCryptography.UpdateAlias
  ( -- * Creating a Request
    UpdateAlias (..),
    newUpdateAlias,

    -- * Request Lenses
    updateAlias_keyArn,
    updateAlias_aliasName,

    -- * Destructuring the Response
    UpdateAliasResponse (..),
    newUpdateAliasResponse,

    -- * Response Lenses
    updateAliasResponse_httpStatus,
    updateAliasResponse_alias,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | The @KeyARN@ for the key that you are updating or removing from the
    -- alias.
    keyArn :: Prelude.Maybe Prelude.Text,
    -- | The alias whose associated key is changing.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'updateAlias_keyArn' - The @KeyARN@ for the key that you are updating or removing from the
-- alias.
--
-- 'aliasName', 'updateAlias_aliasName' - The alias whose associated key is changing.
newUpdateAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  UpdateAlias
newUpdateAlias pAliasName_ =
  UpdateAlias'
    { keyArn = Prelude.Nothing,
      aliasName = pAliasName_
    }

-- | The @KeyARN@ for the key that you are updating or removing from the
-- alias.
updateAlias_keyArn :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_keyArn = Lens.lens (\UpdateAlias' {keyArn} -> keyArn) (\s@UpdateAlias' {} a -> s {keyArn = a} :: UpdateAlias)

-- | The alias whose associated key is changing.
updateAlias_aliasName :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_aliasName = Lens.lens (\UpdateAlias' {aliasName} -> aliasName) (\s@UpdateAlias' {} a -> s {aliasName = a} :: UpdateAlias)

instance Core.AWSRequest UpdateAlias where
  type AWSResponse UpdateAlias = UpdateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Alias")
      )

instance Prelude.Hashable UpdateAlias where
  hashWithSalt _salt UpdateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` aliasName

instance Prelude.NFData UpdateAlias where
  rnf UpdateAlias' {..} =
    Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf aliasName

instance Data.ToHeaders UpdateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.UpdateAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyArn" Data..=) Prelude.<$> keyArn,
            Prelude.Just ("AliasName" Data..= aliasName)
          ]
      )

instance Data.ToPath UpdateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The alias name.
    alias :: Alias
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAliasResponse_httpStatus' - The response's http status code.
--
-- 'alias', 'updateAliasResponse_alias' - The alias name.
newUpdateAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'alias'
  Alias ->
  UpdateAliasResponse
newUpdateAliasResponse pHttpStatus_ pAlias_ =
  UpdateAliasResponse'
    { httpStatus = pHttpStatus_,
      alias = pAlias_
    }

-- | The response's http status code.
updateAliasResponse_httpStatus :: Lens.Lens' UpdateAliasResponse Prelude.Int
updateAliasResponse_httpStatus = Lens.lens (\UpdateAliasResponse' {httpStatus} -> httpStatus) (\s@UpdateAliasResponse' {} a -> s {httpStatus = a} :: UpdateAliasResponse)

-- | The alias name.
updateAliasResponse_alias :: Lens.Lens' UpdateAliasResponse Alias
updateAliasResponse_alias = Lens.lens (\UpdateAliasResponse' {alias} -> alias) (\s@UpdateAliasResponse' {} a -> s {alias = a} :: UpdateAliasResponse)

instance Prelude.NFData UpdateAliasResponse where
  rnf UpdateAliasResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf alias
