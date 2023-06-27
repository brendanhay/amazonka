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
-- Module      : Amazonka.PaymentCryptography.CreateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an /alias/, or a friendly name, for an Amazon Web Services
-- Payment Cryptography key. You can use an alias to identify a key in the
-- console and when you call cryptographic operations such as
-- <https://docs.aws.amazon.com/payment-cryptography/latest/DataAPIReference/API_EncryptData.html EncryptData>
-- or
-- <https://docs.aws.amazon.com/payment-cryptography/latest/DataAPIReference/API_DecryptData.html DecryptData>.
--
-- You can associate the alias with any key in the same Amazon Web Services
-- Region. Each alias is associated with only one key at a time, but a key
-- can have multiple aliases. You can\'t create an alias without a key. The
-- alias must be unique in the account and Amazon Web Services Region, but
-- you can create another alias with the same name in a different Amazon
-- Web Services Region.
--
-- To change the key that\'s associated with the alias, call UpdateAlias.
-- To delete the alias, call DeleteAlias. These operations don\'t affect
-- the underlying key. To get the alias that you created, call ListAliases.
--
-- __Cross-account use__: This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   DeleteAlias
--
-- -   GetAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Amazonka.PaymentCryptography.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_keyArn,
    createAlias_aliasName,

    -- * Destructuring the Response
    CreateAliasResponse (..),
    newCreateAliasResponse,

    -- * Response Lenses
    createAliasResponse_httpStatus,
    createAliasResponse_alias,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The @KeyARN@ of the key to associate with the alias.
    keyArn :: Prelude.Maybe Prelude.Text,
    -- | A friendly name that you can use to refer a key. An alias must begin
    -- with @alias\/@ followed by a name, for example @alias\/ExampleAlias@. It
    -- can contain only alphanumeric characters, forward slashes (\/),
    -- underscores (_), and dashes (-).
    --
    -- Don\'t include confidential or sensitive information in this field. This
    -- field may be displayed in plaintext in CloudTrail logs and other output.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'createAlias_keyArn' - The @KeyARN@ of the key to associate with the alias.
--
-- 'aliasName', 'createAlias_aliasName' - A friendly name that you can use to refer a key. An alias must begin
-- with @alias\/@ followed by a name, for example @alias\/ExampleAlias@. It
-- can contain only alphanumeric characters, forward slashes (\/),
-- underscores (_), and dashes (-).
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
newCreateAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  CreateAlias
newCreateAlias pAliasName_ =
  CreateAlias'
    { keyArn = Prelude.Nothing,
      aliasName = pAliasName_
    }

-- | The @KeyARN@ of the key to associate with the alias.
createAlias_keyArn :: Lens.Lens' CreateAlias (Prelude.Maybe Prelude.Text)
createAlias_keyArn = Lens.lens (\CreateAlias' {keyArn} -> keyArn) (\s@CreateAlias' {} a -> s {keyArn = a} :: CreateAlias)

-- | A friendly name that you can use to refer a key. An alias must begin
-- with @alias\/@ followed by a name, for example @alias\/ExampleAlias@. It
-- can contain only alphanumeric characters, forward slashes (\/),
-- underscores (_), and dashes (-).
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
createAlias_aliasName :: Lens.Lens' CreateAlias Prelude.Text
createAlias_aliasName = Lens.lens (\CreateAlias' {aliasName} -> aliasName) (\s@CreateAlias' {} a -> s {aliasName = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = CreateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Alias")
      )

instance Prelude.Hashable CreateAlias where
  hashWithSalt _salt CreateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` aliasName

instance Prelude.NFData CreateAlias where
  rnf CreateAlias' {..} =
    Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf aliasName

instance Data.ToHeaders CreateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.CreateAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyArn" Data..=) Prelude.<$> keyArn,
            Prelude.Just ("AliasName" Data..= aliasName)
          ]
      )

instance Data.ToPath CreateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The alias for the key.
    alias :: Alias
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAliasResponse_httpStatus' - The response's http status code.
--
-- 'alias', 'createAliasResponse_alias' - The alias for the key.
newCreateAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'alias'
  Alias ->
  CreateAliasResponse
newCreateAliasResponse pHttpStatus_ pAlias_ =
  CreateAliasResponse'
    { httpStatus = pHttpStatus_,
      alias = pAlias_
    }

-- | The response's http status code.
createAliasResponse_httpStatus :: Lens.Lens' CreateAliasResponse Prelude.Int
createAliasResponse_httpStatus = Lens.lens (\CreateAliasResponse' {httpStatus} -> httpStatus) (\s@CreateAliasResponse' {} a -> s {httpStatus = a} :: CreateAliasResponse)

-- | The alias for the key.
createAliasResponse_alias :: Lens.Lens' CreateAliasResponse Alias
createAliasResponse_alias = Lens.lens (\CreateAliasResponse' {alias} -> alias) (\s@CreateAliasResponse' {} a -> s {alias = a} :: CreateAliasResponse)

instance Prelude.NFData CreateAliasResponse where
  rnf CreateAliasResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf alias
