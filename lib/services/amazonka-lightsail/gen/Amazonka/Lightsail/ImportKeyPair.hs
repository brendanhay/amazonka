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
-- Module      : Amazonka.Lightsail.ImportKeyPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a public SSH key from a specific key pair.
module Amazonka.Lightsail.ImportKeyPair
  ( -- * Creating a Request
    ImportKeyPair (..),
    newImportKeyPair,

    -- * Request Lenses
    importKeyPair_keyPairName,
    importKeyPair_publicKeyBase64,

    -- * Destructuring the Response
    ImportKeyPairResponse (..),
    newImportKeyPairResponse,

    -- * Response Lenses
    importKeyPairResponse_operation,
    importKeyPairResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { -- | The name of the key pair for which you want to import the public key.
    keyPairName :: Prelude.Text,
    -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairName', 'importKeyPair_keyPairName' - The name of the key pair for which you want to import the public key.
--
-- 'publicKeyBase64', 'importKeyPair_publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
newImportKeyPair ::
  -- | 'keyPairName'
  Prelude.Text ->
  -- | 'publicKeyBase64'
  Prelude.Text ->
  ImportKeyPair
newImportKeyPair pKeyPairName_ pPublicKeyBase64_ =
  ImportKeyPair'
    { keyPairName = pKeyPairName_,
      publicKeyBase64 = pPublicKeyBase64_
    }

-- | The name of the key pair for which you want to import the public key.
importKeyPair_keyPairName :: Lens.Lens' ImportKeyPair Prelude.Text
importKeyPair_keyPairName = Lens.lens (\ImportKeyPair' {keyPairName} -> keyPairName) (\s@ImportKeyPair' {} a -> s {keyPairName = a} :: ImportKeyPair)

-- | A base64-encoded public key of the @ssh-rsa@ type.
importKeyPair_publicKeyBase64 :: Lens.Lens' ImportKeyPair Prelude.Text
importKeyPair_publicKeyBase64 = Lens.lens (\ImportKeyPair' {publicKeyBase64} -> publicKeyBase64) (\s@ImportKeyPair' {} a -> s {publicKeyBase64 = a} :: ImportKeyPair)

instance Core.AWSRequest ImportKeyPair where
  type
    AWSResponse ImportKeyPair =
      ImportKeyPairResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportKeyPairResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportKeyPair where
  hashWithSalt _salt ImportKeyPair' {..} =
    _salt `Prelude.hashWithSalt` keyPairName
      `Prelude.hashWithSalt` publicKeyBase64

instance Prelude.NFData ImportKeyPair where
  rnf ImportKeyPair' {..} =
    Prelude.rnf keyPairName
      `Prelude.seq` Prelude.rnf publicKeyBase64

instance Data.ToHeaders ImportKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.ImportKeyPair" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportKeyPair where
  toJSON ImportKeyPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("keyPairName" Data..= keyPairName),
            Prelude.Just
              ("publicKeyBase64" Data..= publicKeyBase64)
          ]
      )

instance Data.ToPath ImportKeyPair where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'importKeyPairResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'importKeyPairResponse_httpStatus' - The response's http status code.
newImportKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportKeyPairResponse
newImportKeyPairResponse pHttpStatus_ =
  ImportKeyPairResponse'
    { operation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
importKeyPairResponse_operation :: Lens.Lens' ImportKeyPairResponse (Prelude.Maybe Operation)
importKeyPairResponse_operation = Lens.lens (\ImportKeyPairResponse' {operation} -> operation) (\s@ImportKeyPairResponse' {} a -> s {operation = a} :: ImportKeyPairResponse)

-- | The response's http status code.
importKeyPairResponse_httpStatus :: Lens.Lens' ImportKeyPairResponse Prelude.Int
importKeyPairResponse_httpStatus = Lens.lens (\ImportKeyPairResponse' {httpStatus} -> httpStatus) (\s@ImportKeyPairResponse' {} a -> s {httpStatus = a} :: ImportKeyPairResponse)

instance Prelude.NFData ImportKeyPairResponse where
  rnf ImportKeyPairResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
