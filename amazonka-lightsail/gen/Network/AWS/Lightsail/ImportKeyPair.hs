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
-- Module      : Network.AWS.Lightsail.ImportKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a public SSH key from a specific key pair.
module Network.AWS.Lightsail.ImportKeyPair
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { -- | The name of the key pair for which you want to import the public key.
    keyPairName :: Core.Text,
    -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'publicKeyBase64'
  Core.Text ->
  ImportKeyPair
newImportKeyPair pKeyPairName_ pPublicKeyBase64_ =
  ImportKeyPair'
    { keyPairName = pKeyPairName_,
      publicKeyBase64 = pPublicKeyBase64_
    }

-- | The name of the key pair for which you want to import the public key.
importKeyPair_keyPairName :: Lens.Lens' ImportKeyPair Core.Text
importKeyPair_keyPairName = Lens.lens (\ImportKeyPair' {keyPairName} -> keyPairName) (\s@ImportKeyPair' {} a -> s {keyPairName = a} :: ImportKeyPair)

-- | A base64-encoded public key of the @ssh-rsa@ type.
importKeyPair_publicKeyBase64 :: Lens.Lens' ImportKeyPair Core.Text
importKeyPair_publicKeyBase64 = Lens.lens (\ImportKeyPair' {publicKeyBase64} -> publicKeyBase64) (\s@ImportKeyPair' {} a -> s {publicKeyBase64 = a} :: ImportKeyPair)

instance Core.AWSRequest ImportKeyPair where
  type
    AWSResponse ImportKeyPair =
      ImportKeyPairResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportKeyPairResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportKeyPair

instance Core.NFData ImportKeyPair

instance Core.ToHeaders ImportKeyPair where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.ImportKeyPair" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ImportKeyPair where
  toJSON ImportKeyPair' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("keyPairName" Core..= keyPairName),
            Core.Just
              ("publicKeyBase64" Core..= publicKeyBase64)
          ]
      )

instance Core.ToPath ImportKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery ImportKeyPair where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ImportKeyPairResponse
newImportKeyPairResponse pHttpStatus_ =
  ImportKeyPairResponse'
    { operation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
importKeyPairResponse_operation :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Operation)
importKeyPairResponse_operation = Lens.lens (\ImportKeyPairResponse' {operation} -> operation) (\s@ImportKeyPairResponse' {} a -> s {operation = a} :: ImportKeyPairResponse)

-- | The response's http status code.
importKeyPairResponse_httpStatus :: Lens.Lens' ImportKeyPairResponse Core.Int
importKeyPairResponse_httpStatus = Lens.lens (\ImportKeyPairResponse' {httpStatus} -> httpStatus) (\s@ImportKeyPairResponse' {} a -> s {httpStatus = a} :: ImportKeyPairResponse)

instance Core.NFData ImportKeyPairResponse
