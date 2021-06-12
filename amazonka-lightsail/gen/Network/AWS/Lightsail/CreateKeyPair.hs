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
-- Module      : Network.AWS.Lightsail.CreateKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSH key pair.
--
-- The @create key pair@ operation supports tag-based access control via
-- request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateKeyPair
  ( -- * Creating a Request
    CreateKeyPair (..),
    newCreateKeyPair,

    -- * Request Lenses
    createKeyPair_tags,
    createKeyPair_keyPairName,

    -- * Destructuring the Response
    CreateKeyPairResponse (..),
    newCreateKeyPairResponse,

    -- * Response Lenses
    createKeyPairResponse_privateKeyBase64,
    createKeyPairResponse_operation,
    createKeyPairResponse_keyPair,
    createKeyPairResponse_publicKeyBase64,
    createKeyPairResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The name for your new key pair.
    keyPairName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createKeyPair_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'keyPairName', 'createKeyPair_keyPairName' - The name for your new key pair.
newCreateKeyPair ::
  -- | 'keyPairName'
  Core.Text ->
  CreateKeyPair
newCreateKeyPair pKeyPairName_ =
  CreateKeyPair'
    { tags = Core.Nothing,
      keyPairName = pKeyPairName_
    }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createKeyPair_tags :: Lens.Lens' CreateKeyPair (Core.Maybe [Tag])
createKeyPair_tags = Lens.lens (\CreateKeyPair' {tags} -> tags) (\s@CreateKeyPair' {} a -> s {tags = a} :: CreateKeyPair) Core.. Lens.mapping Lens._Coerce

-- | The name for your new key pair.
createKeyPair_keyPairName :: Lens.Lens' CreateKeyPair Core.Text
createKeyPair_keyPairName = Lens.lens (\CreateKeyPair' {keyPairName} -> keyPairName) (\s@CreateKeyPair' {} a -> s {keyPairName = a} :: CreateKeyPair)

instance Core.AWSRequest CreateKeyPair where
  type
    AWSResponse CreateKeyPair =
      CreateKeyPairResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeyPairResponse'
            Core.<$> (x Core..?> "privateKeyBase64")
            Core.<*> (x Core..?> "operation")
            Core.<*> (x Core..?> "keyPair")
            Core.<*> (x Core..?> "publicKeyBase64")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateKeyPair

instance Core.NFData CreateKeyPair

instance Core.ToHeaders CreateKeyPair where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateKeyPair" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateKeyPair where
  toJSON CreateKeyPair' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("keyPairName" Core..= keyPairName)
          ]
      )

instance Core.ToPath CreateKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery CreateKeyPair where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { -- | A base64-encoded RSA private key.
    privateKeyBase64 :: Core.Maybe Core.Text,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | An array of key-value pairs containing information about the new key
    -- pair you just created.
    keyPair :: Core.Maybe KeyPair,
    -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKeyBase64', 'createKeyPairResponse_privateKeyBase64' - A base64-encoded RSA private key.
--
-- 'operation', 'createKeyPairResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'keyPair', 'createKeyPairResponse_keyPair' - An array of key-value pairs containing information about the new key
-- pair you just created.
--
-- 'publicKeyBase64', 'createKeyPairResponse_publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
--
-- 'httpStatus', 'createKeyPairResponse_httpStatus' - The response's http status code.
newCreateKeyPairResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateKeyPairResponse
newCreateKeyPairResponse pHttpStatus_ =
  CreateKeyPairResponse'
    { privateKeyBase64 =
        Core.Nothing,
      operation = Core.Nothing,
      keyPair = Core.Nothing,
      publicKeyBase64 = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A base64-encoded RSA private key.
createKeyPairResponse_privateKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Core.Text)
createKeyPairResponse_privateKeyBase64 = Lens.lens (\CreateKeyPairResponse' {privateKeyBase64} -> privateKeyBase64) (\s@CreateKeyPairResponse' {} a -> s {privateKeyBase64 = a} :: CreateKeyPairResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createKeyPairResponse_operation :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Operation)
createKeyPairResponse_operation = Lens.lens (\CreateKeyPairResponse' {operation} -> operation) (\s@CreateKeyPairResponse' {} a -> s {operation = a} :: CreateKeyPairResponse)

-- | An array of key-value pairs containing information about the new key
-- pair you just created.
createKeyPairResponse_keyPair :: Lens.Lens' CreateKeyPairResponse (Core.Maybe KeyPair)
createKeyPairResponse_keyPair = Lens.lens (\CreateKeyPairResponse' {keyPair} -> keyPair) (\s@CreateKeyPairResponse' {} a -> s {keyPair = a} :: CreateKeyPairResponse)

-- | A base64-encoded public key of the @ssh-rsa@ type.
createKeyPairResponse_publicKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Core.Text)
createKeyPairResponse_publicKeyBase64 = Lens.lens (\CreateKeyPairResponse' {publicKeyBase64} -> publicKeyBase64) (\s@CreateKeyPairResponse' {} a -> s {publicKeyBase64 = a} :: CreateKeyPairResponse)

-- | The response's http status code.
createKeyPairResponse_httpStatus :: Lens.Lens' CreateKeyPairResponse Core.Int
createKeyPairResponse_httpStatus = Lens.lens (\CreateKeyPairResponse' {httpStatus} -> httpStatus) (\s@CreateKeyPairResponse' {} a -> s {httpStatus = a} :: CreateKeyPairResponse)

instance Core.NFData CreateKeyPairResponse
