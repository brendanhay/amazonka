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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
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
    createKeyPairResponse_keyPair,
    createKeyPairResponse_operation,
    createKeyPairResponse_publicKeyBase64,
    createKeyPairResponse_privateKeyBase64,
    createKeyPairResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name for your new key pair.
    keyPairName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreateKeyPair
newCreateKeyPair pKeyPairName_ =
  CreateKeyPair'
    { tags = Prelude.Nothing,
      keyPairName = pKeyPairName_
    }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createKeyPair_tags :: Lens.Lens' CreateKeyPair (Prelude.Maybe [Tag])
createKeyPair_tags = Lens.lens (\CreateKeyPair' {tags} -> tags) (\s@CreateKeyPair' {} a -> s {tags = a} :: CreateKeyPair) Prelude.. Lens.mapping Lens.coerced

-- | The name for your new key pair.
createKeyPair_keyPairName :: Lens.Lens' CreateKeyPair Prelude.Text
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
            Prelude.<$> (x Core..?> "keyPair")
            Prelude.<*> (x Core..?> "operation")
            Prelude.<*> (x Core..?> "publicKeyBase64")
            Prelude.<*> (x Core..?> "privateKeyBase64")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKeyPair

instance Prelude.NFData CreateKeyPair

instance Core.ToHeaders CreateKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateKeyPair" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateKeyPair where
  toJSON CreateKeyPair' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("keyPairName" Core..= keyPairName)
          ]
      )

instance Core.ToPath CreateKeyPair where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { -- | An array of key-value pairs containing information about the new key
    -- pair you just created.
    keyPair :: Prelude.Maybe KeyPair,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Prelude.Maybe Prelude.Text,
    -- | A base64-encoded RSA private key.
    privateKeyBase64 :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPair', 'createKeyPairResponse_keyPair' - An array of key-value pairs containing information about the new key
-- pair you just created.
--
-- 'operation', 'createKeyPairResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'publicKeyBase64', 'createKeyPairResponse_publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
--
-- 'privateKeyBase64', 'createKeyPairResponse_privateKeyBase64' - A base64-encoded RSA private key.
--
-- 'httpStatus', 'createKeyPairResponse_httpStatus' - The response's http status code.
newCreateKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKeyPairResponse
newCreateKeyPairResponse pHttpStatus_ =
  CreateKeyPairResponse'
    { keyPair = Prelude.Nothing,
      operation = Prelude.Nothing,
      publicKeyBase64 = Prelude.Nothing,
      privateKeyBase64 = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the new key
-- pair you just created.
createKeyPairResponse_keyPair :: Lens.Lens' CreateKeyPairResponse (Prelude.Maybe KeyPair)
createKeyPairResponse_keyPair = Lens.lens (\CreateKeyPairResponse' {keyPair} -> keyPair) (\s@CreateKeyPairResponse' {} a -> s {keyPair = a} :: CreateKeyPairResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createKeyPairResponse_operation :: Lens.Lens' CreateKeyPairResponse (Prelude.Maybe Operation)
createKeyPairResponse_operation = Lens.lens (\CreateKeyPairResponse' {operation} -> operation) (\s@CreateKeyPairResponse' {} a -> s {operation = a} :: CreateKeyPairResponse)

-- | A base64-encoded public key of the @ssh-rsa@ type.
createKeyPairResponse_publicKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Prelude.Maybe Prelude.Text)
createKeyPairResponse_publicKeyBase64 = Lens.lens (\CreateKeyPairResponse' {publicKeyBase64} -> publicKeyBase64) (\s@CreateKeyPairResponse' {} a -> s {publicKeyBase64 = a} :: CreateKeyPairResponse)

-- | A base64-encoded RSA private key.
createKeyPairResponse_privateKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Prelude.Maybe Prelude.Text)
createKeyPairResponse_privateKeyBase64 = Lens.lens (\CreateKeyPairResponse' {privateKeyBase64} -> privateKeyBase64) (\s@CreateKeyPairResponse' {} a -> s {privateKeyBase64 = a} :: CreateKeyPairResponse)

-- | The response's http status code.
createKeyPairResponse_httpStatus :: Lens.Lens' CreateKeyPairResponse Prelude.Int
createKeyPairResponse_httpStatus = Lens.lens (\CreateKeyPairResponse' {httpStatus} -> httpStatus) (\s@CreateKeyPairResponse' {} a -> s {httpStatus = a} :: CreateKeyPairResponse)

instance Prelude.NFData CreateKeyPairResponse
