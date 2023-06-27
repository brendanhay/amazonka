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
-- Module      : Amazonka.PaymentCryptography.CreateKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services Payment Cryptography key, a logical
-- representation of a cryptographic key, that is unique in your account
-- and Amazon Web Services Region. You use keys for cryptographic functions
-- such as encryption and decryption.
--
-- In addition to the key material used in cryptographic operations, an
-- Amazon Web Services Payment Cryptography key includes metadata such as
-- the key ARN, key usage, key origin, creation date, description, and key
-- state.
--
-- When you create a key, you specify both immutable and mutable data about
-- the key. The immutable data contains key attributes that defines the
-- scope and cryptographic operations that you can perform using the key,
-- for example key class (example: @SYMMETRIC_KEY@), key algorithm
-- (example: @TDES_2KEY@), key usage (example:
-- @TR31_P0_PIN_ENCRYPTION_KEY@) and key modes of use (example: @Encrypt@).
-- For information about valid combinations of key attributes, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/keys-validattributes.html Understanding key attributes>
-- in the /Amazon Web Services Payment Cryptography User Guide/. The
-- mutable data contained within a key includes usage timestamp and key
-- deletion timestamp and can be modified after creation.
--
-- Amazon Web Services Payment Cryptography binds key attributes to keys
-- using key blocks when you store or export them. Amazon Web Services
-- Payment Cryptography stores the key contents wrapped and never stores or
-- transmits them in the clear.
--
-- __Cross-account use__: This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   DeleteKey
--
-- -   GetKey
--
-- -   ListKeys
module Amazonka.PaymentCryptography.CreateKey
  ( -- * Creating a Request
    CreateKey (..),
    newCreateKey,

    -- * Request Lenses
    createKey_enabled,
    createKey_keyCheckValueAlgorithm,
    createKey_tags,
    createKey_exportable,
    createKey_keyAttributes,

    -- * Destructuring the Response
    CreateKeyResponse (..),
    newCreateKeyResponse,

    -- * Response Lenses
    createKeyResponse_httpStatus,
    createKeyResponse_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKey' smart constructor.
data CreateKey = CreateKey'
  { -- | Specifies whether to enable the key. If the key is enabled, it is
    -- activated for use within the service. If the key not enabled, then it is
    -- created but not activated. The default value is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The algorithm that Amazon Web Services Payment Cryptography uses to
    -- calculate the key check value (KCV) for DES and AES keys.
    --
    -- For DES key, the KCV is computed by encrypting 8 bytes, each with value
    -- \'00\', with the key to be checked and retaining the 3 highest order
    -- bytes of the encrypted result. For AES key, the KCV is computed by
    -- encrypting 8 bytes, each with value \'01\', with the key to be checked
    -- and retaining the 3 highest order bytes of the encrypted result.
    keyCheckValueAlgorithm :: Prelude.Maybe KeyCheckValueAlgorithm,
    -- | The tags to attach to the key. Each tag consists of a tag key and a tag
    -- value. Both the tag key and the tag value are required, but the tag
    -- value can be an empty (null) string. You can\'t have more than one tag
    -- on an Amazon Web Services Payment Cryptography key with the same tag
    -- key.
    --
    -- To use this parameter, you must have @TagResource@ permission.
    --
    -- Don\'t include confidential or sensitive information in this field. This
    -- field may be displayed in plaintext in CloudTrail logs and other output.
    --
    -- Tagging or untagging an Amazon Web Services Payment Cryptography key can
    -- allow or deny permission to the key.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies whether the key is exportable from the service.
    exportable :: Prelude.Bool,
    -- | The role of the key, the algorithm it supports, and the cryptographic
    -- operations allowed with the key. This data is immutable after the key is
    -- created.
    keyAttributes :: KeyAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createKey_enabled' - Specifies whether to enable the key. If the key is enabled, it is
-- activated for use within the service. If the key not enabled, then it is
-- created but not activated. The default value is enabled.
--
-- 'keyCheckValueAlgorithm', 'createKey_keyCheckValueAlgorithm' - The algorithm that Amazon Web Services Payment Cryptography uses to
-- calculate the key check value (KCV) for DES and AES keys.
--
-- For DES key, the KCV is computed by encrypting 8 bytes, each with value
-- \'00\', with the key to be checked and retaining the 3 highest order
-- bytes of the encrypted result. For AES key, the KCV is computed by
-- encrypting 8 bytes, each with value \'01\', with the key to be checked
-- and retaining the 3 highest order bytes of the encrypted result.
--
-- 'tags', 'createKey_tags' - The tags to attach to the key. Each tag consists of a tag key and a tag
-- value. Both the tag key and the tag value are required, but the tag
-- value can be an empty (null) string. You can\'t have more than one tag
-- on an Amazon Web Services Payment Cryptography key with the same tag
-- key.
--
-- To use this parameter, you must have @TagResource@ permission.
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
--
-- Tagging or untagging an Amazon Web Services Payment Cryptography key can
-- allow or deny permission to the key.
--
-- 'exportable', 'createKey_exportable' - Specifies whether the key is exportable from the service.
--
-- 'keyAttributes', 'createKey_keyAttributes' - The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
newCreateKey ::
  -- | 'exportable'
  Prelude.Bool ->
  -- | 'keyAttributes'
  KeyAttributes ->
  CreateKey
newCreateKey pExportable_ pKeyAttributes_ =
  CreateKey'
    { enabled = Prelude.Nothing,
      keyCheckValueAlgorithm = Prelude.Nothing,
      tags = Prelude.Nothing,
      exportable = pExportable_,
      keyAttributes = pKeyAttributes_
    }

-- | Specifies whether to enable the key. If the key is enabled, it is
-- activated for use within the service. If the key not enabled, then it is
-- created but not activated. The default value is enabled.
createKey_enabled :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Bool)
createKey_enabled = Lens.lens (\CreateKey' {enabled} -> enabled) (\s@CreateKey' {} a -> s {enabled = a} :: CreateKey)

-- | The algorithm that Amazon Web Services Payment Cryptography uses to
-- calculate the key check value (KCV) for DES and AES keys.
--
-- For DES key, the KCV is computed by encrypting 8 bytes, each with value
-- \'00\', with the key to be checked and retaining the 3 highest order
-- bytes of the encrypted result. For AES key, the KCV is computed by
-- encrypting 8 bytes, each with value \'01\', with the key to be checked
-- and retaining the 3 highest order bytes of the encrypted result.
createKey_keyCheckValueAlgorithm :: Lens.Lens' CreateKey (Prelude.Maybe KeyCheckValueAlgorithm)
createKey_keyCheckValueAlgorithm = Lens.lens (\CreateKey' {keyCheckValueAlgorithm} -> keyCheckValueAlgorithm) (\s@CreateKey' {} a -> s {keyCheckValueAlgorithm = a} :: CreateKey)

-- | The tags to attach to the key. Each tag consists of a tag key and a tag
-- value. Both the tag key and the tag value are required, but the tag
-- value can be an empty (null) string. You can\'t have more than one tag
-- on an Amazon Web Services Payment Cryptography key with the same tag
-- key.
--
-- To use this parameter, you must have @TagResource@ permission.
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
--
-- Tagging or untagging an Amazon Web Services Payment Cryptography key can
-- allow or deny permission to the key.
createKey_tags :: Lens.Lens' CreateKey (Prelude.Maybe [Tag])
createKey_tags = Lens.lens (\CreateKey' {tags} -> tags) (\s@CreateKey' {} a -> s {tags = a} :: CreateKey) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the key is exportable from the service.
createKey_exportable :: Lens.Lens' CreateKey Prelude.Bool
createKey_exportable = Lens.lens (\CreateKey' {exportable} -> exportable) (\s@CreateKey' {} a -> s {exportable = a} :: CreateKey)

-- | The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
createKey_keyAttributes :: Lens.Lens' CreateKey KeyAttributes
createKey_keyAttributes = Lens.lens (\CreateKey' {keyAttributes} -> keyAttributes) (\s@CreateKey' {} a -> s {keyAttributes = a} :: CreateKey)

instance Core.AWSRequest CreateKey where
  type AWSResponse CreateKey = CreateKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Key")
      )

instance Prelude.Hashable CreateKey where
  hashWithSalt _salt CreateKey' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` keyCheckValueAlgorithm
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` exportable
      `Prelude.hashWithSalt` keyAttributes

instance Prelude.NFData CreateKey where
  rnf CreateKey' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf keyCheckValueAlgorithm
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf exportable
      `Prelude.seq` Prelude.rnf keyAttributes

instance Data.ToHeaders CreateKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.CreateKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKey where
  toJSON CreateKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("KeyCheckValueAlgorithm" Data..=)
              Prelude.<$> keyCheckValueAlgorithm,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Exportable" Data..= exportable),
            Prelude.Just
              ("KeyAttributes" Data..= keyAttributes)
          ]
      )

instance Data.ToPath CreateKey where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKeyResponse' smart constructor.
data CreateKeyResponse = CreateKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The key material that contains all the key attributes.
    key :: Key
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createKeyResponse_httpStatus' - The response's http status code.
--
-- 'key', 'createKeyResponse_key' - The key material that contains all the key attributes.
newCreateKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Key ->
  CreateKeyResponse
newCreateKeyResponse pHttpStatus_ pKey_ =
  CreateKeyResponse'
    { httpStatus = pHttpStatus_,
      key = pKey_
    }

-- | The response's http status code.
createKeyResponse_httpStatus :: Lens.Lens' CreateKeyResponse Prelude.Int
createKeyResponse_httpStatus = Lens.lens (\CreateKeyResponse' {httpStatus} -> httpStatus) (\s@CreateKeyResponse' {} a -> s {httpStatus = a} :: CreateKeyResponse)

-- | The key material that contains all the key attributes.
createKeyResponse_key :: Lens.Lens' CreateKeyResponse Key
createKeyResponse_key = Lens.lens (\CreateKeyResponse' {key} -> key) (\s@CreateKeyResponse' {} a -> s {key = a} :: CreateKeyResponse)

instance Prelude.NFData CreateKeyResponse where
  rnf CreateKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
