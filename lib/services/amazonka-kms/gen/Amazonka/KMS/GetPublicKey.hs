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
-- Module      : Amazonka.KMS.GetPublicKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the public key of an asymmetric KMS key. Unlike the private key
-- of a asymmetric KMS key, which never leaves KMS unencrypted, callers
-- with @kms:GetPublicKey@ permission can download the public key of an
-- asymmetric KMS key. You can share the public key to allow others to
-- encrypt messages and verify signatures outside of KMS. For information
-- about asymmetric KMS keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Asymmetric KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- You do not need to download the public key. Instead, you can use the
-- public key within KMS by calling the Encrypt, ReEncrypt, or Verify
-- operations with the identifier of an asymmetric KMS key. When you use
-- the public key within KMS, you benefit from the authentication,
-- authorization, and logging that are part of every KMS operation. You
-- also reduce of risk of encrypting data that cannot be decrypted. These
-- features are not effective outside of KMS.
--
-- To verify a signature outside of KMS with an SM2 public key (China
-- Regions only), you must specify the distinguishing ID. By default, KMS
-- uses @1234567812345678@ as the distinguishing ID. For more information,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/asymmetric-key-specs.html#key-spec-sm-offline-verification Offline verification with SM2 key pairs>.
--
-- To help you use the public key safely outside of KMS, @GetPublicKey@
-- returns important information about the public key in the response,
-- including:
--
-- -   <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-KeySpec KeySpec>:
--     The type of key material in the public key, such as @RSA_4096@ or
--     @ECC_NIST_P521@.
--
-- -   <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-KeyUsage KeyUsage>:
--     Whether the key is used for encryption or signing.
--
-- -   <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-EncryptionAlgorithms EncryptionAlgorithms>
--     or
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-SigningAlgorithms SigningAlgorithms>:
--     A list of the encryption algorithms or the signing algorithms for
--     the key.
--
-- Although KMS cannot enforce these restrictions on external operations,
-- it is crucial that you use this information to prevent the public key
-- from being used improperly. For example, you can prevent a public
-- signing key from being used encrypt data, or prevent a public key from
-- being used with an encryption algorithm that is not supported by KMS.
-- You can also avoid errors, such as using the wrong signing algorithm in
-- a verification operation.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation with a KMS key in
-- a different Amazon Web Services account, specify the key ARN or alias
-- ARN in the value of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GetPublicKey>
-- (key policy)
--
-- __Related operations__: CreateKey
module Amazonka.KMS.GetPublicKey
  ( -- * Creating a Request
    GetPublicKey (..),
    newGetPublicKey,

    -- * Request Lenses
    getPublicKey_grantTokens,
    getPublicKey_keyId,

    -- * Destructuring the Response
    GetPublicKeyResponse (..),
    newGetPublicKeyResponse,

    -- * Response Lenses
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_encryptionAlgorithms,
    getPublicKeyResponse_customerMasterKeySpec,
    getPublicKeyResponse_keyUsage,
    getPublicKeyResponse_keySpec,
    getPublicKeyResponse_keyId,
    getPublicKeyResponse_signingAlgorithms,
    getPublicKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPublicKey' smart constructor.
data GetPublicKey = GetPublicKey'
  { -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Identifies the asymmetric KMS key that includes the public key.
    --
    -- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
    -- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
    -- key in a different Amazon Web Services account, you must use the key ARN
    -- or alias ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey. To get the alias name and alias ARN, use ListAliases.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'getPublicKey_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'keyId', 'getPublicKey_keyId' - Identifies the asymmetric KMS key that includes the public key.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
newGetPublicKey ::
  -- | 'keyId'
  Prelude.Text ->
  GetPublicKey
newGetPublicKey pKeyId_ =
  GetPublicKey'
    { grantTokens = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
getPublicKey_grantTokens :: Lens.Lens' GetPublicKey (Prelude.Maybe [Prelude.Text])
getPublicKey_grantTokens = Lens.lens (\GetPublicKey' {grantTokens} -> grantTokens) (\s@GetPublicKey' {} a -> s {grantTokens = a} :: GetPublicKey) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the asymmetric KMS key that includes the public key.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
getPublicKey_keyId :: Lens.Lens' GetPublicKey Prelude.Text
getPublicKey_keyId = Lens.lens (\GetPublicKey' {keyId} -> keyId) (\s@GetPublicKey' {} a -> s {keyId = a} :: GetPublicKey)

instance Core.AWSRequest GetPublicKey where
  type AWSResponse GetPublicKey = GetPublicKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPublicKeyResponse'
            Prelude.<$> (x Core..?> "PublicKey")
            Prelude.<*> ( x Core..?> "EncryptionAlgorithms"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "CustomerMasterKeySpec")
            Prelude.<*> (x Core..?> "KeyUsage")
            Prelude.<*> (x Core..?> "KeySpec")
            Prelude.<*> (x Core..?> "KeyId")
            Prelude.<*> ( x Core..?> "SigningAlgorithms"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPublicKey where
  hashWithSalt _salt GetPublicKey' {..} =
    _salt `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData GetPublicKey where
  rnf GetPublicKey' {..} =
    Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf keyId

instance Core.ToHeaders GetPublicKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.GetPublicKey" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPublicKey where
  toJSON GetPublicKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Core..=) Prelude.<$> grantTokens,
            Prelude.Just ("KeyId" Core..= keyId)
          ]
      )

instance Core.ToPath GetPublicKey where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { -- | The exported public key.
    --
    -- The value is a DER-encoded X.509 public key, also known as
    -- @SubjectPublicKeyInfo@ (SPKI), as defined in
    -- <https://tools.ietf.org/html/rfc5280 RFC 5280>. When you use the HTTP
    -- API or the Amazon Web Services CLI, the value is Base64-encoded.
    -- Otherwise, it is not Base64-encoded.
    publicKey :: Prelude.Maybe Core.Base64,
    -- | The encryption algorithms that KMS supports for this key.
    --
    -- This information is critical. If a public key encrypts data outside of
    -- KMS by using an unsupported encryption algorithm, the ciphertext cannot
    -- be decrypted.
    --
    -- This field appears in the response only when the @KeyUsage@ of the
    -- public key is @ENCRYPT_DECRYPT@.
    encryptionAlgorithms :: Prelude.Maybe [EncryptionAlgorithmSpec],
    -- | Instead, use the @KeySpec@ field in the @GetPublicKey@ response.
    --
    -- The @KeySpec@ and @CustomerMasterKeySpec@ fields have the same value. We
    -- recommend that you use the @KeySpec@ field in your code. However, to
    -- avoid breaking changes, KMS will support both fields.
    customerMasterKeySpec :: Prelude.Maybe CustomerMasterKeySpec,
    -- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@
    -- or @SIGN_VERIFY@.
    --
    -- This information is critical. If a public key with @SIGN_VERIFY@ key
    -- usage encrypts data outside of KMS, the ciphertext cannot be decrypted.
    keyUsage :: Prelude.Maybe KeyUsageType,
    -- | The type of the of the public key that was downloaded.
    keySpec :: Prelude.Maybe KeySpec,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the asymmetric KMS key from which the public key was downloaded.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The signing algorithms that KMS supports for this key.
    --
    -- This field appears in the response only when the @KeyUsage@ of the
    -- public key is @SIGN_VERIFY@.
    signingAlgorithms :: Prelude.Maybe [SigningAlgorithmSpec],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKey', 'getPublicKeyResponse_publicKey' - The exported public key.
--
-- The value is a DER-encoded X.509 public key, also known as
-- @SubjectPublicKeyInfo@ (SPKI), as defined in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>. When you use the HTTP
-- API or the Amazon Web Services CLI, the value is Base64-encoded.
-- Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'encryptionAlgorithms', 'getPublicKeyResponse_encryptionAlgorithms' - The encryption algorithms that KMS supports for this key.
--
-- This information is critical. If a public key encrypts data outside of
-- KMS by using an unsupported encryption algorithm, the ciphertext cannot
-- be decrypted.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @ENCRYPT_DECRYPT@.
--
-- 'customerMasterKeySpec', 'getPublicKeyResponse_customerMasterKeySpec' - Instead, use the @KeySpec@ field in the @GetPublicKey@ response.
--
-- The @KeySpec@ and @CustomerMasterKeySpec@ fields have the same value. We
-- recommend that you use the @KeySpec@ field in your code. However, to
-- avoid breaking changes, KMS will support both fields.
--
-- 'keyUsage', 'getPublicKeyResponse_keyUsage' - The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@
-- or @SIGN_VERIFY@.
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key
-- usage encrypts data outside of KMS, the ciphertext cannot be decrypted.
--
-- 'keySpec', 'getPublicKeyResponse_keySpec' - The type of the of the public key that was downloaded.
--
-- 'keyId', 'getPublicKeyResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric KMS key from which the public key was downloaded.
--
-- 'signingAlgorithms', 'getPublicKeyResponse_signingAlgorithms' - The signing algorithms that KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @SIGN_VERIFY@.
--
-- 'httpStatus', 'getPublicKeyResponse_httpStatus' - The response's http status code.
newGetPublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPublicKeyResponse
newGetPublicKeyResponse pHttpStatus_ =
  GetPublicKeyResponse'
    { publicKey = Prelude.Nothing,
      encryptionAlgorithms = Prelude.Nothing,
      customerMasterKeySpec = Prelude.Nothing,
      keyUsage = Prelude.Nothing,
      keySpec = Prelude.Nothing,
      keyId = Prelude.Nothing,
      signingAlgorithms = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The exported public key.
--
-- The value is a DER-encoded X.509 public key, also known as
-- @SubjectPublicKeyInfo@ (SPKI), as defined in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>. When you use the HTTP
-- API or the Amazon Web Services CLI, the value is Base64-encoded.
-- Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getPublicKeyResponse_publicKey :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe Prelude.ByteString)
getPublicKeyResponse_publicKey = Lens.lens (\GetPublicKeyResponse' {publicKey} -> publicKey) (\s@GetPublicKeyResponse' {} a -> s {publicKey = a} :: GetPublicKeyResponse) Prelude.. Lens.mapping Core._Base64

-- | The encryption algorithms that KMS supports for this key.
--
-- This information is critical. If a public key encrypts data outside of
-- KMS by using an unsupported encryption algorithm, the ciphertext cannot
-- be decrypted.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @ENCRYPT_DECRYPT@.
getPublicKeyResponse_encryptionAlgorithms :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe [EncryptionAlgorithmSpec])
getPublicKeyResponse_encryptionAlgorithms = Lens.lens (\GetPublicKeyResponse' {encryptionAlgorithms} -> encryptionAlgorithms) (\s@GetPublicKeyResponse' {} a -> s {encryptionAlgorithms = a} :: GetPublicKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | Instead, use the @KeySpec@ field in the @GetPublicKey@ response.
--
-- The @KeySpec@ and @CustomerMasterKeySpec@ fields have the same value. We
-- recommend that you use the @KeySpec@ field in your code. However, to
-- avoid breaking changes, KMS will support both fields.
getPublicKeyResponse_customerMasterKeySpec :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe CustomerMasterKeySpec)
getPublicKeyResponse_customerMasterKeySpec = Lens.lens (\GetPublicKeyResponse' {customerMasterKeySpec} -> customerMasterKeySpec) (\s@GetPublicKeyResponse' {} a -> s {customerMasterKeySpec = a} :: GetPublicKeyResponse)

-- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@
-- or @SIGN_VERIFY@.
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key
-- usage encrypts data outside of KMS, the ciphertext cannot be decrypted.
getPublicKeyResponse_keyUsage :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe KeyUsageType)
getPublicKeyResponse_keyUsage = Lens.lens (\GetPublicKeyResponse' {keyUsage} -> keyUsage) (\s@GetPublicKeyResponse' {} a -> s {keyUsage = a} :: GetPublicKeyResponse)

-- | The type of the of the public key that was downloaded.
getPublicKeyResponse_keySpec :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe KeySpec)
getPublicKeyResponse_keySpec = Lens.lens (\GetPublicKeyResponse' {keySpec} -> keySpec) (\s@GetPublicKeyResponse' {} a -> s {keySpec = a} :: GetPublicKeyResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric KMS key from which the public key was downloaded.
getPublicKeyResponse_keyId :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe Prelude.Text)
getPublicKeyResponse_keyId = Lens.lens (\GetPublicKeyResponse' {keyId} -> keyId) (\s@GetPublicKeyResponse' {} a -> s {keyId = a} :: GetPublicKeyResponse)

-- | The signing algorithms that KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @SIGN_VERIFY@.
getPublicKeyResponse_signingAlgorithms :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe [SigningAlgorithmSpec])
getPublicKeyResponse_signingAlgorithms = Lens.lens (\GetPublicKeyResponse' {signingAlgorithms} -> signingAlgorithms) (\s@GetPublicKeyResponse' {} a -> s {signingAlgorithms = a} :: GetPublicKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPublicKeyResponse_httpStatus :: Lens.Lens' GetPublicKeyResponse Prelude.Int
getPublicKeyResponse_httpStatus = Lens.lens (\GetPublicKeyResponse' {httpStatus} -> httpStatus) (\s@GetPublicKeyResponse' {} a -> s {httpStatus = a} :: GetPublicKeyResponse)

instance Prelude.NFData GetPublicKeyResponse where
  rnf GetPublicKeyResponse' {..} =
    Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf encryptionAlgorithms
      `Prelude.seq` Prelude.rnf customerMasterKeySpec
      `Prelude.seq` Prelude.rnf keyUsage
      `Prelude.seq` Prelude.rnf keySpec
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf signingAlgorithms
      `Prelude.seq` Prelude.rnf httpStatus
