{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.GetPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the public key of an asymmetric CMK. Unlike the private key of a
-- asymmetric CMK, which never leaves AWS KMS unencrypted, callers with
-- @kms:GetPublicKey@ permission can download the public key of an
-- asymmetric CMK. You can share the public key to allow others to encrypt
-- messages and verify signatures outside of AWS KMS. For information about
-- symmetric and asymmetric CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs>
-- in the /AWS Key Management Service Developer Guide/.
--
-- You do not need to download the public key. Instead, you can use the
-- public key within AWS KMS by calling the Encrypt, ReEncrypt, or Verify
-- operations with the identifier of an asymmetric CMK. When you use the
-- public key within AWS KMS, you benefit from the authentication,
-- authorization, and logging that are part of every AWS KMS operation. You
-- also reduce of risk of encrypting data that cannot be decrypted. These
-- features are not effective outside of AWS KMS. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/download-public-key.html#download-public-key-considerations Special Considerations for Downloading Public Keys>.
--
-- To help you use the public key safely outside of AWS KMS, @GetPublicKey@
-- returns important information about the public key in the response,
-- including:
--
-- -   <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-CustomerMasterKeySpec CustomerMasterKeySpec>:
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
-- Although AWS KMS cannot enforce these restrictions on external
-- operations, it is crucial that you use this information to prevent the
-- public key from being used improperly. For example, you can prevent a
-- public signing key from being used encrypt data, or prevent a public key
-- from being used with an encryption algorithm that is not supported by
-- AWS KMS. You can also avoid errors, such as using the wrong signing
-- algorithm in a verification operation.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation with a CMK in a
-- different AWS account, specify the key ARN or alias ARN in the value of
-- the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GetPublicKey>
-- (key policy)
--
-- __Related operations__: CreateKey
module Network.AWS.KMS.GetPublicKey
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
    getPublicKeyResponse_signingAlgorithms,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_encryptionAlgorithms,
    getPublicKeyResponse_keyUsage,
    getPublicKeyResponse_keyId,
    getPublicKeyResponse_customerMasterKeySpec,
    getPublicKeyResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPublicKey' smart constructor.
data GetPublicKey = GetPublicKey'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Identifies the asymmetric CMK that includes the public key.
    --
    -- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
    -- name, or alias ARN. When using an alias name, prefix it with
    -- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
    -- the key ARN or alias ARN.
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
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
    -- get the alias name and alias ARN, use ListAliases.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'keyId', 'getPublicKey_keyId' - Identifies the asymmetric CMK that includes the public key.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. When using an alias name, prefix it with
-- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
-- the key ARN or alias ARN.
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
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
-- get the alias name and alias ARN, use ListAliases.
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
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
getPublicKey_grantTokens :: Lens.Lens' GetPublicKey (Prelude.Maybe [Prelude.Text])
getPublicKey_grantTokens = Lens.lens (\GetPublicKey' {grantTokens} -> grantTokens) (\s@GetPublicKey' {} a -> s {grantTokens = a} :: GetPublicKey) Prelude.. Lens.mapping Prelude._Coerce

-- | Identifies the asymmetric CMK that includes the public key.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias
-- name, or alias ARN. When using an alias name, prefix it with
-- @\"alias\/\"@. To specify a CMK in a different AWS account, you must use
-- the key ARN or alias ARN.
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
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey. To
-- get the alias name and alias ARN, use ListAliases.
getPublicKey_keyId :: Lens.Lens' GetPublicKey Prelude.Text
getPublicKey_keyId = Lens.lens (\GetPublicKey' {keyId} -> keyId) (\s@GetPublicKey' {} a -> s {keyId = a} :: GetPublicKey)

instance Prelude.AWSRequest GetPublicKey where
  type Rs GetPublicKey = GetPublicKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPublicKeyResponse'
            Prelude.<$> ( x Prelude..?> "SigningAlgorithms"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "PublicKey")
            Prelude.<*> ( x Prelude..?> "EncryptionAlgorithms"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "KeyUsage")
            Prelude.<*> (x Prelude..?> "KeyId")
            Prelude.<*> (x Prelude..?> "CustomerMasterKeySpec")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPublicKey

instance Prelude.NFData GetPublicKey

instance Prelude.ToHeaders GetPublicKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.GetPublicKey" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetPublicKey where
  toJSON GetPublicKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Prelude..=) Prelude.<$> grantTokens,
            Prelude.Just ("KeyId" Prelude..= keyId)
          ]
      )

instance Prelude.ToPath GetPublicKey where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetPublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { -- | The signing algorithms that AWS KMS supports for this key.
    --
    -- This field appears in the response only when the @KeyUsage@ of the
    -- public key is @SIGN_VERIFY@.
    signingAlgorithms :: Prelude.Maybe [SigningAlgorithmSpec],
    -- | The exported public key.
    --
    -- The value is a DER-encoded X.509 public key, also known as
    -- @SubjectPublicKeyInfo@ (SPKI), as defined in
    -- <https://tools.ietf.org/html/rfc5280 RFC 5280>. When you use the HTTP
    -- API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    publicKey :: Prelude.Maybe Prelude.Base64,
    -- | The encryption algorithms that AWS KMS supports for this key.
    --
    -- This information is critical. If a public key encrypts data outside of
    -- AWS KMS by using an unsupported encryption algorithm, the ciphertext
    -- cannot be decrypted.
    --
    -- This field appears in the response only when the @KeyUsage@ of the
    -- public key is @ENCRYPT_DECRYPT@.
    encryptionAlgorithms :: Prelude.Maybe [EncryptionAlgorithmSpec],
    -- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@
    -- or @SIGN_VERIFY@.
    --
    -- This information is critical. If a public key with @SIGN_VERIFY@ key
    -- usage encrypts data outside of AWS KMS, the ciphertext cannot be
    -- decrypted.
    keyUsage :: Prelude.Maybe KeyUsageType,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the asymmetric CMK from which the public key was downloaded.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The type of the of the public key that was downloaded.
    customerMasterKeySpec :: Prelude.Maybe CustomerMasterKeySpec,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingAlgorithms', 'getPublicKeyResponse_signingAlgorithms' - The signing algorithms that AWS KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @SIGN_VERIFY@.
--
-- 'publicKey', 'getPublicKeyResponse_publicKey' - The exported public key.
--
-- The value is a DER-encoded X.509 public key, also known as
-- @SubjectPublicKeyInfo@ (SPKI), as defined in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>. When you use the HTTP
-- API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'encryptionAlgorithms', 'getPublicKeyResponse_encryptionAlgorithms' - The encryption algorithms that AWS KMS supports for this key.
--
-- This information is critical. If a public key encrypts data outside of
-- AWS KMS by using an unsupported encryption algorithm, the ciphertext
-- cannot be decrypted.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @ENCRYPT_DECRYPT@.
--
-- 'keyUsage', 'getPublicKeyResponse_keyUsage' - The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@
-- or @SIGN_VERIFY@.
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key
-- usage encrypts data outside of AWS KMS, the ciphertext cannot be
-- decrypted.
--
-- 'keyId', 'getPublicKeyResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric CMK from which the public key was downloaded.
--
-- 'customerMasterKeySpec', 'getPublicKeyResponse_customerMasterKeySpec' - The type of the of the public key that was downloaded.
--
-- 'httpStatus', 'getPublicKeyResponse_httpStatus' - The response's http status code.
newGetPublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPublicKeyResponse
newGetPublicKeyResponse pHttpStatus_ =
  GetPublicKeyResponse'
    { signingAlgorithms =
        Prelude.Nothing,
      publicKey = Prelude.Nothing,
      encryptionAlgorithms = Prelude.Nothing,
      keyUsage = Prelude.Nothing,
      keyId = Prelude.Nothing,
      customerMasterKeySpec = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signing algorithms that AWS KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @SIGN_VERIFY@.
getPublicKeyResponse_signingAlgorithms :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe [SigningAlgorithmSpec])
getPublicKeyResponse_signingAlgorithms = Lens.lens (\GetPublicKeyResponse' {signingAlgorithms} -> signingAlgorithms) (\s@GetPublicKeyResponse' {} a -> s {signingAlgorithms = a} :: GetPublicKeyResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The exported public key.
--
-- The value is a DER-encoded X.509 public key, also known as
-- @SubjectPublicKeyInfo@ (SPKI), as defined in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>. When you use the HTTP
-- API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getPublicKeyResponse_publicKey :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe Prelude.ByteString)
getPublicKeyResponse_publicKey = Lens.lens (\GetPublicKeyResponse' {publicKey} -> publicKey) (\s@GetPublicKeyResponse' {} a -> s {publicKey = a} :: GetPublicKeyResponse) Prelude.. Lens.mapping Prelude._Base64

-- | The encryption algorithms that AWS KMS supports for this key.
--
-- This information is critical. If a public key encrypts data outside of
-- AWS KMS by using an unsupported encryption algorithm, the ciphertext
-- cannot be decrypted.
--
-- This field appears in the response only when the @KeyUsage@ of the
-- public key is @ENCRYPT_DECRYPT@.
getPublicKeyResponse_encryptionAlgorithms :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe [EncryptionAlgorithmSpec])
getPublicKeyResponse_encryptionAlgorithms = Lens.lens (\GetPublicKeyResponse' {encryptionAlgorithms} -> encryptionAlgorithms) (\s@GetPublicKeyResponse' {} a -> s {encryptionAlgorithms = a} :: GetPublicKeyResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@
-- or @SIGN_VERIFY@.
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key
-- usage encrypts data outside of AWS KMS, the ciphertext cannot be
-- decrypted.
getPublicKeyResponse_keyUsage :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe KeyUsageType)
getPublicKeyResponse_keyUsage = Lens.lens (\GetPublicKeyResponse' {keyUsage} -> keyUsage) (\s@GetPublicKeyResponse' {} a -> s {keyUsage = a} :: GetPublicKeyResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the asymmetric CMK from which the public key was downloaded.
getPublicKeyResponse_keyId :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe Prelude.Text)
getPublicKeyResponse_keyId = Lens.lens (\GetPublicKeyResponse' {keyId} -> keyId) (\s@GetPublicKeyResponse' {} a -> s {keyId = a} :: GetPublicKeyResponse)

-- | The type of the of the public key that was downloaded.
getPublicKeyResponse_customerMasterKeySpec :: Lens.Lens' GetPublicKeyResponse (Prelude.Maybe CustomerMasterKeySpec)
getPublicKeyResponse_customerMasterKeySpec = Lens.lens (\GetPublicKeyResponse' {customerMasterKeySpec} -> customerMasterKeySpec) (\s@GetPublicKeyResponse' {} a -> s {customerMasterKeySpec = a} :: GetPublicKeyResponse)

-- | The response's http status code.
getPublicKeyResponse_httpStatus :: Lens.Lens' GetPublicKeyResponse Prelude.Int
getPublicKeyResponse_httpStatus = Lens.lens (\GetPublicKeyResponse' {httpStatus} -> httpStatus) (\s@GetPublicKeyResponse' {} a -> s {httpStatus = a} :: GetPublicKeyResponse)

instance Prelude.NFData GetPublicKeyResponse
