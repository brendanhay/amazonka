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
-- Module      : Network.AWS.KMS.Encrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts plaintext into ciphertext by using a customer master key (CMK).
-- The @Encrypt@ operation has two primary use cases:
--
-- -   You can encrypt small amounts of arbitrary data, such as a personal
--     identifier or database password, or other sensitive information.
--
-- -   You can use the @Encrypt@ operation to move encrypted data from one
--     AWS Region to another. For example, in Region A, generate a data key
--     and use the plaintext key to encrypt your data. Then, in Region A,
--     use the @Encrypt@ operation to encrypt the plaintext data key under
--     a CMK in Region B. Now, you can move the encrypted data and the
--     encrypted data key to Region B. When necessary, you can decrypt the
--     encrypted data key and the encrypted data entirely within in Region
--     B.
--
-- You don\'t need to use the @Encrypt@ operation to encrypt a data key.
-- The GenerateDataKey and GenerateDataKeyPair operations return a
-- plaintext data key and an encrypted copy of that data key.
--
-- When you encrypt data, you must specify a symmetric or asymmetric CMK to
-- use in the encryption operation. The CMK must have a @KeyUsage@ value of
-- @ENCRYPT_DECRYPT.@ To find the @KeyUsage@ of a CMK, use the DescribeKey
-- operation.
--
-- If you use a symmetric CMK, you can use an encryption context to add
-- additional security to your encryption operation. If you specify an
-- @EncryptionContext@ when encrypting data, you must specify the same
-- encryption context (a case-sensitive exact match) when decrypting the
-- data. Otherwise, the request to decrypt fails with an
-- @InvalidCiphertextException@. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /AWS Key Management Service Developer Guide/.
--
-- If you specify an asymmetric CMK, you must also specify the encryption
-- algorithm. The algorithm must be compatible with the CMK type.
--
-- When you use an asymmetric CMK to encrypt or reencrypt data, be sure to
-- record the CMK and encryption algorithm that you choose. You will be
-- required to provide the same CMK and encryption algorithm when you
-- decrypt the data. If the CMK and algorithm do not match the values used
-- to encrypt the data, the decrypt operation fails.
--
-- You are not required to supply the CMK ID and encryption algorithm when
-- you decrypt with symmetric CMKs because AWS KMS stores this information
-- in the ciphertext blob. AWS KMS cannot store metadata in ciphertext
-- generated with asymmetric keys. The standard format for asymmetric key
-- ciphertext does not include configurable fields.
--
-- The maximum size of the data that you can encrypt varies with the type
-- of CMK and the encryption algorithm that you choose.
--
-- -   Symmetric CMKs
--
--     -   @SYMMETRIC_DEFAULT@: 4096 bytes
--
-- -   @RSA_2048@
--
--     -   @RSAES_OAEP_SHA_1@: 214 bytes
--
--     -   @RSAES_OAEP_SHA_256@: 190 bytes
--
-- -   @RSA_3072@
--
--     -   @RSAES_OAEP_SHA_1@: 342 bytes
--
--     -   @RSAES_OAEP_SHA_256@: 318 bytes
--
-- -   @RSA_4096@
--
--     -   @RSAES_OAEP_SHA_1@: 470 bytes
--
--     -   @RSAES_OAEP_SHA_256@: 446 bytes
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:Encrypt>
-- (key policy)
--
-- __Related operations:__
--
-- -   Decrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
module Network.AWS.KMS.Encrypt
  ( -- * Creating a Request
    Encrypt (..),
    newEncrypt,

    -- * Request Lenses
    encrypt_grantTokens,
    encrypt_encryptionAlgorithm,
    encrypt_encryptionContext,
    encrypt_keyId,
    encrypt_plaintext,

    -- * Destructuring the Response
    EncryptResponse (..),
    newEncryptResponse,

    -- * Response Lenses
    encryptResponse_encryptionAlgorithm,
    encryptResponse_ciphertextBlob,
    encryptResponse_keyId,
    encryptResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEncrypt' smart constructor.
data Encrypt = Encrypt'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Core.Maybe [Core.Text],
    -- | Specifies the encryption algorithm that AWS KMS will use to encrypt the
    -- plaintext message. The algorithm must be compatible with the CMK that
    -- you specify.
    --
    -- This parameter is required only for asymmetric CMKs. The default value,
    -- @SYMMETRIC_DEFAULT@, is the algorithm used for symmetric CMKs. If you
    -- are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
    encryptionAlgorithm :: Core.Maybe EncryptionAlgorithmSpec,
    -- | Specifies the encryption context that will be used to encrypt the data.
    -- An encryption context is valid only for
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- with a symmetric CMK. The standard asymmetric encryption algorithms that
    -- AWS KMS uses do not support an encryption context.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs
    -- that represents additional authenticated data. When you use an
    -- encryption context to encrypt data, you must specify the same (an exact
    -- case-sensitive match) encryption context to decrypt the data. An
    -- encryption context is optional when encrypting with a symmetric CMK, but
    -- it is highly recommended.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
    -- in the /AWS Key Management Service Developer Guide/.
    encryptionContext :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A unique identifier for the customer master key (CMK).
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
    keyId :: Core.Text,
    -- | Data to be encrypted.
    plaintext :: Core.Sensitive Core.Base64
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'Encrypt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'encrypt_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'encryptionAlgorithm', 'encrypt_encryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to encrypt the
-- plaintext message. The algorithm must be compatible with the CMK that
-- you specify.
--
-- This parameter is required only for asymmetric CMKs. The default value,
-- @SYMMETRIC_DEFAULT@, is the algorithm used for symmetric CMKs. If you
-- are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
--
-- 'encryptionContext', 'encrypt_encryptionContext' - Specifies the encryption context that will be used to encrypt the data.
-- An encryption context is valid only for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- with a symmetric CMK. The standard asymmetric encryption algorithms that
-- AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs
-- that represents additional authenticated data. When you use an
-- encryption context to encrypt data, you must specify the same (an exact
-- case-sensitive match) encryption context to decrypt the data. An
-- encryption context is optional when encrypting with a symmetric CMK, but
-- it is highly recommended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'keyId', 'encrypt_keyId' - A unique identifier for the customer master key (CMK).
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
--
-- 'plaintext', 'encrypt_plaintext' - Data to be encrypted.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newEncrypt ::
  -- | 'keyId'
  Core.Text ->
  -- | 'plaintext'
  Core.ByteString ->
  Encrypt
newEncrypt pKeyId_ pPlaintext_ =
  Encrypt'
    { grantTokens = Core.Nothing,
      encryptionAlgorithm = Core.Nothing,
      encryptionContext = Core.Nothing,
      keyId = pKeyId_,
      plaintext =
        Core._Sensitive Core.. Core._Base64
          Lens.# pPlaintext_
    }

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
encrypt_grantTokens :: Lens.Lens' Encrypt (Core.Maybe [Core.Text])
encrypt_grantTokens = Lens.lens (\Encrypt' {grantTokens} -> grantTokens) (\s@Encrypt' {} a -> s {grantTokens = a} :: Encrypt) Core.. Lens.mapping Lens._Coerce

-- | Specifies the encryption algorithm that AWS KMS will use to encrypt the
-- plaintext message. The algorithm must be compatible with the CMK that
-- you specify.
--
-- This parameter is required only for asymmetric CMKs. The default value,
-- @SYMMETRIC_DEFAULT@, is the algorithm used for symmetric CMKs. If you
-- are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
encrypt_encryptionAlgorithm :: Lens.Lens' Encrypt (Core.Maybe EncryptionAlgorithmSpec)
encrypt_encryptionAlgorithm = Lens.lens (\Encrypt' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@Encrypt' {} a -> s {encryptionAlgorithm = a} :: Encrypt)

-- | Specifies the encryption context that will be used to encrypt the data.
-- An encryption context is valid only for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- with a symmetric CMK. The standard asymmetric encryption algorithms that
-- AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs
-- that represents additional authenticated data. When you use an
-- encryption context to encrypt data, you must specify the same (an exact
-- case-sensitive match) encryption context to decrypt the data. An
-- encryption context is optional when encrypting with a symmetric CMK, but
-- it is highly recommended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /AWS Key Management Service Developer Guide/.
encrypt_encryptionContext :: Lens.Lens' Encrypt (Core.Maybe (Core.HashMap Core.Text Core.Text))
encrypt_encryptionContext = Lens.lens (\Encrypt' {encryptionContext} -> encryptionContext) (\s@Encrypt' {} a -> s {encryptionContext = a} :: Encrypt) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for the customer master key (CMK).
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
encrypt_keyId :: Lens.Lens' Encrypt Core.Text
encrypt_keyId = Lens.lens (\Encrypt' {keyId} -> keyId) (\s@Encrypt' {} a -> s {keyId = a} :: Encrypt)

-- | Data to be encrypted.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
encrypt_plaintext :: Lens.Lens' Encrypt Core.ByteString
encrypt_plaintext = Lens.lens (\Encrypt' {plaintext} -> plaintext) (\s@Encrypt' {} a -> s {plaintext = a} :: Encrypt) Core.. Core._Sensitive Core.. Core._Base64

instance Core.AWSRequest Encrypt where
  type AWSResponse Encrypt = EncryptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EncryptResponse'
            Core.<$> (x Core..?> "EncryptionAlgorithm")
            Core.<*> (x Core..?> "CiphertextBlob")
            Core.<*> (x Core..?> "KeyId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable Encrypt

instance Core.NFData Encrypt

instance Core.ToHeaders Encrypt where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.Encrypt" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON Encrypt where
  toJSON Encrypt' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GrantTokens" Core..=) Core.<$> grantTokens,
            ("EncryptionAlgorithm" Core..=)
              Core.<$> encryptionAlgorithm,
            ("EncryptionContext" Core..=)
              Core.<$> encryptionContext,
            Core.Just ("KeyId" Core..= keyId),
            Core.Just ("Plaintext" Core..= plaintext)
          ]
      )

instance Core.ToPath Encrypt where
  toPath = Core.const "/"

instance Core.ToQuery Encrypt where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEncryptResponse' smart constructor.
data EncryptResponse = EncryptResponse'
  { -- | The encryption algorithm that was used to encrypt the plaintext.
    encryptionAlgorithm :: Core.Maybe EncryptionAlgorithmSpec,
    -- | The encrypted plaintext. When you use the HTTP API or the AWS CLI, the
    -- value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Core.Maybe Core.Base64,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK that was used to encrypt the plaintext.
    keyId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EncryptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionAlgorithm', 'encryptResponse_encryptionAlgorithm' - The encryption algorithm that was used to encrypt the plaintext.
--
-- 'ciphertextBlob', 'encryptResponse_ciphertextBlob' - The encrypted plaintext. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'encryptResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that was used to encrypt the plaintext.
--
-- 'httpStatus', 'encryptResponse_httpStatus' - The response's http status code.
newEncryptResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EncryptResponse
newEncryptResponse pHttpStatus_ =
  EncryptResponse'
    { encryptionAlgorithm =
        Core.Nothing,
      ciphertextBlob = Core.Nothing,
      keyId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The encryption algorithm that was used to encrypt the plaintext.
encryptResponse_encryptionAlgorithm :: Lens.Lens' EncryptResponse (Core.Maybe EncryptionAlgorithmSpec)
encryptResponse_encryptionAlgorithm = Lens.lens (\EncryptResponse' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@EncryptResponse' {} a -> s {encryptionAlgorithm = a} :: EncryptResponse)

-- | The encrypted plaintext. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
encryptResponse_ciphertextBlob :: Lens.Lens' EncryptResponse (Core.Maybe Core.ByteString)
encryptResponse_ciphertextBlob = Lens.lens (\EncryptResponse' {ciphertextBlob} -> ciphertextBlob) (\s@EncryptResponse' {} a -> s {ciphertextBlob = a} :: EncryptResponse) Core.. Lens.mapping Core._Base64

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that was used to encrypt the plaintext.
encryptResponse_keyId :: Lens.Lens' EncryptResponse (Core.Maybe Core.Text)
encryptResponse_keyId = Lens.lens (\EncryptResponse' {keyId} -> keyId) (\s@EncryptResponse' {} a -> s {keyId = a} :: EncryptResponse)

-- | The response's http status code.
encryptResponse_httpStatus :: Lens.Lens' EncryptResponse Core.Int
encryptResponse_httpStatus = Lens.lens (\EncryptResponse' {httpStatus} -> httpStatus) (\s@EncryptResponse' {} a -> s {httpStatus = a} :: EncryptResponse)

instance Core.NFData EncryptResponse
