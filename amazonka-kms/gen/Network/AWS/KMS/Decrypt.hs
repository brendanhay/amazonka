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
-- Module      : Network.AWS.KMS.Decrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext that was encrypted by a AWS KMS customer master key
-- (CMK) using any of the following operations:
--
-- -   Encrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
--
-- -   GenerateDataKeyWithoutPlaintext
--
-- -   GenerateDataKeyPairWithoutPlaintext
--
-- You can use this operation to decrypt ciphertext that was encrypted
-- under a symmetric or asymmetric CMK. When the CMK is asymmetric, you
-- must specify the CMK and the encryption algorithm that was used to
-- encrypt the ciphertext. For information about symmetric and asymmetric
-- CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs>
-- in the /AWS Key Management Service Developer Guide/.
--
-- The Decrypt operation also decrypts ciphertext that was encrypted
-- outside of AWS KMS by the public key in an AWS KMS asymmetric CMK.
-- However, it cannot decrypt ciphertext produced by other libraries, such
-- as the
-- <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK>
-- or
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption>.
-- These libraries return a ciphertext format that is incompatible with AWS
-- KMS.
--
-- If the ciphertext was encrypted under a symmetric CMK, the @KeyId@
-- parameter is optional. AWS KMS can get this information from metadata
-- that it adds to the symmetric ciphertext blob. This feature adds
-- durability to your implementation by ensuring that authorized users can
-- decrypt ciphertext decades after it was encrypted, even if they\'ve lost
-- track of the CMK ID. However, specifying the CMK is always recommended
-- as a best practice. When you use the @KeyId@ parameter to specify a CMK,
-- AWS KMS only uses the CMK you specify. If the ciphertext was encrypted
-- under a different CMK, the @Decrypt@ operation fails. This practice
-- ensures that you use the CMK that you intend.
--
-- Whenever possible, use key policies to give users permission to call the
-- @Decrypt@ operation on a particular CMK, instead of using IAM policies.
-- Otherwise, you might create an IAM user policy that gives the user
-- @Decrypt@ permission on all CMKs. This user could decrypt ciphertext
-- that was encrypted by CMKs in other accounts if the key policy for the
-- cross-account CMK permits it. If you must use an IAM policy for
-- @Decrypt@ permissions, limit the user to particular CMKs or particular
-- trusted accounts. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/iam-policies.html#iam-policies-best-practices Best practices for IAM policies>
-- in the /AWS Key Management Service Developer Guide/.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. You can decrypt a ciphertext using a CMK in
-- a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:Decrypt>
-- (key policy)
--
-- __Related operations:__
--
-- -   Encrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
--
-- -   ReEncrypt
module Network.AWS.KMS.Decrypt
  ( -- * Creating a Request
    Decrypt (..),
    newDecrypt,

    -- * Request Lenses
    decrypt_grantTokens,
    decrypt_encryptionAlgorithm,
    decrypt_encryptionContext,
    decrypt_keyId,
    decrypt_ciphertextBlob,

    -- * Destructuring the Response
    DecryptResponse (..),
    newDecryptResponse,

    -- * Response Lenses
    decryptResponse_plaintext,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_keyId,
    decryptResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDecrypt' smart constructor.
data Decrypt = Decrypt'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the encryption algorithm that will be used to decrypt the
    -- ciphertext. Specify the same algorithm that was used to encrypt the
    -- data. If you specify a different algorithm, the @Decrypt@ operation
    -- fails.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@, represents
    -- the only supported algorithm that is valid for symmetric CMKs.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | Specifies the encryption context to use when decrypting the data. An
    -- encryption context is valid only for
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
    encryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the customer master key (CMK) that AWS KMS uses to decrypt the
    -- ciphertext. Enter a key ID of the CMK that was used to encrypt the
    -- ciphertext.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric CMK. If you used a symmetric CMK, AWS KMS can get the CMK
    -- from metadata that it adds to the symmetric ciphertext blob. However, it
    -- is always recommended as a best practice. This practice ensures that you
    -- use the CMK that you intend.
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
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Ciphertext to be decrypted. The blob includes metadata.
    ciphertextBlob :: Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Decrypt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'decrypt_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'encryptionAlgorithm', 'decrypt_encryptionAlgorithm' - Specifies the encryption algorithm that will be used to decrypt the
-- ciphertext. Specify the same algorithm that was used to encrypt the
-- data. If you specify a different algorithm, the @Decrypt@ operation
-- fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@, represents
-- the only supported algorithm that is valid for symmetric CMKs.
--
-- 'encryptionContext', 'decrypt_encryptionContext' - Specifies the encryption context to use when decrypting the data. An
-- encryption context is valid only for
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
-- 'keyId', 'decrypt_keyId' - Specifies the customer master key (CMK) that AWS KMS uses to decrypt the
-- ciphertext. Enter a key ID of the CMK that was used to encrypt the
-- ciphertext.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric CMK. If you used a symmetric CMK, AWS KMS can get the CMK
-- from metadata that it adds to the symmetric ciphertext blob. However, it
-- is always recommended as a best practice. This practice ensures that you
-- use the CMK that you intend.
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
-- 'ciphertextBlob', 'decrypt_ciphertextBlob' - Ciphertext to be decrypted. The blob includes metadata.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newDecrypt ::
  -- | 'ciphertextBlob'
  Prelude.ByteString ->
  Decrypt
newDecrypt pCiphertextBlob_ =
  Decrypt'
    { grantTokens = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      encryptionContext = Prelude.Nothing,
      keyId = Prelude.Nothing,
      ciphertextBlob =
        Prelude._Base64 Lens.# pCiphertextBlob_
    }

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
decrypt_grantTokens :: Lens.Lens' Decrypt (Prelude.Maybe [Prelude.Text])
decrypt_grantTokens = Lens.lens (\Decrypt' {grantTokens} -> grantTokens) (\s@Decrypt' {} a -> s {grantTokens = a} :: Decrypt) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the encryption algorithm that will be used to decrypt the
-- ciphertext. Specify the same algorithm that was used to encrypt the
-- data. If you specify a different algorithm, the @Decrypt@ operation
-- fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@, represents
-- the only supported algorithm that is valid for symmetric CMKs.
decrypt_encryptionAlgorithm :: Lens.Lens' Decrypt (Prelude.Maybe EncryptionAlgorithmSpec)
decrypt_encryptionAlgorithm = Lens.lens (\Decrypt' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@Decrypt' {} a -> s {encryptionAlgorithm = a} :: Decrypt)

-- | Specifies the encryption context to use when decrypting the data. An
-- encryption context is valid only for
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
decrypt_encryptionContext :: Lens.Lens' Decrypt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
decrypt_encryptionContext = Lens.lens (\Decrypt' {encryptionContext} -> encryptionContext) (\s@Decrypt' {} a -> s {encryptionContext = a} :: Decrypt) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the customer master key (CMK) that AWS KMS uses to decrypt the
-- ciphertext. Enter a key ID of the CMK that was used to encrypt the
-- ciphertext.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric CMK. If you used a symmetric CMK, AWS KMS can get the CMK
-- from metadata that it adds to the symmetric ciphertext blob. However, it
-- is always recommended as a best practice. This practice ensures that you
-- use the CMK that you intend.
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
decrypt_keyId :: Lens.Lens' Decrypt (Prelude.Maybe Prelude.Text)
decrypt_keyId = Lens.lens (\Decrypt' {keyId} -> keyId) (\s@Decrypt' {} a -> s {keyId = a} :: Decrypt)

-- | Ciphertext to be decrypted. The blob includes metadata.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decrypt_ciphertextBlob :: Lens.Lens' Decrypt Prelude.ByteString
decrypt_ciphertextBlob = Lens.lens (\Decrypt' {ciphertextBlob} -> ciphertextBlob) (\s@Decrypt' {} a -> s {ciphertextBlob = a} :: Decrypt) Prelude.. Prelude._Base64

instance Prelude.AWSRequest Decrypt where
  type Rs Decrypt = DecryptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DecryptResponse'
            Prelude.<$> (x Prelude..?> "Plaintext")
            Prelude.<*> (x Prelude..?> "EncryptionAlgorithm")
            Prelude.<*> (x Prelude..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Decrypt

instance Prelude.NFData Decrypt

instance Prelude.ToHeaders Decrypt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.Decrypt" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON Decrypt where
  toJSON Decrypt' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Prelude..=) Prelude.<$> grantTokens,
            ("EncryptionAlgorithm" Prelude..=)
              Prelude.<$> encryptionAlgorithm,
            ("EncryptionContext" Prelude..=)
              Prelude.<$> encryptionContext,
            ("KeyId" Prelude..=) Prelude.<$> keyId,
            Prelude.Just
              ("CiphertextBlob" Prelude..= ciphertextBlob)
          ]
      )

instance Prelude.ToPath Decrypt where
  toPath = Prelude.const "/"

instance Prelude.ToQuery Decrypt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDecryptResponse' smart constructor.
data DecryptResponse = DecryptResponse'
  { -- | Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the
    -- value is Base64-encoded. Otherwise, it is not Base64-encoded.
    plaintext :: Prelude.Maybe (Prelude.Sensitive Prelude.Base64),
    -- | The encryption algorithm that was used to decrypt the ciphertext.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK that was used to decrypt the ciphertext.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DecryptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'plaintext', 'decryptResponse_plaintext' - Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'encryptionAlgorithm', 'decryptResponse_encryptionAlgorithm' - The encryption algorithm that was used to decrypt the ciphertext.
--
-- 'keyId', 'decryptResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that was used to decrypt the ciphertext.
--
-- 'httpStatus', 'decryptResponse_httpStatus' - The response's http status code.
newDecryptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DecryptResponse
newDecryptResponse pHttpStatus_ =
  DecryptResponse'
    { plaintext = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      keyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decryptResponse_plaintext :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.ByteString)
decryptResponse_plaintext = Lens.lens (\DecryptResponse' {plaintext} -> plaintext) (\s@DecryptResponse' {} a -> s {plaintext = a} :: DecryptResponse) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Base64)

-- | The encryption algorithm that was used to decrypt the ciphertext.
decryptResponse_encryptionAlgorithm :: Lens.Lens' DecryptResponse (Prelude.Maybe EncryptionAlgorithmSpec)
decryptResponse_encryptionAlgorithm = Lens.lens (\DecryptResponse' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@DecryptResponse' {} a -> s {encryptionAlgorithm = a} :: DecryptResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that was used to decrypt the ciphertext.
decryptResponse_keyId :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.Text)
decryptResponse_keyId = Lens.lens (\DecryptResponse' {keyId} -> keyId) (\s@DecryptResponse' {} a -> s {keyId = a} :: DecryptResponse)

-- | The response's http status code.
decryptResponse_httpStatus :: Lens.Lens' DecryptResponse Prelude.Int
decryptResponse_httpStatus = Lens.lens (\DecryptResponse' {httpStatus} -> httpStatus) (\s@DecryptResponse' {} a -> s {httpStatus = a} :: DecryptResponse)

instance Prelude.NFData DecryptResponse
