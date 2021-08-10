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
-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key for client-side encryption. This
-- operation returns a plaintext copy of the data key and a copy that is
-- encrypted under a customer master key (CMK) that you specify. You can
-- use the plaintext key to encrypt your data outside of AWS KMS and store
-- the encrypted data key with the encrypted data.
--
-- @GenerateDataKey@ returns a unique data key for each request. The bytes
-- in the plaintext key are not related to the caller or the CMK.
--
-- To generate a data key, specify the symmetric CMK that will be used to
-- encrypt the data key. You cannot use an asymmetric CMK to generate data
-- keys. To get the type of your CMK, use the DescribeKey operation. You
-- must also specify the length of the data key. Use either the @KeySpec@
-- or @NumberOfBytes@ parameters (but not both). For 128-bit and 256-bit
-- data keys, use the @KeySpec@ parameter.
--
-- To get only an encrypted copy of the data key, use
-- GenerateDataKeyWithoutPlaintext. To generate an asymmetric data key
-- pair, use the GenerateDataKeyPair or GenerateDataKeyPairWithoutPlaintext
-- operation. To get a cryptographically secure random byte string, use
-- GenerateRandom.
--
-- You can use the optional encryption context to add additional security
-- to the encryption operation. If you specify an @EncryptionContext@, you
-- must specify the same encryption context (a case-sensitive exact match)
-- when decrypting the encrypted data key. Otherwise, the request to
-- decrypt fails with an @InvalidCiphertextException@. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /AWS Key Management Service Developer Guide/.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __How to use your data key__
--
-- We recommend that you use the following pattern to encrypt data locally
-- in your application. You can write your own code or use a client-side
-- encryption library, such as the
-- <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK>,
-- the
-- <https://docs.aws.amazon.com/dynamodb-encryption-client/latest/devguide/ Amazon DynamoDB Encryption Client>,
-- or
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption>
-- to do these tasks for you.
--
-- To encrypt data outside of AWS KMS:
--
-- 1.  Use the @GenerateDataKey@ operation to get a data key.
--
-- 2.  Use the plaintext data key (in the @Plaintext@ field of the
--     response) to encrypt your data outside of AWS KMS. Then erase the
--     plaintext data key from memory.
--
-- 3.  Store the encrypted data key (in the @CiphertextBlob@ field of the
--     response) with the encrypted data.
--
-- To decrypt data outside of AWS KMS:
--
-- 1.  Use the Decrypt operation to decrypt the encrypted data key. The
--     operation returns a plaintext copy of the data key.
--
-- 2.  Use the plaintext data key to decrypt data outside of AWS KMS, then
--     erase the plaintext data key from memory.
--
-- __Cross-account use__: Yes. To perform this operation with a CMK in a
-- different AWS account, specify the key ARN or alias ARN in the value of
-- the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GenerateDataKey>
-- (key policy)
--
-- __Related operations:__
--
-- -   Decrypt
--
-- -   Encrypt
--
-- -   GenerateDataKeyPair
--
-- -   GenerateDataKeyPairWithoutPlaintext
--
-- -   GenerateDataKeyWithoutPlaintext
module Network.AWS.KMS.GenerateDataKey
  ( -- * Creating a Request
    GenerateDataKey (..),
    newGenerateDataKey,

    -- * Request Lenses
    generateDataKey_grantTokens,
    generateDataKey_numberOfBytes,
    generateDataKey_encryptionContext,
    generateDataKey_keySpec,
    generateDataKey_keyId,

    -- * Destructuring the Response
    GenerateDataKeyResponse (..),
    newGenerateDataKeyResponse,

    -- * Response Lenses
    generateDataKeyResponse_httpStatus,
    generateDataKeyResponse_keyId,
    generateDataKeyResponse_plaintext,
    generateDataKeyResponse_ciphertextBlob,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateDataKey' smart constructor.
data GenerateDataKey = GenerateDataKey'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the length of the data key in bytes. For example, use the
    -- value 64 to generate a 512-bit data key (64 bytes is 512 bits). For
    -- 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@
    -- parameter.
    --
    -- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
    -- (but not both) in every @GenerateDataKey@ request.
    numberOfBytes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the encryption context that will be used when encrypting the
    -- data key.
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
    -- | Specifies the length of the data key. Use @AES_128@ to generate a
    -- 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
    --
    -- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
    -- (but not both) in every @GenerateDataKey@ request.
    keySpec :: Prelude.Maybe DataKeySpec,
    -- | Identifies the symmetric CMK that encrypts the data key.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateDataKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'generateDataKey_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'numberOfBytes', 'generateDataKey_numberOfBytes' - Specifies the length of the data key in bytes. For example, use the
-- value 64 to generate a 512-bit data key (64 bytes is 512 bits). For
-- 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@
-- parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
--
-- 'encryptionContext', 'generateDataKey_encryptionContext' - Specifies the encryption context that will be used when encrypting the
-- data key.
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
-- 'keySpec', 'generateDataKey_keySpec' - Specifies the length of the data key. Use @AES_128@ to generate a
-- 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
--
-- 'keyId', 'generateDataKey_keyId' - Identifies the symmetric CMK that encrypts the data key.
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
newGenerateDataKey ::
  -- | 'keyId'
  Prelude.Text ->
  GenerateDataKey
newGenerateDataKey pKeyId_ =
  GenerateDataKey'
    { grantTokens = Prelude.Nothing,
      numberOfBytes = Prelude.Nothing,
      encryptionContext = Prelude.Nothing,
      keySpec = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
generateDataKey_grantTokens :: Lens.Lens' GenerateDataKey (Prelude.Maybe [Prelude.Text])
generateDataKey_grantTokens = Lens.lens (\GenerateDataKey' {grantTokens} -> grantTokens) (\s@GenerateDataKey' {} a -> s {grantTokens = a} :: GenerateDataKey) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the length of the data key in bytes. For example, use the
-- value 64 to generate a 512-bit data key (64 bytes is 512 bits). For
-- 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@
-- parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
generateDataKey_numberOfBytes :: Lens.Lens' GenerateDataKey (Prelude.Maybe Prelude.Natural)
generateDataKey_numberOfBytes = Lens.lens (\GenerateDataKey' {numberOfBytes} -> numberOfBytes) (\s@GenerateDataKey' {} a -> s {numberOfBytes = a} :: GenerateDataKey)

-- | Specifies the encryption context that will be used when encrypting the
-- data key.
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
generateDataKey_encryptionContext :: Lens.Lens' GenerateDataKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
generateDataKey_encryptionContext = Lens.lens (\GenerateDataKey' {encryptionContext} -> encryptionContext) (\s@GenerateDataKey' {} a -> s {encryptionContext = a} :: GenerateDataKey) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the length of the data key. Use @AES_128@ to generate a
-- 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
generateDataKey_keySpec :: Lens.Lens' GenerateDataKey (Prelude.Maybe DataKeySpec)
generateDataKey_keySpec = Lens.lens (\GenerateDataKey' {keySpec} -> keySpec) (\s@GenerateDataKey' {} a -> s {keySpec = a} :: GenerateDataKey)

-- | Identifies the symmetric CMK that encrypts the data key.
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
generateDataKey_keyId :: Lens.Lens' GenerateDataKey Prelude.Text
generateDataKey_keyId = Lens.lens (\GenerateDataKey' {keyId} -> keyId) (\s@GenerateDataKey' {} a -> s {keyId = a} :: GenerateDataKey)

instance Core.AWSRequest GenerateDataKey where
  type
    AWSResponse GenerateDataKey =
      GenerateDataKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "KeyId")
            Prelude.<*> (x Core..:> "Plaintext")
            Prelude.<*> (x Core..:> "CiphertextBlob")
      )

instance Prelude.Hashable GenerateDataKey

instance Prelude.NFData GenerateDataKey

instance Core.ToHeaders GenerateDataKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.GenerateDataKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GenerateDataKey where
  toJSON GenerateDataKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Core..=) Prelude.<$> grantTokens,
            ("NumberOfBytes" Core..=) Prelude.<$> numberOfBytes,
            ("EncryptionContext" Core..=)
              Prelude.<$> encryptionContext,
            ("KeySpec" Core..=) Prelude.<$> keySpec,
            Prelude.Just ("KeyId" Core..= keyId)
          ]
      )

instance Core.ToPath GenerateDataKey where
  toPath = Prelude.const "/"

instance Core.ToQuery GenerateDataKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateDataKeyResponse' smart constructor.
data GenerateDataKeyResponse = GenerateDataKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK that encrypted the data key.
    keyId :: Prelude.Text,
    -- | The plaintext data key. When you use the HTTP API or the AWS CLI, the
    -- value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this
    -- data key to encrypt your data outside of KMS. Then, remove it from
    -- memory as soon as possible.
    plaintext :: Core.Sensitive Core.Base64,
    -- | The encrypted copy of the data key. When you use the HTTP API or the AWS
    -- CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateDataKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'generateDataKeyResponse_httpStatus' - The response's http status code.
--
-- 'keyId', 'generateDataKeyResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that encrypted the data key.
--
-- 'plaintext', 'generateDataKeyResponse_plaintext' - The plaintext data key. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this
-- data key to encrypt your data outside of KMS. Then, remove it from
-- memory as soon as possible.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'ciphertextBlob', 'generateDataKeyResponse_ciphertextBlob' - The encrypted copy of the data key. When you use the HTTP API or the AWS
-- CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newGenerateDataKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyId'
  Prelude.Text ->
  -- | 'plaintext'
  Prelude.ByteString ->
  -- | 'ciphertextBlob'
  Prelude.ByteString ->
  GenerateDataKeyResponse
newGenerateDataKeyResponse
  pHttpStatus_
  pKeyId_
  pPlaintext_
  pCiphertextBlob_ =
    GenerateDataKeyResponse'
      { httpStatus = pHttpStatus_,
        keyId = pKeyId_,
        plaintext =
          Core._Sensitive Prelude.. Core._Base64
            Lens.# pPlaintext_,
        ciphertextBlob =
          Core._Base64 Lens.# pCiphertextBlob_
      }

-- | The response's http status code.
generateDataKeyResponse_httpStatus :: Lens.Lens' GenerateDataKeyResponse Prelude.Int
generateDataKeyResponse_httpStatus = Lens.lens (\GenerateDataKeyResponse' {httpStatus} -> httpStatus) (\s@GenerateDataKeyResponse' {} a -> s {httpStatus = a} :: GenerateDataKeyResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that encrypted the data key.
generateDataKeyResponse_keyId :: Lens.Lens' GenerateDataKeyResponse Prelude.Text
generateDataKeyResponse_keyId = Lens.lens (\GenerateDataKeyResponse' {keyId} -> keyId) (\s@GenerateDataKeyResponse' {} a -> s {keyId = a} :: GenerateDataKeyResponse)

-- | The plaintext data key. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this
-- data key to encrypt your data outside of KMS. Then, remove it from
-- memory as soon as possible.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyResponse_plaintext :: Lens.Lens' GenerateDataKeyResponse Prelude.ByteString
generateDataKeyResponse_plaintext = Lens.lens (\GenerateDataKeyResponse' {plaintext} -> plaintext) (\s@GenerateDataKeyResponse' {} a -> s {plaintext = a} :: GenerateDataKeyResponse) Prelude.. Core._Sensitive Prelude.. Core._Base64

-- | The encrypted copy of the data key. When you use the HTTP API or the AWS
-- CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyResponse_ciphertextBlob :: Lens.Lens' GenerateDataKeyResponse Prelude.ByteString
generateDataKeyResponse_ciphertextBlob = Lens.lens (\GenerateDataKeyResponse' {ciphertextBlob} -> ciphertextBlob) (\s@GenerateDataKeyResponse' {} a -> s {ciphertextBlob = a} :: GenerateDataKeyResponse) Prelude.. Core._Base64

instance Prelude.NFData GenerateDataKeyResponse
