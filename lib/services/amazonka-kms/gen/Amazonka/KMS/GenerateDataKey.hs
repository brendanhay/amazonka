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
-- Module      : Amazonka.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique symmetric data key for use outside of KMS. This
-- operation returns a plaintext copy of the data key and a copy that is
-- encrypted under a symmetric encryption KMS key that you specify. The
-- bytes in the plaintext key are random; they are not related to the
-- caller or the KMS key. You can use the plaintext key to encrypt your
-- data outside of KMS and store the encrypted data key with the encrypted
-- data.
--
-- To generate a data key, specify the symmetric encryption KMS key that
-- will be used to encrypt the data key. You cannot use an asymmetric KMS
-- key to encrypt data keys. To get the type of your KMS key, use the
-- DescribeKey operation.
--
-- You must also specify the length of the data key. Use either the
-- @KeySpec@ or @NumberOfBytes@ parameters (but not both). For 128-bit and
-- 256-bit data keys, use the @KeySpec@ parameter.
--
-- To generate an SM4 data key (China Regions only), specify a @KeySpec@
-- value of @AES_128@ or @NumberOfBytes@ value of @128@. The symmetric
-- encryption key used in China Regions to encrypt your data key is an SM4
-- encryption key.
--
-- To get only an encrypted copy of the data key, use
-- GenerateDataKeyWithoutPlaintext. To generate an asymmetric data key
-- pair, use the GenerateDataKeyPair or GenerateDataKeyPairWithoutPlaintext
-- operation. To get a cryptographically secure random byte string, use
-- GenerateRandom.
--
-- You can use an optional encryption context to add additional security to
-- the encryption operation. If you specify an @EncryptionContext@, you
-- must specify the same encryption context (a case-sensitive exact match)
-- when decrypting the encrypted data key. Otherwise, the request to
-- decrypt fails with an @InvalidCiphertextException@. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /Key Management Service Developer Guide/.
--
-- Applications in Amazon Web Services Nitro Enclaves can call this
-- operation by using the
-- <https://github.com/aws/aws-nitro-enclaves-sdk-c Amazon Web Services Nitro Enclaves Development Kit>.
-- For information about the supporting parameters, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves use KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __How to use your data key__
--
-- We recommend that you use the following pattern to encrypt data locally
-- in your application. You can write your own code or use a client-side
-- encryption library, such as the
-- <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ Amazon Web Services Encryption SDK>,
-- the
-- <https://docs.aws.amazon.com/dynamodb-encryption-client/latest/devguide/ Amazon DynamoDB Encryption Client>,
-- or
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption>
-- to do these tasks for you.
--
-- To encrypt data outside of KMS:
--
-- 1.  Use the @GenerateDataKey@ operation to get a data key.
--
-- 2.  Use the plaintext data key (in the @Plaintext@ field of the
--     response) to encrypt your data outside of KMS. Then erase the
--     plaintext data key from memory.
--
-- 3.  Store the encrypted data key (in the @CiphertextBlob@ field of the
--     response) with the encrypted data.
--
-- To decrypt data outside of KMS:
--
-- 1.  Use the Decrypt operation to decrypt the encrypted data key. The
--     operation returns a plaintext copy of the data key.
--
-- 2.  Use the plaintext data key to decrypt data outside of KMS, then
--     erase the plaintext data key from memory.
--
-- __Cross-account use__: Yes. To perform this operation with a KMS key in
-- a different Amazon Web Services account, specify the key ARN or alias
-- ARN in the value of the @KeyId@ parameter.
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
module Amazonka.KMS.GenerateDataKey
  ( -- * Creating a Request
    GenerateDataKey (..),
    newGenerateDataKey,

    -- * Request Lenses
    generateDataKey_encryptionContext,
    generateDataKey_grantTokens,
    generateDataKey_keySpec,
    generateDataKey_numberOfBytes,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateDataKey' smart constructor.
data GenerateDataKey = GenerateDataKey'
  { -- | Specifies the encryption context that will be used when encrypting the
    -- data key.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs
    -- that represent additional authenticated data. When you use an encryption
    -- context to encrypt data, you must specify the same (an exact
    -- case-sensitive match) encryption context to decrypt the data. An
    -- encryption context is supported only on operations with symmetric
    -- encryption KMS keys. On operations with symmetric encryption KMS keys,
    -- an encryption context is optional, but it is strongly recommended.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption context>
    -- in the /Key Management Service Developer Guide/.
    encryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the length of the data key. Use @AES_128@ to generate a
    -- 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
    --
    -- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
    -- (but not both) in every @GenerateDataKey@ request.
    keySpec :: Prelude.Maybe DataKeySpec,
    -- | Specifies the length of the data key in bytes. For example, use the
    -- value 64 to generate a 512-bit data key (64 bytes is 512 bits). For
    -- 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@
    -- parameter.
    --
    -- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
    -- (but not both) in every @GenerateDataKey@ request.
    numberOfBytes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the symmetric encryption KMS key that encrypts the data key.
    -- You cannot specify an asymmetric KMS key or a KMS key in a custom key
    -- store. To get the type and origin of your KMS key, use the DescribeKey
    -- operation.
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
-- Create a value of 'GenerateDataKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionContext', 'generateDataKey_encryptionContext' - Specifies the encryption context that will be used when encrypting the
-- data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs
-- that represent additional authenticated data. When you use an encryption
-- context to encrypt data, you must specify the same (an exact
-- case-sensitive match) encryption context to decrypt the data. An
-- encryption context is supported only on operations with symmetric
-- encryption KMS keys. On operations with symmetric encryption KMS keys,
-- an encryption context is optional, but it is strongly recommended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption context>
-- in the /Key Management Service Developer Guide/.
--
-- 'grantTokens', 'generateDataKey_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'keySpec', 'generateDataKey_keySpec' - Specifies the length of the data key. Use @AES_128@ to generate a
-- 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
--
-- 'numberOfBytes', 'generateDataKey_numberOfBytes' - Specifies the length of the data key in bytes. For example, use the
-- value 64 to generate a 512-bit data key (64 bytes is 512 bits). For
-- 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@
-- parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
--
-- 'keyId', 'generateDataKey_keyId' - Specifies the symmetric encryption KMS key that encrypts the data key.
-- You cannot specify an asymmetric KMS key or a KMS key in a custom key
-- store. To get the type and origin of your KMS key, use the DescribeKey
-- operation.
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
newGenerateDataKey ::
  -- | 'keyId'
  Prelude.Text ->
  GenerateDataKey
newGenerateDataKey pKeyId_ =
  GenerateDataKey'
    { encryptionContext =
        Prelude.Nothing,
      grantTokens = Prelude.Nothing,
      keySpec = Prelude.Nothing,
      numberOfBytes = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | Specifies the encryption context that will be used when encrypting the
-- data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs
-- that represent additional authenticated data. When you use an encryption
-- context to encrypt data, you must specify the same (an exact
-- case-sensitive match) encryption context to decrypt the data. An
-- encryption context is supported only on operations with symmetric
-- encryption KMS keys. On operations with symmetric encryption KMS keys,
-- an encryption context is optional, but it is strongly recommended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption context>
-- in the /Key Management Service Developer Guide/.
generateDataKey_encryptionContext :: Lens.Lens' GenerateDataKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
generateDataKey_encryptionContext = Lens.lens (\GenerateDataKey' {encryptionContext} -> encryptionContext) (\s@GenerateDataKey' {} a -> s {encryptionContext = a} :: GenerateDataKey) Prelude.. Lens.mapping Lens.coerced

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
generateDataKey_grantTokens :: Lens.Lens' GenerateDataKey (Prelude.Maybe [Prelude.Text])
generateDataKey_grantTokens = Lens.lens (\GenerateDataKey' {grantTokens} -> grantTokens) (\s@GenerateDataKey' {} a -> s {grantTokens = a} :: GenerateDataKey) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the length of the data key. Use @AES_128@ to generate a
-- 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
generateDataKey_keySpec :: Lens.Lens' GenerateDataKey (Prelude.Maybe DataKeySpec)
generateDataKey_keySpec = Lens.lens (\GenerateDataKey' {keySpec} -> keySpec) (\s@GenerateDataKey' {} a -> s {keySpec = a} :: GenerateDataKey)

-- | Specifies the length of the data key in bytes. For example, use the
-- value 64 to generate a 512-bit data key (64 bytes is 512 bits). For
-- 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@
-- parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter
-- (but not both) in every @GenerateDataKey@ request.
generateDataKey_numberOfBytes :: Lens.Lens' GenerateDataKey (Prelude.Maybe Prelude.Natural)
generateDataKey_numberOfBytes = Lens.lens (\GenerateDataKey' {numberOfBytes} -> numberOfBytes) (\s@GenerateDataKey' {} a -> s {numberOfBytes = a} :: GenerateDataKey)

-- | Specifies the symmetric encryption KMS key that encrypts the data key.
-- You cannot specify an asymmetric KMS key or a KMS key in a custom key
-- store. To get the type and origin of your KMS key, use the DescribeKey
-- operation.
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
generateDataKey_keyId :: Lens.Lens' GenerateDataKey Prelude.Text
generateDataKey_keyId = Lens.lens (\GenerateDataKey' {keyId} -> keyId) (\s@GenerateDataKey' {} a -> s {keyId = a} :: GenerateDataKey)

instance Core.AWSRequest GenerateDataKey where
  type
    AWSResponse GenerateDataKey =
      GenerateDataKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyId")
            Prelude.<*> (x Data..:> "Plaintext")
            Prelude.<*> (x Data..:> "CiphertextBlob")
      )

instance Prelude.Hashable GenerateDataKey where
  hashWithSalt _salt GenerateDataKey' {..} =
    _salt `Prelude.hashWithSalt` encryptionContext
      `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` keySpec
      `Prelude.hashWithSalt` numberOfBytes
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData GenerateDataKey where
  rnf GenerateDataKey' {..} =
    Prelude.rnf encryptionContext
      `Prelude.seq` Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf keySpec
      `Prelude.seq` Prelude.rnf numberOfBytes
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders GenerateDataKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.GenerateDataKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateDataKey where
  toJSON GenerateDataKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionContext" Data..=)
              Prelude.<$> encryptionContext,
            ("GrantTokens" Data..=) Prelude.<$> grantTokens,
            ("KeySpec" Data..=) Prelude.<$> keySpec,
            ("NumberOfBytes" Data..=) Prelude.<$> numberOfBytes,
            Prelude.Just ("KeyId" Data..= keyId)
          ]
      )

instance Data.ToPath GenerateDataKey where
  toPath = Prelude.const "/"

instance Data.ToQuery GenerateDataKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateDataKeyResponse' smart constructor.
data GenerateDataKeyResponse = GenerateDataKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key that encrypted the data key.
    keyId :: Prelude.Text,
    -- | The plaintext data key. When you use the HTTP API or the Amazon Web
    -- Services CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded. Use this data key to encrypt your data outside of KMS.
    -- Then, remove it from memory as soon as possible.
    plaintext :: Data.Sensitive Data.Base64,
    -- | The encrypted copy of the data key. When you use the HTTP API or the
    -- Amazon Web Services CLI, the value is Base64-encoded. Otherwise, it is
    -- not Base64-encoded.
    ciphertextBlob :: Data.Base64
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
-- of the KMS key that encrypted the data key.
--
-- 'plaintext', 'generateDataKeyResponse_plaintext' - The plaintext data key. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded. Use this data key to encrypt your data outside of KMS.
-- Then, remove it from memory as soon as possible.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'ciphertextBlob', 'generateDataKeyResponse_ciphertextBlob' - The encrypted copy of the data key. When you use the HTTP API or the
-- Amazon Web Services CLI, the value is Base64-encoded. Otherwise, it is
-- not Base64-encoded.--
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
          Data._Sensitive Prelude.. Data._Base64
            Lens.# pPlaintext_,
        ciphertextBlob =
          Data._Base64 Lens.# pCiphertextBlob_
      }

-- | The response's http status code.
generateDataKeyResponse_httpStatus :: Lens.Lens' GenerateDataKeyResponse Prelude.Int
generateDataKeyResponse_httpStatus = Lens.lens (\GenerateDataKeyResponse' {httpStatus} -> httpStatus) (\s@GenerateDataKeyResponse' {} a -> s {httpStatus = a} :: GenerateDataKeyResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that encrypted the data key.
generateDataKeyResponse_keyId :: Lens.Lens' GenerateDataKeyResponse Prelude.Text
generateDataKeyResponse_keyId = Lens.lens (\GenerateDataKeyResponse' {keyId} -> keyId) (\s@GenerateDataKeyResponse' {} a -> s {keyId = a} :: GenerateDataKeyResponse)

-- | The plaintext data key. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded. Use this data key to encrypt your data outside of KMS.
-- Then, remove it from memory as soon as possible.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyResponse_plaintext :: Lens.Lens' GenerateDataKeyResponse Prelude.ByteString
generateDataKeyResponse_plaintext = Lens.lens (\GenerateDataKeyResponse' {plaintext} -> plaintext) (\s@GenerateDataKeyResponse' {} a -> s {plaintext = a} :: GenerateDataKeyResponse) Prelude.. Data._Sensitive Prelude.. Data._Base64

-- | The encrypted copy of the data key. When you use the HTTP API or the
-- Amazon Web Services CLI, the value is Base64-encoded. Otherwise, it is
-- not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyResponse_ciphertextBlob :: Lens.Lens' GenerateDataKeyResponse Prelude.ByteString
generateDataKeyResponse_ciphertextBlob = Lens.lens (\GenerateDataKeyResponse' {ciphertextBlob} -> ciphertextBlob) (\s@GenerateDataKeyResponse' {} a -> s {ciphertextBlob = a} :: GenerateDataKeyResponse) Prelude.. Data._Base64

instance Prelude.NFData GenerateDataKeyResponse where
  rnf GenerateDataKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf plaintext
      `Prelude.seq` Prelude.rnf ciphertextBlob
