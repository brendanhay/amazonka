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
-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key. This operation returns a data key
-- that is encrypted under a customer master key (CMK) that you specify. To
-- request an asymmetric data key pair, use the GenerateDataKeyPair or
-- GenerateDataKeyPairWithoutPlaintext operations.
--
-- @GenerateDataKeyWithoutPlaintext@ is identical to the GenerateDataKey
-- operation except that returns only the encrypted copy of the data key.
-- This operation is useful for systems that need to encrypt data at some
-- point, but not immediately. When you need to encrypt the data, you call
-- the Decrypt operation on the encrypted copy of the key.
--
-- It\'s also useful in distributed systems with different levels of trust.
-- For example, you might store encrypted data in containers. One component
-- of your system creates new containers and stores an encrypted data key
-- with each container. Then, a different component puts the data into the
-- containers. That component first decrypts the data key, uses the
-- plaintext data key to encrypt data, puts the encrypted data into the
-- container, and then destroys the plaintext data key. In this system, the
-- component that creates the containers never sees the plaintext data key.
--
-- @GenerateDataKeyWithoutPlaintext@ returns a unique data key for each
-- request. The bytes in the keys are not related to the caller or CMK that
-- is used to encrypt the private key.
--
-- To generate a data key, you must specify the symmetric customer master
-- key (CMK) that is used to encrypt the data key. You cannot use an
-- asymmetric CMK to generate a data key. To get the type of your CMK, use
-- the DescribeKey operation.
--
-- If the operation succeeds, you will find the encrypted copy of the data
-- key in the @CiphertextBlob@ field.
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
-- __Cross-account use__: Yes. To perform this operation with a CMK in a
-- different AWS account, specify the key ARN or alias ARN in the value of
-- the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GenerateDataKeyWithoutPlaintext>
-- (key policy)
--
-- __Related operations:__
--
-- -   Decrypt
--
-- -   Encrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
--
-- -   GenerateDataKeyPairWithoutPlaintext
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
  ( -- * Creating a Request
    GenerateDataKeyWithoutPlaintext (..),
    newGenerateDataKeyWithoutPlaintext,

    -- * Request Lenses
    generateDataKeyWithoutPlaintext_grantTokens,
    generateDataKeyWithoutPlaintext_numberOfBytes,
    generateDataKeyWithoutPlaintext_encryptionContext,
    generateDataKeyWithoutPlaintext_keySpec,
    generateDataKeyWithoutPlaintext_keyId,

    -- * Destructuring the Response
    GenerateDataKeyWithoutPlaintextResponse (..),
    newGenerateDataKeyWithoutPlaintextResponse,

    -- * Response Lenses
    generateDataKeyWithoutPlaintextResponse_ciphertextBlob,
    generateDataKeyWithoutPlaintextResponse_keyId,
    generateDataKeyWithoutPlaintextResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateDataKeyWithoutPlaintext' smart constructor.
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | The length of the data key in bytes. For example, use the value 64 to
    -- generate a 512-bit data key (64 bytes is 512 bits). For common key
    -- lengths (128-bit and 256-bit symmetric keys), we recommend that you use
    -- the @KeySpec@ field instead of this one.
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
    -- | The length of the data key. Use @AES_128@ to generate a 128-bit
    -- symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
    keySpec :: Prelude.Maybe DataKeySpec,
    -- | The identifier of the symmetric customer master key (CMK) that encrypts
    -- the data key.
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
-- Create a value of 'GenerateDataKeyWithoutPlaintext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'generateDataKeyWithoutPlaintext_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'numberOfBytes', 'generateDataKeyWithoutPlaintext_numberOfBytes' - The length of the data key in bytes. For example, use the value 64 to
-- generate a 512-bit data key (64 bytes is 512 bits). For common key
-- lengths (128-bit and 256-bit symmetric keys), we recommend that you use
-- the @KeySpec@ field instead of this one.
--
-- 'encryptionContext', 'generateDataKeyWithoutPlaintext_encryptionContext' - Specifies the encryption context that will be used when encrypting the
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
-- 'keySpec', 'generateDataKeyWithoutPlaintext_keySpec' - The length of the data key. Use @AES_128@ to generate a 128-bit
-- symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- 'keyId', 'generateDataKeyWithoutPlaintext_keyId' - The identifier of the symmetric customer master key (CMK) that encrypts
-- the data key.
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
newGenerateDataKeyWithoutPlaintext ::
  -- | 'keyId'
  Prelude.Text ->
  GenerateDataKeyWithoutPlaintext
newGenerateDataKeyWithoutPlaintext pKeyId_ =
  GenerateDataKeyWithoutPlaintext'
    { grantTokens =
        Prelude.Nothing,
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
generateDataKeyWithoutPlaintext_grantTokens :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Prelude.Maybe [Prelude.Text])
generateDataKeyWithoutPlaintext_grantTokens = Lens.lens (\GenerateDataKeyWithoutPlaintext' {grantTokens} -> grantTokens) (\s@GenerateDataKeyWithoutPlaintext' {} a -> s {grantTokens = a} :: GenerateDataKeyWithoutPlaintext) Prelude.. Lens.mapping Lens._Coerce

-- | The length of the data key in bytes. For example, use the value 64 to
-- generate a 512-bit data key (64 bytes is 512 bits). For common key
-- lengths (128-bit and 256-bit symmetric keys), we recommend that you use
-- the @KeySpec@ field instead of this one.
generateDataKeyWithoutPlaintext_numberOfBytes :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Prelude.Maybe Prelude.Natural)
generateDataKeyWithoutPlaintext_numberOfBytes = Lens.lens (\GenerateDataKeyWithoutPlaintext' {numberOfBytes} -> numberOfBytes) (\s@GenerateDataKeyWithoutPlaintext' {} a -> s {numberOfBytes = a} :: GenerateDataKeyWithoutPlaintext)

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
generateDataKeyWithoutPlaintext_encryptionContext :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
generateDataKeyWithoutPlaintext_encryptionContext = Lens.lens (\GenerateDataKeyWithoutPlaintext' {encryptionContext} -> encryptionContext) (\s@GenerateDataKeyWithoutPlaintext' {} a -> s {encryptionContext = a} :: GenerateDataKeyWithoutPlaintext) Prelude.. Lens.mapping Lens._Coerce

-- | The length of the data key. Use @AES_128@ to generate a 128-bit
-- symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
generateDataKeyWithoutPlaintext_keySpec :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Prelude.Maybe DataKeySpec)
generateDataKeyWithoutPlaintext_keySpec = Lens.lens (\GenerateDataKeyWithoutPlaintext' {keySpec} -> keySpec) (\s@GenerateDataKeyWithoutPlaintext' {} a -> s {keySpec = a} :: GenerateDataKeyWithoutPlaintext)

-- | The identifier of the symmetric customer master key (CMK) that encrypts
-- the data key.
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
generateDataKeyWithoutPlaintext_keyId :: Lens.Lens' GenerateDataKeyWithoutPlaintext Prelude.Text
generateDataKeyWithoutPlaintext_keyId = Lens.lens (\GenerateDataKeyWithoutPlaintext' {keyId} -> keyId) (\s@GenerateDataKeyWithoutPlaintext' {} a -> s {keyId = a} :: GenerateDataKeyWithoutPlaintext)

instance
  Core.AWSRequest
    GenerateDataKeyWithoutPlaintext
  where
  type
    AWSResponse GenerateDataKeyWithoutPlaintext =
      GenerateDataKeyWithoutPlaintextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataKeyWithoutPlaintextResponse'
            Prelude.<$> (x Core..?> "CiphertextBlob")
            Prelude.<*> (x Core..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GenerateDataKeyWithoutPlaintext

instance
  Prelude.NFData
    GenerateDataKeyWithoutPlaintext

instance
  Core.ToHeaders
    GenerateDataKeyWithoutPlaintext
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.GenerateDataKeyWithoutPlaintext" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GenerateDataKeyWithoutPlaintext where
  toJSON GenerateDataKeyWithoutPlaintext' {..} =
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

instance Core.ToPath GenerateDataKeyWithoutPlaintext where
  toPath = Prelude.const "/"

instance Core.ToQuery GenerateDataKeyWithoutPlaintext where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateDataKeyWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'
  { -- | The encrypted data key. When you use the HTTP API or the AWS CLI, the
    -- value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Prelude.Maybe Core.Base64,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK that encrypted the data key.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateDataKeyWithoutPlaintextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ciphertextBlob', 'generateDataKeyWithoutPlaintextResponse_ciphertextBlob' - The encrypted data key. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'generateDataKeyWithoutPlaintextResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that encrypted the data key.
--
-- 'httpStatus', 'generateDataKeyWithoutPlaintextResponse_httpStatus' - The response's http status code.
newGenerateDataKeyWithoutPlaintextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateDataKeyWithoutPlaintextResponse
newGenerateDataKeyWithoutPlaintextResponse
  pHttpStatus_ =
    GenerateDataKeyWithoutPlaintextResponse'
      { ciphertextBlob =
          Prelude.Nothing,
        keyId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The encrypted data key. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyWithoutPlaintextResponse_ciphertextBlob :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse (Prelude.Maybe Prelude.ByteString)
generateDataKeyWithoutPlaintextResponse_ciphertextBlob = Lens.lens (\GenerateDataKeyWithoutPlaintextResponse' {ciphertextBlob} -> ciphertextBlob) (\s@GenerateDataKeyWithoutPlaintextResponse' {} a -> s {ciphertextBlob = a} :: GenerateDataKeyWithoutPlaintextResponse) Prelude.. Lens.mapping Core._Base64

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that encrypted the data key.
generateDataKeyWithoutPlaintextResponse_keyId :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse (Prelude.Maybe Prelude.Text)
generateDataKeyWithoutPlaintextResponse_keyId = Lens.lens (\GenerateDataKeyWithoutPlaintextResponse' {keyId} -> keyId) (\s@GenerateDataKeyWithoutPlaintextResponse' {} a -> s {keyId = a} :: GenerateDataKeyWithoutPlaintextResponse)

-- | The response's http status code.
generateDataKeyWithoutPlaintextResponse_httpStatus :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse Prelude.Int
generateDataKeyWithoutPlaintextResponse_httpStatus = Lens.lens (\GenerateDataKeyWithoutPlaintextResponse' {httpStatus} -> httpStatus) (\s@GenerateDataKeyWithoutPlaintextResponse' {} a -> s {httpStatus = a} :: GenerateDataKeyWithoutPlaintextResponse)

instance
  Prelude.NFData
    GenerateDataKeyWithoutPlaintextResponse
