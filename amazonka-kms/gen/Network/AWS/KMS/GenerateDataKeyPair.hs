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
-- Module      : Network.AWS.KMS.GenerateDataKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique asymmetric data key pair. The @GenerateDataKeyPair@
-- operation returns a plaintext public key, a plaintext private key, and a
-- copy of the private key that is encrypted under the symmetric CMK you
-- specify. You can use the data key pair to perform asymmetric
-- cryptography outside of AWS KMS.
--
-- @GenerateDataKeyPair@ returns a unique data key pair for each request.
-- The bytes in the keys are not related to the caller or the CMK that is
-- used to encrypt the private key.
--
-- You can use the public key that @GenerateDataKeyPair@ returns to encrypt
-- data or verify a signature outside of AWS KMS. Then, store the encrypted
-- private key with the data. When you are ready to decrypt data or sign a
-- message, you can use the Decrypt operation to decrypt the encrypted
-- private key.
--
-- To generate a data key pair, you must specify a symmetric customer
-- master key (CMK) to encrypt the private key in a data key pair. You
-- cannot use an asymmetric CMK or a CMK in a custom key store. To get the
-- type and origin of your CMK, use the DescribeKey operation.
--
-- If you are using the data key pair to encrypt data, or for any operation
-- where you don\'t immediately need a private key, consider using the
-- GenerateDataKeyPairWithoutPlaintext operation.
-- @GenerateDataKeyPairWithoutPlaintext@ returns a plaintext public key and
-- an encrypted private key, but omits the plaintext private key that you
-- need only to decrypt ciphertext or sign a message. Later, when you need
-- to decrypt the data or sign a message, use the Decrypt operation to
-- decrypt the encrypted private key in the data key pair.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GenerateDataKeyPair>
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
-- -   GenerateDataKeyPairWithoutPlaintext
--
-- -   GenerateDataKeyWithoutPlaintext
module Network.AWS.KMS.GenerateDataKeyPair
  ( -- * Creating a Request
    GenerateDataKeyPair (..),
    newGenerateDataKeyPair,

    -- * Request Lenses
    generateDataKeyPair_grantTokens,
    generateDataKeyPair_encryptionContext,
    generateDataKeyPair_keyId,
    generateDataKeyPair_keyPairSpec,

    -- * Destructuring the Response
    GenerateDataKeyPairResponse (..),
    newGenerateDataKeyPairResponse,

    -- * Response Lenses
    generateDataKeyPairResponse_publicKey,
    generateDataKeyPairResponse_keyPairSpec,
    generateDataKeyPairResponse_privateKeyCiphertextBlob,
    generateDataKeyPairResponse_privateKeyPlaintext,
    generateDataKeyPairResponse_keyId,
    generateDataKeyPairResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateDataKeyPair' smart constructor.
data GenerateDataKeyPair = GenerateDataKeyPair'
  { -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the encryption context that will be used when encrypting the
    -- private key in the data key pair.
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
    -- | Specifies the symmetric CMK that encrypts the private key in the data
    -- key pair. You cannot specify an asymmetric CMK or a CMK in a custom key
    -- store. To get the type and origin of your CMK, use the DescribeKey
    -- operation.
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
    keyId :: Prelude.Text,
    -- | Determines the type of data key pair that is generated.
    --
    -- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to
    -- encrypt and decrypt or to sign and verify (but not both), and the rule
    -- that permits you to use ECC CMKs only to sign and verify, are not
    -- effective outside of AWS KMS.
    keyPairSpec :: DataKeyPairSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GenerateDataKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'generateDataKeyPair_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'encryptionContext', 'generateDataKeyPair_encryptionContext' - Specifies the encryption context that will be used when encrypting the
-- private key in the data key pair.
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
-- 'keyId', 'generateDataKeyPair_keyId' - Specifies the symmetric CMK that encrypts the private key in the data
-- key pair. You cannot specify an asymmetric CMK or a CMK in a custom key
-- store. To get the type and origin of your CMK, use the DescribeKey
-- operation.
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
-- 'keyPairSpec', 'generateDataKeyPair_keyPairSpec' - Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to
-- encrypt and decrypt or to sign and verify (but not both), and the rule
-- that permits you to use ECC CMKs only to sign and verify, are not
-- effective outside of AWS KMS.
newGenerateDataKeyPair ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'keyPairSpec'
  DataKeyPairSpec ->
  GenerateDataKeyPair
newGenerateDataKeyPair pKeyId_ pKeyPairSpec_ =
  GenerateDataKeyPair'
    { grantTokens = Prelude.Nothing,
      encryptionContext = Prelude.Nothing,
      keyId = pKeyId_,
      keyPairSpec = pKeyPairSpec_
    }

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
generateDataKeyPair_grantTokens :: Lens.Lens' GenerateDataKeyPair (Prelude.Maybe [Prelude.Text])
generateDataKeyPair_grantTokens = Lens.lens (\GenerateDataKeyPair' {grantTokens} -> grantTokens) (\s@GenerateDataKeyPair' {} a -> s {grantTokens = a} :: GenerateDataKeyPair) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the encryption context that will be used when encrypting the
-- private key in the data key pair.
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
generateDataKeyPair_encryptionContext :: Lens.Lens' GenerateDataKeyPair (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
generateDataKeyPair_encryptionContext = Lens.lens (\GenerateDataKeyPair' {encryptionContext} -> encryptionContext) (\s@GenerateDataKeyPair' {} a -> s {encryptionContext = a} :: GenerateDataKeyPair) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the symmetric CMK that encrypts the private key in the data
-- key pair. You cannot specify an asymmetric CMK or a CMK in a custom key
-- store. To get the type and origin of your CMK, use the DescribeKey
-- operation.
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
generateDataKeyPair_keyId :: Lens.Lens' GenerateDataKeyPair Prelude.Text
generateDataKeyPair_keyId = Lens.lens (\GenerateDataKeyPair' {keyId} -> keyId) (\s@GenerateDataKeyPair' {} a -> s {keyId = a} :: GenerateDataKeyPair)

-- | Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to
-- encrypt and decrypt or to sign and verify (but not both), and the rule
-- that permits you to use ECC CMKs only to sign and verify, are not
-- effective outside of AWS KMS.
generateDataKeyPair_keyPairSpec :: Lens.Lens' GenerateDataKeyPair DataKeyPairSpec
generateDataKeyPair_keyPairSpec = Lens.lens (\GenerateDataKeyPair' {keyPairSpec} -> keyPairSpec) (\s@GenerateDataKeyPair' {} a -> s {keyPairSpec = a} :: GenerateDataKeyPair)

instance Prelude.AWSRequest GenerateDataKeyPair where
  type
    Rs GenerateDataKeyPair =
      GenerateDataKeyPairResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataKeyPairResponse'
            Prelude.<$> (x Prelude..?> "PublicKey")
            Prelude.<*> (x Prelude..?> "KeyPairSpec")
            Prelude.<*> (x Prelude..?> "PrivateKeyCiphertextBlob")
            Prelude.<*> (x Prelude..?> "PrivateKeyPlaintext")
            Prelude.<*> (x Prelude..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateDataKeyPair

instance Prelude.NFData GenerateDataKeyPair

instance Prelude.ToHeaders GenerateDataKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.GenerateDataKeyPair" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GenerateDataKeyPair where
  toJSON GenerateDataKeyPair' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Prelude..=) Prelude.<$> grantTokens,
            ("EncryptionContext" Prelude..=)
              Prelude.<$> encryptionContext,
            Prelude.Just ("KeyId" Prelude..= keyId),
            Prelude.Just ("KeyPairSpec" Prelude..= keyPairSpec)
          ]
      )

instance Prelude.ToPath GenerateDataKeyPair where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GenerateDataKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateDataKeyPairResponse' smart constructor.
data GenerateDataKeyPairResponse = GenerateDataKeyPairResponse'
  { -- | The public key (in plaintext).
    publicKey :: Prelude.Maybe Prelude.Base64,
    -- | The type of data key pair that was generated.
    keyPairSpec :: Prelude.Maybe DataKeyPairSpec,
    -- | The encrypted copy of the private key. When you use the HTTP API or the
    -- AWS CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    privateKeyCiphertextBlob :: Prelude.Maybe Prelude.Base64,
    -- | The plaintext copy of the private key. When you use the HTTP API or the
    -- AWS CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    privateKeyPlaintext :: Prelude.Maybe (Prelude.Sensitive Prelude.Base64),
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK that encrypted the private key.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GenerateDataKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKey', 'generateDataKeyPairResponse_publicKey' - The public key (in plaintext).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyPairSpec', 'generateDataKeyPairResponse_keyPairSpec' - The type of data key pair that was generated.
--
-- 'privateKeyCiphertextBlob', 'generateDataKeyPairResponse_privateKeyCiphertextBlob' - The encrypted copy of the private key. When you use the HTTP API or the
-- AWS CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'privateKeyPlaintext', 'generateDataKeyPairResponse_privateKeyPlaintext' - The plaintext copy of the private key. When you use the HTTP API or the
-- AWS CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'generateDataKeyPairResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that encrypted the private key.
--
-- 'httpStatus', 'generateDataKeyPairResponse_httpStatus' - The response's http status code.
newGenerateDataKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateDataKeyPairResponse
newGenerateDataKeyPairResponse pHttpStatus_ =
  GenerateDataKeyPairResponse'
    { publicKey =
        Prelude.Nothing,
      keyPairSpec = Prelude.Nothing,
      privateKeyCiphertextBlob = Prelude.Nothing,
      privateKeyPlaintext = Prelude.Nothing,
      keyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The public key (in plaintext).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyPairResponse_publicKey :: Lens.Lens' GenerateDataKeyPairResponse (Prelude.Maybe Prelude.ByteString)
generateDataKeyPairResponse_publicKey = Lens.lens (\GenerateDataKeyPairResponse' {publicKey} -> publicKey) (\s@GenerateDataKeyPairResponse' {} a -> s {publicKey = a} :: GenerateDataKeyPairResponse) Prelude.. Lens.mapping Prelude._Base64

-- | The type of data key pair that was generated.
generateDataKeyPairResponse_keyPairSpec :: Lens.Lens' GenerateDataKeyPairResponse (Prelude.Maybe DataKeyPairSpec)
generateDataKeyPairResponse_keyPairSpec = Lens.lens (\GenerateDataKeyPairResponse' {keyPairSpec} -> keyPairSpec) (\s@GenerateDataKeyPairResponse' {} a -> s {keyPairSpec = a} :: GenerateDataKeyPairResponse)

-- | The encrypted copy of the private key. When you use the HTTP API or the
-- AWS CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyPairResponse_privateKeyCiphertextBlob :: Lens.Lens' GenerateDataKeyPairResponse (Prelude.Maybe Prelude.ByteString)
generateDataKeyPairResponse_privateKeyCiphertextBlob = Lens.lens (\GenerateDataKeyPairResponse' {privateKeyCiphertextBlob} -> privateKeyCiphertextBlob) (\s@GenerateDataKeyPairResponse' {} a -> s {privateKeyCiphertextBlob = a} :: GenerateDataKeyPairResponse) Prelude.. Lens.mapping Prelude._Base64

-- | The plaintext copy of the private key. When you use the HTTP API or the
-- AWS CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyPairResponse_privateKeyPlaintext :: Lens.Lens' GenerateDataKeyPairResponse (Prelude.Maybe Prelude.ByteString)
generateDataKeyPairResponse_privateKeyPlaintext = Lens.lens (\GenerateDataKeyPairResponse' {privateKeyPlaintext} -> privateKeyPlaintext) (\s@GenerateDataKeyPairResponse' {} a -> s {privateKeyPlaintext = a} :: GenerateDataKeyPairResponse) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Base64)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that encrypted the private key.
generateDataKeyPairResponse_keyId :: Lens.Lens' GenerateDataKeyPairResponse (Prelude.Maybe Prelude.Text)
generateDataKeyPairResponse_keyId = Lens.lens (\GenerateDataKeyPairResponse' {keyId} -> keyId) (\s@GenerateDataKeyPairResponse' {} a -> s {keyId = a} :: GenerateDataKeyPairResponse)

-- | The response's http status code.
generateDataKeyPairResponse_httpStatus :: Lens.Lens' GenerateDataKeyPairResponse Prelude.Int
generateDataKeyPairResponse_httpStatus = Lens.lens (\GenerateDataKeyPairResponse' {httpStatus} -> httpStatus) (\s@GenerateDataKeyPairResponse' {} a -> s {httpStatus = a} :: GenerateDataKeyPairResponse)

instance Prelude.NFData GenerateDataKeyPairResponse
