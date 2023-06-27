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
-- Module      : Amazonka.KMS.Decrypt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext that was encrypted by a KMS key using any of the
-- following operations:
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
-- under a symmetric encryption KMS key or an asymmetric encryption KMS
-- key. When the KMS key is asymmetric, you must specify the KMS key and
-- the encryption algorithm that was used to encrypt the ciphertext. For
-- information about asymmetric KMS keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Asymmetric KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- The @Decrypt@ operation also decrypts ciphertext that was encrypted
-- outside of KMS by the public key in an KMS asymmetric KMS key. However,
-- it cannot decrypt symmetric ciphertext produced by other libraries, such
-- as the
-- <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ Amazon Web Services Encryption SDK>
-- or
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption>.
-- These libraries return a ciphertext format that is incompatible with
-- KMS.
--
-- If the ciphertext was encrypted under a symmetric encryption KMS key,
-- the @KeyId@ parameter is optional. KMS can get this information from
-- metadata that it adds to the symmetric ciphertext blob. This feature
-- adds durability to your implementation by ensuring that authorized users
-- can decrypt ciphertext decades after it was encrypted, even if they\'ve
-- lost track of the key ID. However, specifying the KMS key is always
-- recommended as a best practice. When you use the @KeyId@ parameter to
-- specify a KMS key, KMS only uses the KMS key you specify. If the
-- ciphertext was encrypted under a different KMS key, the @Decrypt@
-- operation fails. This practice ensures that you use the KMS key that you
-- intend.
--
-- Whenever possible, use key policies to give users permission to call the
-- @Decrypt@ operation on a particular KMS key, instead of using &IAM;
-- policies. Otherwise, you might create an &IAM; policy that gives the
-- user @Decrypt@ permission on all KMS keys. This user could decrypt
-- ciphertext that was encrypted by KMS keys in other accounts if the key
-- policy for the cross-account KMS key permits it. If you must use an IAM
-- policy for @Decrypt@ permissions, limit the user to particular KMS keys
-- or particular trusted accounts. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/iam-policies.html#iam-policies-best-practices Best practices for IAM policies>
-- in the /Key Management Service Developer Guide/.
--
-- @Decrypt@ also supports
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave.html Amazon Web Services Nitro Enclaves>,
-- which provide an isolated compute environment in Amazon EC2. To call
-- @Decrypt@ for a Nitro enclave, use the
-- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
-- or any Amazon Web Services SDK. Use the @Recipient@ parameter to provide
-- the attestation document for the enclave. Instead of the plaintext data,
-- the response includes the plaintext data encrypted with the public key
-- from the attestation document (@CiphertextForRecipient@).For information
-- about the interaction between KMS and Amazon Web Services Nitro
-- Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/..
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. If you use the @KeyId@ parameter to identify
-- a KMS key in a different Amazon Web Services account, specify the key
-- ARN or the alias ARN of the KMS key.
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
module Amazonka.KMS.Decrypt
  ( -- * Creating a Request
    Decrypt (..),
    newDecrypt,

    -- * Request Lenses
    decrypt_encryptionAlgorithm,
    decrypt_encryptionContext,
    decrypt_grantTokens,
    decrypt_keyId,
    decrypt_recipient,
    decrypt_ciphertextBlob,

    -- * Destructuring the Response
    DecryptResponse (..),
    newDecryptResponse,

    -- * Response Lenses
    decryptResponse_ciphertextForRecipient,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_keyId,
    decryptResponse_plaintext,
    decryptResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDecrypt' smart constructor.
data Decrypt = Decrypt'
  { -- | Specifies the encryption algorithm that will be used to decrypt the
    -- ciphertext. Specify the same algorithm that was used to encrypt the
    -- data. If you specify a different algorithm, the @Decrypt@ operation
    -- fails.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric KMS key. The default value, @SYMMETRIC_DEFAULT@,
    -- represents the only supported algorithm that is valid for symmetric
    -- encryption KMS keys.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | Specifies the encryption context to use when decrypting the data. An
    -- encryption context is valid only for
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- with a symmetric encryption KMS key. The standard asymmetric encryption
    -- algorithms and HMAC algorithms that KMS uses do not support an
    -- encryption context.
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
    -- | Specifies the KMS key that KMS uses to decrypt the ciphertext.
    --
    -- Enter a key ID of the KMS key that was used to encrypt the ciphertext.
    -- If you identify a different KMS key, the @Decrypt@ operation throws an
    -- @IncorrectKeyException@.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric KMS key. If you used a symmetric encryption KMS key, KMS
    -- can get the KMS key from metadata that it adds to the symmetric
    -- ciphertext blob. However, it is always recommended as a best practice.
    -- This practice ensures that you use the KMS key that you intend.
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
    keyId :: Prelude.Maybe Prelude.Text,
    -- | A signed
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave-how.html#term-attestdoc attestation document>
    -- from an Amazon Web Services Nitro enclave and the encryption algorithm
    -- to use with the enclave\'s public key. The only valid encryption
    -- algorithm is @RSAES_OAEP_SHA_256@.
    --
    -- This parameter only supports attestation documents for Amazon Web
    -- Services Nitro Enclaves. To include this parameter, use the
    -- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
    -- or any Amazon Web Services SDK.
    --
    -- When you use this parameter, instead of returning the plaintext data,
    -- KMS encrypts the plaintext data with the public key in the attestation
    -- document, and returns the resulting ciphertext in the
    -- @CiphertextForRecipient@ field in the response. This ciphertext can be
    -- decrypted only with the private key in the enclave. The @Plaintext@
    -- field in the response is null or empty.
    --
    -- For information about the interaction between KMS and Amazon Web
    -- Services Nitro Enclaves, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
    -- in the /Key Management Service Developer Guide/.
    recipient :: Prelude.Maybe RecipientInfo,
    -- | Ciphertext to be decrypted. The blob includes metadata.
    ciphertextBlob :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Decrypt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionAlgorithm', 'decrypt_encryptionAlgorithm' - Specifies the encryption algorithm that will be used to decrypt the
-- ciphertext. Specify the same algorithm that was used to encrypt the
-- data. If you specify a different algorithm, the @Decrypt@ operation
-- fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. The default value, @SYMMETRIC_DEFAULT@,
-- represents the only supported algorithm that is valid for symmetric
-- encryption KMS keys.
--
-- 'encryptionContext', 'decrypt_encryptionContext' - Specifies the encryption context to use when decrypting the data. An
-- encryption context is valid only for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- with a symmetric encryption KMS key. The standard asymmetric encryption
-- algorithms and HMAC algorithms that KMS uses do not support an
-- encryption context.
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
-- 'grantTokens', 'decrypt_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'keyId', 'decrypt_keyId' - Specifies the KMS key that KMS uses to decrypt the ciphertext.
--
-- Enter a key ID of the KMS key that was used to encrypt the ciphertext.
-- If you identify a different KMS key, the @Decrypt@ operation throws an
-- @IncorrectKeyException@.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. If you used a symmetric encryption KMS key, KMS
-- can get the KMS key from metadata that it adds to the symmetric
-- ciphertext blob. However, it is always recommended as a best practice.
-- This practice ensures that you use the KMS key that you intend.
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
--
-- 'recipient', 'decrypt_recipient' - A signed
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave-how.html#term-attestdoc attestation document>
-- from an Amazon Web Services Nitro enclave and the encryption algorithm
-- to use with the enclave\'s public key. The only valid encryption
-- algorithm is @RSAES_OAEP_SHA_256@.
--
-- This parameter only supports attestation documents for Amazon Web
-- Services Nitro Enclaves. To include this parameter, use the
-- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
-- or any Amazon Web Services SDK.
--
-- When you use this parameter, instead of returning the plaintext data,
-- KMS encrypts the plaintext data with the public key in the attestation
-- document, and returns the resulting ciphertext in the
-- @CiphertextForRecipient@ field in the response. This ciphertext can be
-- decrypted only with the private key in the enclave. The @Plaintext@
-- field in the response is null or empty.
--
-- For information about the interaction between KMS and Amazon Web
-- Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.
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
    { encryptionAlgorithm = Prelude.Nothing,
      encryptionContext = Prelude.Nothing,
      grantTokens = Prelude.Nothing,
      keyId = Prelude.Nothing,
      recipient = Prelude.Nothing,
      ciphertextBlob =
        Data._Base64 Lens.# pCiphertextBlob_
    }

-- | Specifies the encryption algorithm that will be used to decrypt the
-- ciphertext. Specify the same algorithm that was used to encrypt the
-- data. If you specify a different algorithm, the @Decrypt@ operation
-- fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. The default value, @SYMMETRIC_DEFAULT@,
-- represents the only supported algorithm that is valid for symmetric
-- encryption KMS keys.
decrypt_encryptionAlgorithm :: Lens.Lens' Decrypt (Prelude.Maybe EncryptionAlgorithmSpec)
decrypt_encryptionAlgorithm = Lens.lens (\Decrypt' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@Decrypt' {} a -> s {encryptionAlgorithm = a} :: Decrypt)

-- | Specifies the encryption context to use when decrypting the data. An
-- encryption context is valid only for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- with a symmetric encryption KMS key. The standard asymmetric encryption
-- algorithms and HMAC algorithms that KMS uses do not support an
-- encryption context.
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
decrypt_encryptionContext :: Lens.Lens' Decrypt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
decrypt_encryptionContext = Lens.lens (\Decrypt' {encryptionContext} -> encryptionContext) (\s@Decrypt' {} a -> s {encryptionContext = a} :: Decrypt) Prelude.. Lens.mapping Lens.coerced

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
decrypt_grantTokens :: Lens.Lens' Decrypt (Prelude.Maybe [Prelude.Text])
decrypt_grantTokens = Lens.lens (\Decrypt' {grantTokens} -> grantTokens) (\s@Decrypt' {} a -> s {grantTokens = a} :: Decrypt) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the KMS key that KMS uses to decrypt the ciphertext.
--
-- Enter a key ID of the KMS key that was used to encrypt the ciphertext.
-- If you identify a different KMS key, the @Decrypt@ operation throws an
-- @IncorrectKeyException@.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. If you used a symmetric encryption KMS key, KMS
-- can get the KMS key from metadata that it adds to the symmetric
-- ciphertext blob. However, it is always recommended as a best practice.
-- This practice ensures that you use the KMS key that you intend.
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
decrypt_keyId :: Lens.Lens' Decrypt (Prelude.Maybe Prelude.Text)
decrypt_keyId = Lens.lens (\Decrypt' {keyId} -> keyId) (\s@Decrypt' {} a -> s {keyId = a} :: Decrypt)

-- | A signed
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitro-enclave-how.html#term-attestdoc attestation document>
-- from an Amazon Web Services Nitro enclave and the encryption algorithm
-- to use with the enclave\'s public key. The only valid encryption
-- algorithm is @RSAES_OAEP_SHA_256@.
--
-- This parameter only supports attestation documents for Amazon Web
-- Services Nitro Enclaves. To include this parameter, use the
-- <https://docs.aws.amazon.com/enclaves/latest/user/developing-applications.html#sdk Amazon Web Services Nitro Enclaves SDK>
-- or any Amazon Web Services SDK.
--
-- When you use this parameter, instead of returning the plaintext data,
-- KMS encrypts the plaintext data with the public key in the attestation
-- document, and returns the resulting ciphertext in the
-- @CiphertextForRecipient@ field in the response. This ciphertext can be
-- decrypted only with the private key in the enclave. The @Plaintext@
-- field in the response is null or empty.
--
-- For information about the interaction between KMS and Amazon Web
-- Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.
decrypt_recipient :: Lens.Lens' Decrypt (Prelude.Maybe RecipientInfo)
decrypt_recipient = Lens.lens (\Decrypt' {recipient} -> recipient) (\s@Decrypt' {} a -> s {recipient = a} :: Decrypt)

-- | Ciphertext to be decrypted. The blob includes metadata.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decrypt_ciphertextBlob :: Lens.Lens' Decrypt Prelude.ByteString
decrypt_ciphertextBlob = Lens.lens (\Decrypt' {ciphertextBlob} -> ciphertextBlob) (\s@Decrypt' {} a -> s {ciphertextBlob = a} :: Decrypt) Prelude.. Data._Base64

instance Core.AWSRequest Decrypt where
  type AWSResponse Decrypt = DecryptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DecryptResponse'
            Prelude.<$> (x Data..?> "CiphertextForRecipient")
            Prelude.<*> (x Data..?> "EncryptionAlgorithm")
            Prelude.<*> (x Data..?> "KeyId")
            Prelude.<*> (x Data..?> "Plaintext")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Decrypt where
  hashWithSalt _salt Decrypt' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionAlgorithm
      `Prelude.hashWithSalt` encryptionContext
      `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` recipient
      `Prelude.hashWithSalt` ciphertextBlob

instance Prelude.NFData Decrypt where
  rnf Decrypt' {..} =
    Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf encryptionContext
      `Prelude.seq` Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf recipient
      `Prelude.seq` Prelude.rnf ciphertextBlob

instance Data.ToHeaders Decrypt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.Decrypt" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Decrypt where
  toJSON Decrypt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionAlgorithm" Data..=)
              Prelude.<$> encryptionAlgorithm,
            ("EncryptionContext" Data..=)
              Prelude.<$> encryptionContext,
            ("GrantTokens" Data..=) Prelude.<$> grantTokens,
            ("KeyId" Data..=) Prelude.<$> keyId,
            ("Recipient" Data..=) Prelude.<$> recipient,
            Prelude.Just
              ("CiphertextBlob" Data..= ciphertextBlob)
          ]
      )

instance Data.ToPath Decrypt where
  toPath = Prelude.const "/"

instance Data.ToQuery Decrypt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDecryptResponse' smart constructor.
data DecryptResponse = DecryptResponse'
  { -- | The plaintext data encrypted with the public key in the attestation
    -- document.
    --
    -- This field is included in the response only when the @Recipient@
    -- parameter in the request includes a valid attestation document from an
    -- Amazon Web Services Nitro enclave. For information about the interaction
    -- between KMS and Amazon Web Services Nitro Enclaves, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
    -- in the /Key Management Service Developer Guide/.
    ciphertextForRecipient :: Prelude.Maybe Data.Base64,
    -- | The encryption algorithm that was used to decrypt the ciphertext.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key that was used to decrypt the ciphertext.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Decrypted plaintext data. When you use the HTTP API or the Amazon Web
    -- Services CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    --
    -- If the response includes the @CiphertextForRecipient@ field, the
    -- @Plaintext@ field is null or empty.
    plaintext :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecryptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ciphertextForRecipient', 'decryptResponse_ciphertextForRecipient' - The plaintext data encrypted with the public key in the attestation
-- document.
--
-- This field is included in the response only when the @Recipient@
-- parameter in the request includes a valid attestation document from an
-- Amazon Web Services Nitro enclave. For information about the interaction
-- between KMS and Amazon Web Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'encryptionAlgorithm', 'decryptResponse_encryptionAlgorithm' - The encryption algorithm that was used to decrypt the ciphertext.
--
-- 'keyId', 'decryptResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that was used to decrypt the ciphertext.
--
-- 'plaintext', 'decryptResponse_plaintext' - Decrypted plaintext data. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.
--
-- If the response includes the @CiphertextForRecipient@ field, the
-- @Plaintext@ field is null or empty.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'decryptResponse_httpStatus' - The response's http status code.
newDecryptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DecryptResponse
newDecryptResponse pHttpStatus_ =
  DecryptResponse'
    { ciphertextForRecipient =
        Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      keyId = Prelude.Nothing,
      plaintext = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The plaintext data encrypted with the public key in the attestation
-- document.
--
-- This field is included in the response only when the @Recipient@
-- parameter in the request includes a valid attestation document from an
-- Amazon Web Services Nitro enclave. For information about the interaction
-- between KMS and Amazon Web Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decryptResponse_ciphertextForRecipient :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.ByteString)
decryptResponse_ciphertextForRecipient = Lens.lens (\DecryptResponse' {ciphertextForRecipient} -> ciphertextForRecipient) (\s@DecryptResponse' {} a -> s {ciphertextForRecipient = a} :: DecryptResponse) Prelude.. Lens.mapping Data._Base64

-- | The encryption algorithm that was used to decrypt the ciphertext.
decryptResponse_encryptionAlgorithm :: Lens.Lens' DecryptResponse (Prelude.Maybe EncryptionAlgorithmSpec)
decryptResponse_encryptionAlgorithm = Lens.lens (\DecryptResponse' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@DecryptResponse' {} a -> s {encryptionAlgorithm = a} :: DecryptResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that was used to decrypt the ciphertext.
decryptResponse_keyId :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.Text)
decryptResponse_keyId = Lens.lens (\DecryptResponse' {keyId} -> keyId) (\s@DecryptResponse' {} a -> s {keyId = a} :: DecryptResponse)

-- | Decrypted plaintext data. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.
--
-- If the response includes the @CiphertextForRecipient@ field, the
-- @Plaintext@ field is null or empty.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decryptResponse_plaintext :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.ByteString)
decryptResponse_plaintext = Lens.lens (\DecryptResponse' {plaintext} -> plaintext) (\s@DecryptResponse' {} a -> s {plaintext = a} :: DecryptResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The response's http status code.
decryptResponse_httpStatus :: Lens.Lens' DecryptResponse Prelude.Int
decryptResponse_httpStatus = Lens.lens (\DecryptResponse' {httpStatus} -> httpStatus) (\s@DecryptResponse' {} a -> s {httpStatus = a} :: DecryptResponse)

instance Prelude.NFData DecryptResponse where
  rnf DecryptResponse' {..} =
    Prelude.rnf ciphertextForRecipient
      `Prelude.seq` Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf plaintext
      `Prelude.seq` Prelude.rnf httpStatus
