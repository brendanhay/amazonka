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
-- Module      : Network.AWS.KMS.ReEncrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext and then reencrypts it entirely within AWS KMS. You
-- can use this operation to change the customer master key (CMK) under
-- which data is encrypted, such as when you
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html#rotate-keys-manually manually rotate>
-- a CMK or change the CMK that protects a ciphertext. You can also use it
-- to reencrypt ciphertext under the same CMK, such as to change the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>
-- of a ciphertext.
--
-- The @ReEncrypt@ operation can decrypt ciphertext that was encrypted by
-- using an AWS KMS CMK in an AWS KMS operation, such as Encrypt or
-- GenerateDataKey. It can also decrypt ciphertext that was encrypted by
-- using the public key of an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#asymmetric-cmks asymmetric CMK>
-- outside of AWS KMS. However, it cannot decrypt ciphertext produced by
-- other libraries, such as the
-- <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK>
-- or
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption>.
-- These libraries return a ciphertext format that is incompatible with AWS
-- KMS.
--
-- When you use the @ReEncrypt@ operation, you need to provide information
-- for the decrypt operation and the subsequent encrypt operation.
--
-- -   If your ciphertext was encrypted under an asymmetric CMK, you must
--     use the @SourceKeyId@ parameter to identify the CMK that encrypted
--     the ciphertext. You must also supply the encryption algorithm that
--     was used. This information is required to decrypt the data.
--
-- -   If your ciphertext was encrypted under a symmetric CMK, the
--     @SourceKeyId@ parameter is optional. AWS KMS can get this
--     information from metadata that it adds to the symmetric ciphertext
--     blob. This feature adds durability to your implementation by
--     ensuring that authorized users can decrypt ciphertext decades after
--     it was encrypted, even if they\'ve lost track of the CMK ID.
--     However, specifying the source CMK is always recommended as a best
--     practice. When you use the @SourceKeyId@ parameter to specify a CMK,
--     AWS KMS uses only the CMK you specify. If the ciphertext was
--     encrypted under a different CMK, the @ReEncrypt@ operation fails.
--     This practice ensures that you use the CMK that you intend.
--
-- -   To reencrypt the data, you must use the @DestinationKeyId@ parameter
--     specify the CMK that re-encrypts the data after it is decrypted. You
--     can select a symmetric or asymmetric CMK. If the destination CMK is
--     an asymmetric CMK, you must also provide the encryption algorithm.
--     The algorithm that you choose must be compatible with the CMK.
--
--     When you use an asymmetric CMK to encrypt or reencrypt data, be sure
--     to record the CMK and encryption algorithm that you choose. You will
--     be required to provide the same CMK and encryption algorithm when
--     you decrypt the data. If the CMK and algorithm do not match the
--     values used to encrypt the data, the decrypt operation fails.
--
--     You are not required to supply the CMK ID and encryption algorithm
--     when you decrypt with symmetric CMKs because AWS KMS stores this
--     information in the ciphertext blob. AWS KMS cannot store metadata in
--     ciphertext generated with asymmetric keys. The standard format for
--     asymmetric key ciphertext does not include configurable fields.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. The source CMK and destination CMK can be in
-- different AWS accounts. Either or both CMKs can be in a different
-- account than the caller.
--
-- __Required permissions__:
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ReEncryptFrom>
--     permission on the source CMK (key policy)
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ReEncryptTo>
--     permission on the destination CMK (key policy)
--
-- To permit reencryption from or to a CMK, include the
-- @\"kms:ReEncrypt*\"@ permission in your
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html key policy>.
-- This permission is automatically included in the key policy when you use
-- the console to create a CMK. But you must include it manually when you
-- create a CMK programmatically or when you use the PutKeyPolicy operation
-- to set a key policy.
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
module Network.AWS.KMS.ReEncrypt
  ( -- * Creating a Request
    ReEncrypt (..),
    newReEncrypt,

    -- * Request Lenses
    reEncrypt_destinationEncryptionContext,
    reEncrypt_grantTokens,
    reEncrypt_sourceEncryptionContext,
    reEncrypt_sourceKeyId,
    reEncrypt_destinationEncryptionAlgorithm,
    reEncrypt_sourceEncryptionAlgorithm,
    reEncrypt_ciphertextBlob,
    reEncrypt_destinationKeyId,

    -- * Destructuring the Response
    ReEncryptResponse (..),
    newReEncryptResponse,

    -- * Response Lenses
    reEncryptResponse_sourceKeyId,
    reEncryptResponse_destinationEncryptionAlgorithm,
    reEncryptResponse_ciphertextBlob,
    reEncryptResponse_sourceEncryptionAlgorithm,
    reEncryptResponse_keyId,
    reEncryptResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReEncrypt' smart constructor.
data ReEncrypt = ReEncrypt'
  { -- | Specifies that encryption context to use when the reencrypting the data.
    --
    -- A destination encryption context is valid only when the destination CMK
    -- is a symmetric CMK. The standard ciphertext format for asymmetric CMKs
    -- does not include fields for metadata.
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
    destinationEncryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of grant tokens.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
    -- in the /AWS Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the encryption context to use to decrypt the ciphertext. Enter
    -- the same encryption context that was used to encrypt the ciphertext.
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
    sourceEncryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the customer master key (CMK) that AWS KMS will use to decrypt
    -- the ciphertext before it is re-encrypted. Enter a key ID of the CMK that
    -- was used to encrypt the ciphertext.
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
    sourceKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the encryption algorithm that AWS KMS will use to reecrypt the
    -- data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@,
    -- represents the encryption algorithm used for symmetric CMKs.
    --
    -- This parameter is required only when the destination CMK is an
    -- asymmetric CMK.
    destinationEncryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | Specifies the encryption algorithm that AWS KMS will use to decrypt the
    -- ciphertext before it is reencrypted. The default value,
    -- @SYMMETRIC_DEFAULT@, represents the algorithm used for symmetric CMKs.
    --
    -- Specify the same algorithm that was used to encrypt the ciphertext. If
    -- you specify a different algorithm, the decrypt attempt fails.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric CMK.
    sourceEncryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | Ciphertext of the data to reencrypt.
    ciphertextBlob :: Core.Base64,
    -- | A unique identifier for the CMK that is used to reencrypt the data.
    -- Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of
    -- @ENCRYPT_DECRYPT@. To find the @KeyUsage@ value of a CMK, use the
    -- DescribeKey operation.
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
    destinationKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReEncrypt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationEncryptionContext', 'reEncrypt_destinationEncryptionContext' - Specifies that encryption context to use when the reencrypting the data.
--
-- A destination encryption context is valid only when the destination CMK
-- is a symmetric CMK. The standard ciphertext format for asymmetric CMKs
-- does not include fields for metadata.
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
-- 'grantTokens', 'reEncrypt_grantTokens' - A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'sourceEncryptionContext', 'reEncrypt_sourceEncryptionContext' - Specifies the encryption context to use to decrypt the ciphertext. Enter
-- the same encryption context that was used to encrypt the ciphertext.
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
-- 'sourceKeyId', 'reEncrypt_sourceKeyId' - Specifies the customer master key (CMK) that AWS KMS will use to decrypt
-- the ciphertext before it is re-encrypted. Enter a key ID of the CMK that
-- was used to encrypt the ciphertext.
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
-- 'destinationEncryptionAlgorithm', 'reEncrypt_destinationEncryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to reecrypt the
-- data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@,
-- represents the encryption algorithm used for symmetric CMKs.
--
-- This parameter is required only when the destination CMK is an
-- asymmetric CMK.
--
-- 'sourceEncryptionAlgorithm', 'reEncrypt_sourceEncryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to decrypt the
-- ciphertext before it is reencrypted. The default value,
-- @SYMMETRIC_DEFAULT@, represents the algorithm used for symmetric CMKs.
--
-- Specify the same algorithm that was used to encrypt the ciphertext. If
-- you specify a different algorithm, the decrypt attempt fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric CMK.
--
-- 'ciphertextBlob', 'reEncrypt_ciphertextBlob' - Ciphertext of the data to reencrypt.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'destinationKeyId', 'reEncrypt_destinationKeyId' - A unique identifier for the CMK that is used to reencrypt the data.
-- Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of
-- @ENCRYPT_DECRYPT@. To find the @KeyUsage@ value of a CMK, use the
-- DescribeKey operation.
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
newReEncrypt ::
  -- | 'ciphertextBlob'
  Prelude.ByteString ->
  -- | 'destinationKeyId'
  Prelude.Text ->
  ReEncrypt
newReEncrypt pCiphertextBlob_ pDestinationKeyId_ =
  ReEncrypt'
    { destinationEncryptionContext =
        Prelude.Nothing,
      grantTokens = Prelude.Nothing,
      sourceEncryptionContext = Prelude.Nothing,
      sourceKeyId = Prelude.Nothing,
      destinationEncryptionAlgorithm = Prelude.Nothing,
      sourceEncryptionAlgorithm = Prelude.Nothing,
      ciphertextBlob =
        Core._Base64 Lens.# pCiphertextBlob_,
      destinationKeyId = pDestinationKeyId_
    }

-- | Specifies that encryption context to use when the reencrypting the data.
--
-- A destination encryption context is valid only when the destination CMK
-- is a symmetric CMK. The standard ciphertext format for asymmetric CMKs
-- does not include fields for metadata.
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
reEncrypt_destinationEncryptionContext :: Lens.Lens' ReEncrypt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reEncrypt_destinationEncryptionContext = Lens.lens (\ReEncrypt' {destinationEncryptionContext} -> destinationEncryptionContext) (\s@ReEncrypt' {} a -> s {destinationEncryptionContext = a} :: ReEncrypt) Prelude.. Lens.mapping Lens._Coerce

-- | A list of grant tokens.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
reEncrypt_grantTokens :: Lens.Lens' ReEncrypt (Prelude.Maybe [Prelude.Text])
reEncrypt_grantTokens = Lens.lens (\ReEncrypt' {grantTokens} -> grantTokens) (\s@ReEncrypt' {} a -> s {grantTokens = a} :: ReEncrypt) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the encryption context to use to decrypt the ciphertext. Enter
-- the same encryption context that was used to encrypt the ciphertext.
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
reEncrypt_sourceEncryptionContext :: Lens.Lens' ReEncrypt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reEncrypt_sourceEncryptionContext = Lens.lens (\ReEncrypt' {sourceEncryptionContext} -> sourceEncryptionContext) (\s@ReEncrypt' {} a -> s {sourceEncryptionContext = a} :: ReEncrypt) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the customer master key (CMK) that AWS KMS will use to decrypt
-- the ciphertext before it is re-encrypted. Enter a key ID of the CMK that
-- was used to encrypt the ciphertext.
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
reEncrypt_sourceKeyId :: Lens.Lens' ReEncrypt (Prelude.Maybe Prelude.Text)
reEncrypt_sourceKeyId = Lens.lens (\ReEncrypt' {sourceKeyId} -> sourceKeyId) (\s@ReEncrypt' {} a -> s {sourceKeyId = a} :: ReEncrypt)

-- | Specifies the encryption algorithm that AWS KMS will use to reecrypt the
-- data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@,
-- represents the encryption algorithm used for symmetric CMKs.
--
-- This parameter is required only when the destination CMK is an
-- asymmetric CMK.
reEncrypt_destinationEncryptionAlgorithm :: Lens.Lens' ReEncrypt (Prelude.Maybe EncryptionAlgorithmSpec)
reEncrypt_destinationEncryptionAlgorithm = Lens.lens (\ReEncrypt' {destinationEncryptionAlgorithm} -> destinationEncryptionAlgorithm) (\s@ReEncrypt' {} a -> s {destinationEncryptionAlgorithm = a} :: ReEncrypt)

-- | Specifies the encryption algorithm that AWS KMS will use to decrypt the
-- ciphertext before it is reencrypted. The default value,
-- @SYMMETRIC_DEFAULT@, represents the algorithm used for symmetric CMKs.
--
-- Specify the same algorithm that was used to encrypt the ciphertext. If
-- you specify a different algorithm, the decrypt attempt fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric CMK.
reEncrypt_sourceEncryptionAlgorithm :: Lens.Lens' ReEncrypt (Prelude.Maybe EncryptionAlgorithmSpec)
reEncrypt_sourceEncryptionAlgorithm = Lens.lens (\ReEncrypt' {sourceEncryptionAlgorithm} -> sourceEncryptionAlgorithm) (\s@ReEncrypt' {} a -> s {sourceEncryptionAlgorithm = a} :: ReEncrypt)

-- | Ciphertext of the data to reencrypt.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
reEncrypt_ciphertextBlob :: Lens.Lens' ReEncrypt Prelude.ByteString
reEncrypt_ciphertextBlob = Lens.lens (\ReEncrypt' {ciphertextBlob} -> ciphertextBlob) (\s@ReEncrypt' {} a -> s {ciphertextBlob = a} :: ReEncrypt) Prelude.. Core._Base64

-- | A unique identifier for the CMK that is used to reencrypt the data.
-- Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of
-- @ENCRYPT_DECRYPT@. To find the @KeyUsage@ value of a CMK, use the
-- DescribeKey operation.
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
reEncrypt_destinationKeyId :: Lens.Lens' ReEncrypt Prelude.Text
reEncrypt_destinationKeyId = Lens.lens (\ReEncrypt' {destinationKeyId} -> destinationKeyId) (\s@ReEncrypt' {} a -> s {destinationKeyId = a} :: ReEncrypt)

instance Core.AWSRequest ReEncrypt where
  type AWSResponse ReEncrypt = ReEncryptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ReEncryptResponse'
            Prelude.<$> (x Core..?> "SourceKeyId")
            Prelude.<*> (x Core..?> "DestinationEncryptionAlgorithm")
            Prelude.<*> (x Core..?> "CiphertextBlob")
            Prelude.<*> (x Core..?> "SourceEncryptionAlgorithm")
            Prelude.<*> (x Core..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReEncrypt

instance Prelude.NFData ReEncrypt

instance Core.ToHeaders ReEncrypt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.ReEncrypt" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ReEncrypt where
  toJSON ReEncrypt' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DestinationEncryptionContext" Core..=)
              Prelude.<$> destinationEncryptionContext,
            ("GrantTokens" Core..=) Prelude.<$> grantTokens,
            ("SourceEncryptionContext" Core..=)
              Prelude.<$> sourceEncryptionContext,
            ("SourceKeyId" Core..=) Prelude.<$> sourceKeyId,
            ("DestinationEncryptionAlgorithm" Core..=)
              Prelude.<$> destinationEncryptionAlgorithm,
            ("SourceEncryptionAlgorithm" Core..=)
              Prelude.<$> sourceEncryptionAlgorithm,
            Prelude.Just
              ("CiphertextBlob" Core..= ciphertextBlob),
            Prelude.Just
              ("DestinationKeyId" Core..= destinationKeyId)
          ]
      )

instance Core.ToPath ReEncrypt where
  toPath = Prelude.const "/"

instance Core.ToQuery ReEncrypt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReEncryptResponse' smart constructor.
data ReEncryptResponse = ReEncryptResponse'
  { -- | Unique identifier of the CMK used to originally encrypt the data.
    sourceKeyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption algorithm that was used to reencrypt the data.
    destinationEncryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | The reencrypted data. When you use the HTTP API or the AWS CLI, the
    -- value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Prelude.Maybe Core.Base64,
    -- | The encryption algorithm that was used to decrypt the ciphertext before
    -- it was reencrypted.
    sourceEncryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK that was used to reencrypt the data.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReEncryptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceKeyId', 'reEncryptResponse_sourceKeyId' - Unique identifier of the CMK used to originally encrypt the data.
--
-- 'destinationEncryptionAlgorithm', 'reEncryptResponse_destinationEncryptionAlgorithm' - The encryption algorithm that was used to reencrypt the data.
--
-- 'ciphertextBlob', 'reEncryptResponse_ciphertextBlob' - The reencrypted data. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'sourceEncryptionAlgorithm', 'reEncryptResponse_sourceEncryptionAlgorithm' - The encryption algorithm that was used to decrypt the ciphertext before
-- it was reencrypted.
--
-- 'keyId', 'reEncryptResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that was used to reencrypt the data.
--
-- 'httpStatus', 'reEncryptResponse_httpStatus' - The response's http status code.
newReEncryptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReEncryptResponse
newReEncryptResponse pHttpStatus_ =
  ReEncryptResponse'
    { sourceKeyId = Prelude.Nothing,
      destinationEncryptionAlgorithm = Prelude.Nothing,
      ciphertextBlob = Prelude.Nothing,
      sourceEncryptionAlgorithm = Prelude.Nothing,
      keyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier of the CMK used to originally encrypt the data.
reEncryptResponse_sourceKeyId :: Lens.Lens' ReEncryptResponse (Prelude.Maybe Prelude.Text)
reEncryptResponse_sourceKeyId = Lens.lens (\ReEncryptResponse' {sourceKeyId} -> sourceKeyId) (\s@ReEncryptResponse' {} a -> s {sourceKeyId = a} :: ReEncryptResponse)

-- | The encryption algorithm that was used to reencrypt the data.
reEncryptResponse_destinationEncryptionAlgorithm :: Lens.Lens' ReEncryptResponse (Prelude.Maybe EncryptionAlgorithmSpec)
reEncryptResponse_destinationEncryptionAlgorithm = Lens.lens (\ReEncryptResponse' {destinationEncryptionAlgorithm} -> destinationEncryptionAlgorithm) (\s@ReEncryptResponse' {} a -> s {destinationEncryptionAlgorithm = a} :: ReEncryptResponse)

-- | The reencrypted data. When you use the HTTP API or the AWS CLI, the
-- value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
reEncryptResponse_ciphertextBlob :: Lens.Lens' ReEncryptResponse (Prelude.Maybe Prelude.ByteString)
reEncryptResponse_ciphertextBlob = Lens.lens (\ReEncryptResponse' {ciphertextBlob} -> ciphertextBlob) (\s@ReEncryptResponse' {} a -> s {ciphertextBlob = a} :: ReEncryptResponse) Prelude.. Lens.mapping Core._Base64

-- | The encryption algorithm that was used to decrypt the ciphertext before
-- it was reencrypted.
reEncryptResponse_sourceEncryptionAlgorithm :: Lens.Lens' ReEncryptResponse (Prelude.Maybe EncryptionAlgorithmSpec)
reEncryptResponse_sourceEncryptionAlgorithm = Lens.lens (\ReEncryptResponse' {sourceEncryptionAlgorithm} -> sourceEncryptionAlgorithm) (\s@ReEncryptResponse' {} a -> s {sourceEncryptionAlgorithm = a} :: ReEncryptResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK that was used to reencrypt the data.
reEncryptResponse_keyId :: Lens.Lens' ReEncryptResponse (Prelude.Maybe Prelude.Text)
reEncryptResponse_keyId = Lens.lens (\ReEncryptResponse' {keyId} -> keyId) (\s@ReEncryptResponse' {} a -> s {keyId = a} :: ReEncryptResponse)

-- | The response's http status code.
reEncryptResponse_httpStatus :: Lens.Lens' ReEncryptResponse Prelude.Int
reEncryptResponse_httpStatus = Lens.lens (\ReEncryptResponse' {httpStatus} -> httpStatus) (\s@ReEncryptResponse' {} a -> s {httpStatus = a} :: ReEncryptResponse)

instance Prelude.NFData ReEncryptResponse
