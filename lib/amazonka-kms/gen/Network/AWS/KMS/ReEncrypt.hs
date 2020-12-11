{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ReEncrypt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext and then reencrypts it entirely within AWS KMS. You can use this operation to change the customer master key (CMK) under which data is encrypted, such as when you <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html#rotate-keys-manually manually rotate> a CMK or change the CMK that protects a ciphertext. You can also use it to reencrypt ciphertext under the same CMK, such as to change the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> of a ciphertext.
--
-- The @ReEncrypt@ operation can decrypt ciphertext that was encrypted by using an AWS KMS CMK in an AWS KMS operation, such as 'Encrypt' or 'GenerateDataKey' . It can also decrypt ciphertext that was encrypted by using the public key of an <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#asymmetric-cmks asymmetric CMK> outside of AWS KMS. However, it cannot decrypt ciphertext produced by other libraries, such as the <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK> or <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption> . These libraries return a ciphertext format that is incompatible with AWS KMS.
-- When you use the @ReEncrypt@ operation, you need to provide information for the decrypt operation and the subsequent encrypt operation.
--
--     * If your ciphertext was encrypted under an asymmetric CMK, you must identify the /source CMK/ , that is, the CMK that encrypted the ciphertext. You must also supply the encryption algorithm that was used. This information is required to decrypt the data.
--
--
--     * It is optional, but you can specify a source CMK even when the ciphertext was encrypted under a symmetric CMK. This ensures that the ciphertext is decrypted only by using a particular CMK. If the CMK that you specify cannot decrypt the ciphertext, the @ReEncrypt@ operation fails.
--
--
--     * To reencrypt the data, you must specify the /destination CMK/ , that is, the CMK that re-encrypts the data after it is decrypted. You can select a symmetric or asymmetric CMK. If the destination CMK is an asymmetric CMK, you must also provide the encryption algorithm. The algorithm that you choose must be compatible with the CMK.
-- /Important:/ When you use an asymmetric CMK to encrypt or reencrypt data, be sure to record the CMK and encryption algorithm that you choose. You will be required to provide the same CMK and encryption algorithm when you decrypt the data. If the CMK and algorithm do not match the values used to encrypt the data, the decrypt operation fails.
-- You are not required to supply the CMK ID and encryption algorithm when you decrypt with symmetric CMKs because AWS KMS stores this information in the ciphertext blob. AWS KMS cannot store metadata in ciphertext generated with asymmetric keys. The standard format for asymmetric key ciphertext does not include configurable fields.
--
--
-- Unlike other AWS KMS API operations, @ReEncrypt@ callers must have two permissions:
--
--     * @kms:ReEncryptFrom@ permission on the source CMK
--
--
--     * @kms:ReEncryptTo@ permission on the destination CMK
--
--
-- To permit reencryption from or to a CMK, include the @"kms:ReEncrypt*"@ permission in your <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html key policy> . This permission is automatically included in the key policy when you use the console to create a CMK. But you must include it manually when you create a CMK programmatically or when you use the 'PutKeyPolicy' operation to set a key policy.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.ReEncrypt
  ( -- * Creating a request
    ReEncrypt (..),
    mkReEncrypt,

    -- ** Request lenses
    reDestinationEncryptionContext,
    reSourceKeyId,
    reSourceEncryptionContext,
    reGrantTokens,
    reDestinationEncryptionAlgorithm,
    reSourceEncryptionAlgorithm,
    reCiphertextBlob,
    reDestinationKeyId,

    -- * Destructuring the response
    ReEncryptResponse (..),
    mkReEncryptResponse,

    -- ** Response lenses
    rersSourceKeyId,
    rersKeyId,
    rersDestinationEncryptionAlgorithm,
    rersSourceEncryptionAlgorithm,
    rersCiphertextBlob,
    rersResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReEncrypt' smart constructor.
data ReEncrypt = ReEncrypt'
  { destinationEncryptionContext ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    sourceKeyId :: Lude.Maybe Lude.Text,
    sourceEncryptionContext ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    grantTokens :: Lude.Maybe [Lude.Text],
    destinationEncryptionAlgorithm ::
      Lude.Maybe EncryptionAlgorithmSpec,
    sourceEncryptionAlgorithm :: Lude.Maybe EncryptionAlgorithmSpec,
    ciphertextBlob :: Lude.Base64,
    destinationKeyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReEncrypt' with the minimum fields required to make a request.
--
-- * 'ciphertextBlob' - Ciphertext of the data to reencrypt.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'destinationEncryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to reecrypt the data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@ , represents the encryption algorithm used for symmetric CMKs.
--
-- This parameter is required only when the destination CMK is an asymmetric CMK.
-- * 'destinationEncryptionContext' - Specifies that encryption context to use when the reencrypting the data.
--
-- A destination encryption context is valid only when the destination CMK is a symmetric CMK. The standard ciphertext format for asymmetric CMKs does not include fields for metadata.
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'destinationKeyId' - A unique identifier for the CMK that is used to reencrypt the data. Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of @ENCRYPT_DECRYPT@ . To find the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'sourceEncryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to decrypt the ciphertext before it is reencrypted. The default value, @SYMMETRIC_DEFAULT@ , represents the algorithm used for symmetric CMKs.
--
-- Specify the same algorithm that was used to encrypt the ciphertext. If you specify a different algorithm, the decrypt attempt fails.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK.
-- * 'sourceEncryptionContext' - Specifies the encryption context to use to decrypt the ciphertext. Enter the same encryption context that was used to encrypt the ciphertext.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'sourceKeyId' - A unique identifier for the CMK that is used to decrypt the ciphertext before it reencrypts it using the destination CMK.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext before it is reencrypted.
-- If you specify a @KeyId@ value, the decrypt part of the @ReEncrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ .
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
mkReEncrypt ::
  -- | 'ciphertextBlob'
  Lude.Base64 ->
  -- | 'destinationKeyId'
  Lude.Text ->
  ReEncrypt
mkReEncrypt pCiphertextBlob_ pDestinationKeyId_ =
  ReEncrypt'
    { destinationEncryptionContext = Lude.Nothing,
      sourceKeyId = Lude.Nothing,
      sourceEncryptionContext = Lude.Nothing,
      grantTokens = Lude.Nothing,
      destinationEncryptionAlgorithm = Lude.Nothing,
      sourceEncryptionAlgorithm = Lude.Nothing,
      ciphertextBlob = pCiphertextBlob_,
      destinationKeyId = pDestinationKeyId_
    }

-- | Specifies that encryption context to use when the reencrypting the data.
--
-- A destination encryption context is valid only when the destination CMK is a symmetric CMK. The standard ciphertext format for asymmetric CMKs does not include fields for metadata.
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'destinationEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDestinationEncryptionContext :: Lens.Lens' ReEncrypt (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
reDestinationEncryptionContext = Lens.lens (destinationEncryptionContext :: ReEncrypt -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {destinationEncryptionContext = a} :: ReEncrypt)
{-# DEPRECATED reDestinationEncryptionContext "Use generic-lens or generic-optics with 'destinationEncryptionContext' instead." #-}

-- | A unique identifier for the CMK that is used to decrypt the ciphertext before it reencrypts it using the destination CMK.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext before it is reencrypted.
-- If you specify a @KeyId@ value, the decrypt part of the @ReEncrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ .
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- /Note:/ Consider using 'sourceKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reSourceKeyId :: Lens.Lens' ReEncrypt (Lude.Maybe Lude.Text)
reSourceKeyId = Lens.lens (sourceKeyId :: ReEncrypt -> Lude.Maybe Lude.Text) (\s a -> s {sourceKeyId = a} :: ReEncrypt)
{-# DEPRECATED reSourceKeyId "Use generic-lens or generic-optics with 'sourceKeyId' instead." #-}

-- | Specifies the encryption context to use to decrypt the ciphertext. Enter the same encryption context that was used to encrypt the ciphertext.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'sourceEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reSourceEncryptionContext :: Lens.Lens' ReEncrypt (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
reSourceEncryptionContext = Lens.lens (sourceEncryptionContext :: ReEncrypt -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {sourceEncryptionContext = a} :: ReEncrypt)
{-# DEPRECATED reSourceEncryptionContext "Use generic-lens or generic-optics with 'sourceEncryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reGrantTokens :: Lens.Lens' ReEncrypt (Lude.Maybe [Lude.Text])
reGrantTokens = Lens.lens (grantTokens :: ReEncrypt -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: ReEncrypt)
{-# DEPRECATED reGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Specifies the encryption algorithm that AWS KMS will use to reecrypt the data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@ , represents the encryption algorithm used for symmetric CMKs.
--
-- This parameter is required only when the destination CMK is an asymmetric CMK.
--
-- /Note:/ Consider using 'destinationEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDestinationEncryptionAlgorithm :: Lens.Lens' ReEncrypt (Lude.Maybe EncryptionAlgorithmSpec)
reDestinationEncryptionAlgorithm = Lens.lens (destinationEncryptionAlgorithm :: ReEncrypt -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {destinationEncryptionAlgorithm = a} :: ReEncrypt)
{-# DEPRECATED reDestinationEncryptionAlgorithm "Use generic-lens or generic-optics with 'destinationEncryptionAlgorithm' instead." #-}

-- | Specifies the encryption algorithm that AWS KMS will use to decrypt the ciphertext before it is reencrypted. The default value, @SYMMETRIC_DEFAULT@ , represents the algorithm used for symmetric CMKs.
--
-- Specify the same algorithm that was used to encrypt the ciphertext. If you specify a different algorithm, the decrypt attempt fails.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK.
--
-- /Note:/ Consider using 'sourceEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reSourceEncryptionAlgorithm :: Lens.Lens' ReEncrypt (Lude.Maybe EncryptionAlgorithmSpec)
reSourceEncryptionAlgorithm = Lens.lens (sourceEncryptionAlgorithm :: ReEncrypt -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {sourceEncryptionAlgorithm = a} :: ReEncrypt)
{-# DEPRECATED reSourceEncryptionAlgorithm "Use generic-lens or generic-optics with 'sourceEncryptionAlgorithm' instead." #-}

-- | Ciphertext of the data to reencrypt.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCiphertextBlob :: Lens.Lens' ReEncrypt Lude.Base64
reCiphertextBlob = Lens.lens (ciphertextBlob :: ReEncrypt -> Lude.Base64) (\s a -> s {ciphertextBlob = a} :: ReEncrypt)
{-# DEPRECATED reCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | A unique identifier for the CMK that is used to reencrypt the data. Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of @ENCRYPT_DECRYPT@ . To find the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- /Note:/ Consider using 'destinationKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDestinationKeyId :: Lens.Lens' ReEncrypt Lude.Text
reDestinationKeyId = Lens.lens (destinationKeyId :: ReEncrypt -> Lude.Text) (\s a -> s {destinationKeyId = a} :: ReEncrypt)
{-# DEPRECATED reDestinationKeyId "Use generic-lens or generic-optics with 'destinationKeyId' instead." #-}

instance Lude.AWSRequest ReEncrypt where
  type Rs ReEncrypt = ReEncryptResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReEncryptResponse'
            Lude.<$> (x Lude..?> "SourceKeyId")
            Lude.<*> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "DestinationEncryptionAlgorithm")
            Lude.<*> (x Lude..?> "SourceEncryptionAlgorithm")
            Lude.<*> (x Lude..?> "CiphertextBlob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReEncrypt where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ReEncrypt" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ReEncrypt where
  toJSON ReEncrypt' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DestinationEncryptionContext" Lude..=)
              Lude.<$> destinationEncryptionContext,
            ("SourceKeyId" Lude..=) Lude.<$> sourceKeyId,
            ("SourceEncryptionContext" Lude..=)
              Lude.<$> sourceEncryptionContext,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            ("DestinationEncryptionAlgorithm" Lude..=)
              Lude.<$> destinationEncryptionAlgorithm,
            ("SourceEncryptionAlgorithm" Lude..=)
              Lude.<$> sourceEncryptionAlgorithm,
            Lude.Just ("CiphertextBlob" Lude..= ciphertextBlob),
            Lude.Just ("DestinationKeyId" Lude..= destinationKeyId)
          ]
      )

instance Lude.ToPath ReEncrypt where
  toPath = Lude.const "/"

instance Lude.ToQuery ReEncrypt where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkReEncryptResponse' smart constructor.
data ReEncryptResponse = ReEncryptResponse'
  { sourceKeyId ::
      Lude.Maybe Lude.Text,
    keyId :: Lude.Maybe Lude.Text,
    destinationEncryptionAlgorithm ::
      Lude.Maybe EncryptionAlgorithmSpec,
    sourceEncryptionAlgorithm ::
      Lude.Maybe EncryptionAlgorithmSpec,
    ciphertextBlob :: Lude.Maybe Lude.Base64,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReEncryptResponse' with the minimum fields required to make a request.
--
-- * 'ciphertextBlob' - The reencrypted data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'destinationEncryptionAlgorithm' - The encryption algorithm that was used to reencrypt the data.
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to reencrypt the data.
-- * 'responseStatus' - The response status code.
-- * 'sourceEncryptionAlgorithm' - The encryption algorithm that was used to decrypt the ciphertext before it was reencrypted.
-- * 'sourceKeyId' - Unique identifier of the CMK used to originally encrypt the data.
mkReEncryptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReEncryptResponse
mkReEncryptResponse pResponseStatus_ =
  ReEncryptResponse'
    { sourceKeyId = Lude.Nothing,
      keyId = Lude.Nothing,
      destinationEncryptionAlgorithm = Lude.Nothing,
      sourceEncryptionAlgorithm = Lude.Nothing,
      ciphertextBlob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique identifier of the CMK used to originally encrypt the data.
--
-- /Note:/ Consider using 'sourceKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rersSourceKeyId :: Lens.Lens' ReEncryptResponse (Lude.Maybe Lude.Text)
rersSourceKeyId = Lens.lens (sourceKeyId :: ReEncryptResponse -> Lude.Maybe Lude.Text) (\s a -> s {sourceKeyId = a} :: ReEncryptResponse)
{-# DEPRECATED rersSourceKeyId "Use generic-lens or generic-optics with 'sourceKeyId' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to reencrypt the data.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rersKeyId :: Lens.Lens' ReEncryptResponse (Lude.Maybe Lude.Text)
rersKeyId = Lens.lens (keyId :: ReEncryptResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: ReEncryptResponse)
{-# DEPRECATED rersKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The encryption algorithm that was used to reencrypt the data.
--
-- /Note:/ Consider using 'destinationEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rersDestinationEncryptionAlgorithm :: Lens.Lens' ReEncryptResponse (Lude.Maybe EncryptionAlgorithmSpec)
rersDestinationEncryptionAlgorithm = Lens.lens (destinationEncryptionAlgorithm :: ReEncryptResponse -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {destinationEncryptionAlgorithm = a} :: ReEncryptResponse)
{-# DEPRECATED rersDestinationEncryptionAlgorithm "Use generic-lens or generic-optics with 'destinationEncryptionAlgorithm' instead." #-}

-- | The encryption algorithm that was used to decrypt the ciphertext before it was reencrypted.
--
-- /Note:/ Consider using 'sourceEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rersSourceEncryptionAlgorithm :: Lens.Lens' ReEncryptResponse (Lude.Maybe EncryptionAlgorithmSpec)
rersSourceEncryptionAlgorithm = Lens.lens (sourceEncryptionAlgorithm :: ReEncryptResponse -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {sourceEncryptionAlgorithm = a} :: ReEncryptResponse)
{-# DEPRECATED rersSourceEncryptionAlgorithm "Use generic-lens or generic-optics with 'sourceEncryptionAlgorithm' instead." #-}

-- | The reencrypted data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rersCiphertextBlob :: Lens.Lens' ReEncryptResponse (Lude.Maybe Lude.Base64)
rersCiphertextBlob = Lens.lens (ciphertextBlob :: ReEncryptResponse -> Lude.Maybe Lude.Base64) (\s a -> s {ciphertextBlob = a} :: ReEncryptResponse)
{-# DEPRECATED rersCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rersResponseStatus :: Lens.Lens' ReEncryptResponse Lude.Int
rersResponseStatus = Lens.lens (responseStatus :: ReEncryptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReEncryptResponse)
{-# DEPRECATED rersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
