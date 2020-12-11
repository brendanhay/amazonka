{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Decrypt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext that was encrypted by a AWS KMS customer master key (CMK) using any of the following operations:
--
--
--     * 'Encrypt'
--
--
--     * 'GenerateDataKey'
--
--
--     * 'GenerateDataKeyPair'
--
--
--     * 'GenerateDataKeyWithoutPlaintext'
--
--
--     * 'GenerateDataKeyPairWithoutPlaintext'
--
--
-- You can use this operation to decrypt ciphertext that was encrypted under a symmetric or asymmetric CMK. When the CMK is asymmetric, you must specify the CMK and the encryption algorithm that was used to encrypt the ciphertext. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
-- The Decrypt operation also decrypts ciphertext that was encrypted outside of AWS KMS by the public key in an AWS KMS asymmetric CMK. However, it cannot decrypt ciphertext produced by other libraries, such as the <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK> or <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption> . These libraries return a ciphertext format that is incompatible with AWS KMS.
-- If the ciphertext was encrypted under a symmetric CMK, you do not need to specify the CMK or the encryption algorithm. AWS KMS can get this information from metadata that it adds to the symmetric ciphertext blob. However, if you prefer, you can specify the @KeyId@ to ensure that a particular CMK is used to decrypt the ciphertext. If you specify a different CMK than the one used to encrypt the ciphertext, the @Decrypt@ operation fails.
-- Whenever possible, use key policies to give users permission to call the Decrypt operation on a particular CMK, instead of using IAM policies. Otherwise, you might create an IAM user policy that gives the user Decrypt permission on all CMKs. This user could decrypt ciphertext that was encrypted by CMKs in other accounts if the key policy for the cross-account CMK permits it. If you must use an IAM policy for @Decrypt@ permissions, limit the user to particular CMKs or particular trusted accounts.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.Decrypt
  ( -- * Creating a request
    Decrypt (..),
    mkDecrypt,

    -- ** Request lenses
    decKeyId,
    decEncryptionContext,
    decGrantTokens,
    decEncryptionAlgorithm,
    decCiphertextBlob,

    -- * Destructuring the response
    DecryptResponse (..),
    mkDecryptResponse,

    -- ** Response lenses
    drsKeyId,
    drsPlaintext,
    drsEncryptionAlgorithm,
    drsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDecrypt' smart constructor.
data Decrypt = Decrypt'
  { keyId :: Lude.Maybe Lude.Text,
    encryptionContext ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    grantTokens :: Lude.Maybe [Lude.Text],
    encryptionAlgorithm :: Lude.Maybe EncryptionAlgorithmSpec,
    ciphertextBlob :: Lude.Base64
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Decrypt' with the minimum fields required to make a request.
--
-- * 'ciphertextBlob' - Ciphertext to be decrypted. The blob includes metadata.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'encryptionAlgorithm' - Specifies the encryption algorithm that will be used to decrypt the ciphertext. Specify the same algorithm that was used to encrypt the data. If you specify a different algorithm, the @Decrypt@ operation fails.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@ , represents the only supported algorithm that is valid for symmetric CMKs.
-- * 'encryptionContext' - Specifies the encryption context to use when decrypting the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyId' - Specifies the customer master key (CMK) that AWS KMS will use to decrypt the ciphertext. Enter a key ID of the CMK that was used to encrypt the ciphertext.
--
-- If you specify a @KeyId@ value, the @Decrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext.
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
mkDecrypt ::
  -- | 'ciphertextBlob'
  Lude.Base64 ->
  Decrypt
mkDecrypt pCiphertextBlob_ =
  Decrypt'
    { keyId = Lude.Nothing,
      encryptionContext = Lude.Nothing,
      grantTokens = Lude.Nothing,
      encryptionAlgorithm = Lude.Nothing,
      ciphertextBlob = pCiphertextBlob_
    }

-- | Specifies the customer master key (CMK) that AWS KMS will use to decrypt the ciphertext. Enter a key ID of the CMK that was used to encrypt the ciphertext.
--
-- If you specify a @KeyId@ value, the @Decrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext.
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
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decKeyId :: Lens.Lens' Decrypt (Lude.Maybe Lude.Text)
decKeyId = Lens.lens (keyId :: Decrypt -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: Decrypt)
{-# DEPRECATED decKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the encryption context to use when decrypting the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEncryptionContext :: Lens.Lens' Decrypt (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
decEncryptionContext = Lens.lens (encryptionContext :: Decrypt -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContext = a} :: Decrypt)
{-# DEPRECATED decEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decGrantTokens :: Lens.Lens' Decrypt (Lude.Maybe [Lude.Text])
decGrantTokens = Lens.lens (grantTokens :: Decrypt -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: Decrypt)
{-# DEPRECATED decGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Specifies the encryption algorithm that will be used to decrypt the ciphertext. Specify the same algorithm that was used to encrypt the data. If you specify a different algorithm, the @Decrypt@ operation fails.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@ , represents the only supported algorithm that is valid for symmetric CMKs.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEncryptionAlgorithm :: Lens.Lens' Decrypt (Lude.Maybe EncryptionAlgorithmSpec)
decEncryptionAlgorithm = Lens.lens (encryptionAlgorithm :: Decrypt -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {encryptionAlgorithm = a} :: Decrypt)
{-# DEPRECATED decEncryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead." #-}

-- | Ciphertext to be decrypted. The blob includes metadata.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decCiphertextBlob :: Lens.Lens' Decrypt Lude.Base64
decCiphertextBlob = Lens.lens (ciphertextBlob :: Decrypt -> Lude.Base64) (\s a -> s {ciphertextBlob = a} :: Decrypt)
{-# DEPRECATED decCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

instance Lude.AWSRequest Decrypt where
  type Rs Decrypt = DecryptResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DecryptResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "Plaintext")
            Lude.<*> (x Lude..?> "EncryptionAlgorithm")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Decrypt where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.Decrypt" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON Decrypt where
  toJSON Decrypt' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyId" Lude..=) Lude.<$> keyId,
            ("EncryptionContext" Lude..=) Lude.<$> encryptionContext,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            ("EncryptionAlgorithm" Lude..=) Lude.<$> encryptionAlgorithm,
            Lude.Just ("CiphertextBlob" Lude..= ciphertextBlob)
          ]
      )

instance Lude.ToPath Decrypt where
  toPath = Lude.const "/"

instance Lude.ToQuery Decrypt where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDecryptResponse' smart constructor.
data DecryptResponse = DecryptResponse'
  { keyId ::
      Lude.Maybe Lude.Text,
    plaintext :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    encryptionAlgorithm :: Lude.Maybe EncryptionAlgorithmSpec,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecryptResponse' with the minimum fields required to make a request.
--
-- * 'encryptionAlgorithm' - The encryption algorithm that was used to decrypt the ciphertext.
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to decrypt the ciphertext.
-- * 'plaintext' - Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'responseStatus' - The response status code.
mkDecryptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DecryptResponse
mkDecryptResponse pResponseStatus_ =
  DecryptResponse'
    { keyId = Lude.Nothing,
      plaintext = Lude.Nothing,
      encryptionAlgorithm = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to decrypt the ciphertext.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsKeyId :: Lens.Lens' DecryptResponse (Lude.Maybe Lude.Text)
drsKeyId = Lens.lens (keyId :: DecryptResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: DecryptResponse)
{-# DEPRECATED drsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPlaintext :: Lens.Lens' DecryptResponse (Lude.Maybe (Lude.Sensitive Lude.Base64))
drsPlaintext = Lens.lens (plaintext :: DecryptResponse -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {plaintext = a} :: DecryptResponse)
{-# DEPRECATED drsPlaintext "Use generic-lens or generic-optics with 'plaintext' instead." #-}

-- | The encryption algorithm that was used to decrypt the ciphertext.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEncryptionAlgorithm :: Lens.Lens' DecryptResponse (Lude.Maybe EncryptionAlgorithmSpec)
drsEncryptionAlgorithm = Lens.lens (encryptionAlgorithm :: DecryptResponse -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {encryptionAlgorithm = a} :: DecryptResponse)
{-# DEPRECATED drsEncryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DecryptResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DecryptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DecryptResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
