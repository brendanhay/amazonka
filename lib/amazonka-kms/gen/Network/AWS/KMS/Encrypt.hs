{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Encrypt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts plaintext into ciphertext by using a customer master key (CMK). The @Encrypt@ operation has two primary use cases:
--
--
--     * You can encrypt small amounts of arbitrary data, such as a personal identifier or database password, or other sensitive information.
--
--
--     * You can use the @Encrypt@ operation to move encrypted data from one AWS Region to another. For example, in Region A, generate a data key and use the plaintext key to encrypt your data. Then, in Region A, use the @Encrypt@ operation to encrypt the plaintext data key under a CMK in Region B. Now, you can move the encrypted data and the encrypted data key to Region B. When necessary, you can decrypt the encrypted data key and the encrypted data entirely within in Region B.
--
--
-- You don't need to use the @Encrypt@ operation to encrypt a data key. The 'GenerateDataKey' and 'GenerateDataKeyPair' operations return a plaintext data key and an encrypted copy of that data key.
-- When you encrypt data, you must specify a symmetric or asymmetric CMK to use in the encryption operation. The CMK must have a @KeyUsage@ value of @ENCRYPT_DECRYPT.@ To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
-- If you use a symmetric CMK, you can use an encryption context to add additional security to your encryption operation. If you specify an @EncryptionContext@ when encrypting data, you must specify the same encryption context (a case-sensitive exact match) when decrypting the data. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- If you specify an asymmetric CMK, you must also specify the encryption algorithm. The algorithm must be compatible with the CMK type.
-- /Important:/ When you use an asymmetric CMK to encrypt or reencrypt data, be sure to record the CMK and encryption algorithm that you choose. You will be required to provide the same CMK and encryption algorithm when you decrypt the data. If the CMK and algorithm do not match the values used to encrypt the data, the decrypt operation fails.
-- You are not required to supply the CMK ID and encryption algorithm when you decrypt with symmetric CMKs because AWS KMS stores this information in the ciphertext blob. AWS KMS cannot store metadata in ciphertext generated with asymmetric keys. The standard format for asymmetric key ciphertext does not include configurable fields.
-- The maximum size of the data that you can encrypt varies with the type of CMK and the encryption algorithm that you choose.
--
--     * Symmetric CMKs
--
--     * @SYMMETRIC_DEFAULT@ : 4096 bytes
--
--
--
--
--     * @RSA_2048@
--
--     * @RSAES_OAEP_SHA_1@ : 214 bytes
--
--
--     * @RSAES_OAEP_SHA_256@ : 190 bytes
--
--
--
--
--     * @RSA_3072@
--
--     * @RSAES_OAEP_SHA_1@ : 342 bytes
--
--
--     * @RSAES_OAEP_SHA_256@ : 318 bytes
--
--
--
--
--     * @RSA_4096@
--
--     * @RSAES_OAEP_SHA_1@ : 470 bytes
--
--
--     * @RSAES_OAEP_SHA_256@ : 446 bytes
--
--
--
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
-- To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
module Network.AWS.KMS.Encrypt
  ( -- * Creating a request
    Encrypt (..),
    mkEncrypt,

    -- ** Request lenses
    eKeyId,
    eEncryptionContext,
    eGrantTokens,
    ePlaintext,
    eEncryptionAlgorithm,

    -- * Destructuring the response
    EncryptResponse (..),
    mkEncryptResponse,

    -- ** Response lenses
    ersKeyId,
    ersEncryptionAlgorithm,
    ersCiphertextBlob,
    ersResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEncrypt' smart constructor.
data Encrypt = Encrypt'
  { -- | A unique identifier for the customer master key (CMK).
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
    keyId :: Lude.Text,
    -- | Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
    encryptionContext :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Lude.Maybe [Lude.Text],
    -- | Data to be encrypted.
    plaintext :: Lude.Sensitive Lude.Base64,
    -- | Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify.
    --
    -- This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
    encryptionAlgorithm :: Lude.Maybe EncryptionAlgorithmSpec
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Encrypt' with the minimum fields required to make a request.
--
-- * 'keyId' - A unique identifier for the customer master key (CMK).
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
-- * 'encryptionContext' - Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'plaintext' - Data to be encrypted.
-- * 'encryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify.
--
-- This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
mkEncrypt ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'plaintext'
  Lude.Sensitive Lude.Base64 ->
  Encrypt
mkEncrypt pKeyId_ pPlaintext_ =
  Encrypt'
    { keyId = pKeyId_,
      encryptionContext = Lude.Nothing,
      grantTokens = Lude.Nothing,
      plaintext = pPlaintext_,
      encryptionAlgorithm = Lude.Nothing
    }

-- | A unique identifier for the customer master key (CMK).
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
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKeyId :: Lens.Lens' Encrypt Lude.Text
eKeyId = Lens.lens (keyId :: Encrypt -> Lude.Text) (\s a -> s {keyId = a} :: Encrypt)
{-# DEPRECATED eKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionContext :: Lens.Lens' Encrypt (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
eEncryptionContext = Lens.lens (encryptionContext :: Encrypt -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContext = a} :: Encrypt)
{-# DEPRECATED eEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eGrantTokens :: Lens.Lens' Encrypt (Lude.Maybe [Lude.Text])
eGrantTokens = Lens.lens (grantTokens :: Encrypt -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: Encrypt)
{-# DEPRECATED eGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Data to be encrypted.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePlaintext :: Lens.Lens' Encrypt (Lude.Sensitive Lude.Base64)
ePlaintext = Lens.lens (plaintext :: Encrypt -> Lude.Sensitive Lude.Base64) (\s a -> s {plaintext = a} :: Encrypt)
{-# DEPRECATED ePlaintext "Use generic-lens or generic-optics with 'plaintext' instead." #-}

-- | Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify.
--
-- This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionAlgorithm :: Lens.Lens' Encrypt (Lude.Maybe EncryptionAlgorithmSpec)
eEncryptionAlgorithm = Lens.lens (encryptionAlgorithm :: Encrypt -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {encryptionAlgorithm = a} :: Encrypt)
{-# DEPRECATED eEncryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead." #-}

instance Lude.AWSRequest Encrypt where
  type Rs Encrypt = EncryptResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          EncryptResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "EncryptionAlgorithm")
            Lude.<*> (x Lude..?> "CiphertextBlob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Encrypt where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.Encrypt" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON Encrypt where
  toJSON Encrypt' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            ("EncryptionContext" Lude..=) Lude.<$> encryptionContext,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            Lude.Just ("Plaintext" Lude..= plaintext),
            ("EncryptionAlgorithm" Lude..=) Lude.<$> encryptionAlgorithm
          ]
      )

instance Lude.ToPath Encrypt where
  toPath = Lude.const "/"

instance Lude.ToQuery Encrypt where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEncryptResponse' smart constructor.
data EncryptResponse = EncryptResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
    keyId :: Lude.Maybe Lude.Text,
    -- | The encryption algorithm that was used to encrypt the plaintext.
    encryptionAlgorithm :: Lude.Maybe EncryptionAlgorithmSpec,
    -- | The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Lude.Maybe Lude.Base64,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
-- * 'encryptionAlgorithm' - The encryption algorithm that was used to encrypt the plaintext.
-- * 'ciphertextBlob' - The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
-- * 'responseStatus' - The response status code.
mkEncryptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EncryptResponse
mkEncryptResponse pResponseStatus_ =
  EncryptResponse'
    { keyId = Lude.Nothing,
      encryptionAlgorithm = Lude.Nothing,
      ciphertextBlob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersKeyId :: Lens.Lens' EncryptResponse (Lude.Maybe Lude.Text)
ersKeyId = Lens.lens (keyId :: EncryptResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: EncryptResponse)
{-# DEPRECATED ersKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The encryption algorithm that was used to encrypt the plaintext.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersEncryptionAlgorithm :: Lens.Lens' EncryptResponse (Lude.Maybe EncryptionAlgorithmSpec)
ersEncryptionAlgorithm = Lens.lens (encryptionAlgorithm :: EncryptResponse -> Lude.Maybe EncryptionAlgorithmSpec) (\s a -> s {encryptionAlgorithm = a} :: EncryptResponse)
{-# DEPRECATED ersEncryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead." #-}

-- | The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersCiphertextBlob :: Lens.Lens' EncryptResponse (Lude.Maybe Lude.Base64)
ersCiphertextBlob = Lens.lens (ciphertextBlob :: EncryptResponse -> Lude.Maybe Lude.Base64) (\s a -> s {ciphertextBlob = a} :: EncryptResponse)
{-# DEPRECATED ersCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersResponseStatus :: Lens.Lens' EncryptResponse Lude.Int
ersResponseStatus = Lens.lens (responseStatus :: EncryptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EncryptResponse)
{-# DEPRECATED ersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
