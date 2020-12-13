{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key for client-side encryption. This operation returns a plaintext copy of the data key and a copy that is encrypted under a customer master key (CMK) that you specify. You can use the plaintext key to encrypt your data outside of AWS KMS and store the encrypted data key with the encrypted data.
--
-- @GenerateDataKey@ returns a unique data key for each request. The bytes in the plaintext key are not related to the caller or the CMK.
-- To generate a data key, specify the symmetric CMK that will be used to encrypt the data key. You cannot use an asymmetric CMK to generate data keys. To get the type of your CMK, use the 'DescribeKey' operation. You must also specify the length of the data key. Use either the @KeySpec@ or @NumberOfBytes@ parameters (but not both). For 128-bit and 256-bit data keys, use the @KeySpec@ parameter.
-- To get only an encrypted copy of the data key, use 'GenerateDataKeyWithoutPlaintext' . To generate an asymmetric data key pair, use the 'GenerateDataKeyPair' or 'GenerateDataKeyPairWithoutPlaintext' operation. To get a cryptographically secure random byte string, use 'GenerateRandom' .
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
-- __How to use your data key__
-- We recommend that you use the following pattern to encrypt data locally in your application. You can write your own code or use a client-side encryption library, such as the <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK> , the <https://docs.aws.amazon.com/dynamodb-encryption-client/latest/devguide/ Amazon DynamoDB Encryption Client> , or <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption> to do these tasks for you.
-- To encrypt data outside of AWS KMS:
--
--     * Use the @GenerateDataKey@ operation to get a data key.
--
--
--     * Use the plaintext data key (in the @Plaintext@ field of the response) to encrypt your data outside of AWS KMS. Then erase the plaintext data key from memory.
--
--
--     * Store the encrypted data key (in the @CiphertextBlob@ field of the response) with the encrypted data.
--
--
-- To decrypt data outside of AWS KMS:
--
--     * Use the 'Decrypt' operation to decrypt the encrypted data key. The operation returns a plaintext copy of the data key.
--
--
--     * Use the plaintext data key to decrypt data outside of AWS KMS, then erase the plaintext data key from memory.
module Network.AWS.KMS.GenerateDataKey
  ( -- * Creating a request
    GenerateDataKey (..),
    mkGenerateDataKey,

    -- ** Request lenses
    gdkKeySpec,
    gdkKeyId,
    gdkEncryptionContext,
    gdkNumberOfBytes,
    gdkGrantTokens,

    -- * Destructuring the response
    GenerateDataKeyResponse (..),
    mkGenerateDataKeyResponse,

    -- ** Response lenses
    gdkrsKeyId,
    gdkrsPlaintext,
    gdkrsCiphertextBlob,
    gdkrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateDataKey' smart constructor.
data GenerateDataKey = GenerateDataKey'
  { -- | Specifies the length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
    --
    -- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
    keySpec :: Lude.Maybe DataKeySpec,
    -- | Identifies the symmetric CMK that encrypts the data key.
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
    -- | Specifies the encryption context that will be used when encrypting the data key.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
    encryptionContext :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies the length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@ parameter.
    --
    -- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
    numberOfBytes :: Lude.Maybe Lude.Natural,
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKey' with the minimum fields required to make a request.
--
-- * 'keySpec' - Specifies the length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
-- * 'keyId' - Identifies the symmetric CMK that encrypts the data key.
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
-- * 'encryptionContext' - Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'numberOfBytes' - Specifies the length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@ parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
mkGenerateDataKey ::
  -- | 'keyId'
  Lude.Text ->
  GenerateDataKey
mkGenerateDataKey pKeyId_ =
  GenerateDataKey'
    { keySpec = Lude.Nothing,
      keyId = pKeyId_,
      encryptionContext = Lude.Nothing,
      numberOfBytes = Lude.Nothing,
      grantTokens = Lude.Nothing
    }

-- | Specifies the length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
--
-- /Note:/ Consider using 'keySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkKeySpec :: Lens.Lens' GenerateDataKey (Lude.Maybe DataKeySpec)
gdkKeySpec = Lens.lens (keySpec :: GenerateDataKey -> Lude.Maybe DataKeySpec) (\s a -> s {keySpec = a} :: GenerateDataKey)
{-# DEPRECATED gdkKeySpec "Use generic-lens or generic-optics with 'keySpec' instead." #-}

-- | Identifies the symmetric CMK that encrypts the data key.
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
gdkKeyId :: Lens.Lens' GenerateDataKey Lude.Text
gdkKeyId = Lens.lens (keyId :: GenerateDataKey -> Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKey)
{-# DEPRECATED gdkKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkEncryptionContext :: Lens.Lens' GenerateDataKey (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdkEncryptionContext = Lens.lens (encryptionContext :: GenerateDataKey -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContext = a} :: GenerateDataKey)
{-# DEPRECATED gdkEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | Specifies the length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@ parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
--
-- /Note:/ Consider using 'numberOfBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkNumberOfBytes :: Lens.Lens' GenerateDataKey (Lude.Maybe Lude.Natural)
gdkNumberOfBytes = Lens.lens (numberOfBytes :: GenerateDataKey -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfBytes = a} :: GenerateDataKey)
{-# DEPRECATED gdkNumberOfBytes "Use generic-lens or generic-optics with 'numberOfBytes' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkGrantTokens :: Lens.Lens' GenerateDataKey (Lude.Maybe [Lude.Text])
gdkGrantTokens = Lens.lens (grantTokens :: GenerateDataKey -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: GenerateDataKey)
{-# DEPRECATED gdkGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

instance Lude.AWSRequest GenerateDataKey where
  type Rs GenerateDataKey = GenerateDataKeyResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateDataKeyResponse'
            Lude.<$> (x Lude..:> "KeyId")
            Lude.<*> (x Lude..:> "Plaintext")
            Lude.<*> (x Lude..:> "CiphertextBlob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateDataKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GenerateDataKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateDataKey where
  toJSON GenerateDataKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeySpec" Lude..=) Lude.<$> keySpec,
            Lude.Just ("KeyId" Lude..= keyId),
            ("EncryptionContext" Lude..=) Lude.<$> encryptionContext,
            ("NumberOfBytes" Lude..=) Lude.<$> numberOfBytes,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens
          ]
      )

instance Lude.ToPath GenerateDataKey where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateDataKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateDataKeyResponse' smart constructor.
data GenerateDataKeyResponse = GenerateDataKeyResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
    keyId :: Lude.Text,
    -- | The plaintext data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this data key to encrypt your data outside of KMS. Then, remove it from memory as soon as possible.
    plaintext :: Lude.Sensitive Lude.Base64,
    -- | The encrypted copy of the data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Lude.Base64,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKeyResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
-- * 'plaintext' - The plaintext data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this data key to encrypt your data outside of KMS. Then, remove it from memory as soon as possible.
-- * 'ciphertextBlob' - The encrypted copy of the data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
-- * 'responseStatus' - The response status code.
mkGenerateDataKeyResponse ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'plaintext'
  Lude.Sensitive Lude.Base64 ->
  -- | 'ciphertextBlob'
  Lude.Base64 ->
  -- | 'responseStatus'
  Lude.Int ->
  GenerateDataKeyResponse
mkGenerateDataKeyResponse
  pKeyId_
  pPlaintext_
  pCiphertextBlob_
  pResponseStatus_ =
    GenerateDataKeyResponse'
      { keyId = pKeyId_,
        plaintext = pPlaintext_,
        ciphertextBlob = pCiphertextBlob_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrsKeyId :: Lens.Lens' GenerateDataKeyResponse Lude.Text
gdkrsKeyId = Lens.lens (keyId :: GenerateDataKeyResponse -> Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyResponse)
{-# DEPRECATED gdkrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The plaintext data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this data key to encrypt your data outside of KMS. Then, remove it from memory as soon as possible.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrsPlaintext :: Lens.Lens' GenerateDataKeyResponse (Lude.Sensitive Lude.Base64)
gdkrsPlaintext = Lens.lens (plaintext :: GenerateDataKeyResponse -> Lude.Sensitive Lude.Base64) (\s a -> s {plaintext = a} :: GenerateDataKeyResponse)
{-# DEPRECATED gdkrsPlaintext "Use generic-lens or generic-optics with 'plaintext' instead." #-}

-- | The encrypted copy of the data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrsCiphertextBlob :: Lens.Lens' GenerateDataKeyResponse Lude.Base64
gdkrsCiphertextBlob = Lens.lens (ciphertextBlob :: GenerateDataKeyResponse -> Lude.Base64) (\s a -> s {ciphertextBlob = a} :: GenerateDataKeyResponse)
{-# DEPRECATED gdkrsCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrsResponseStatus :: Lens.Lens' GenerateDataKeyResponse Lude.Int
gdkrsResponseStatus = Lens.lens (responseStatus :: GenerateDataKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateDataKeyResponse)
{-# DEPRECATED gdkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
