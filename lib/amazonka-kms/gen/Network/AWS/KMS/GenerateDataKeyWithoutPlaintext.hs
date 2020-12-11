{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key. This operation returns a data key that is encrypted under a customer master key (CMK) that you specify. To request an asymmetric data key pair, use the 'GenerateDataKeyPair' or 'GenerateDataKeyPairWithoutPlaintext' operations.
--
-- @GenerateDataKeyWithoutPlaintext@ is identical to the 'GenerateDataKey' operation except that returns only the encrypted copy of the data key. This operation is useful for systems that need to encrypt data at some point, but not immediately. When you need to encrypt the data, you call the 'Decrypt' operation on the encrypted copy of the key.
-- It's also useful in distributed systems with different levels of trust. For example, you might store encrypted data in containers. One component of your system creates new containers and stores an encrypted data key with each container. Then, a different component puts the data into the containers. That component first decrypts the data key, uses the plaintext data key to encrypt data, puts the encrypted data into the container, and then destroys the plaintext data key. In this system, the component that creates the containers never sees the plaintext data key.
-- @GenerateDataKeyWithoutPlaintext@ returns a unique data key for each request. The bytes in the keys are not related to the caller or CMK that is used to encrypt the private key.
-- To generate a data key, you must specify the symmetric customer master key (CMK) that is used to encrypt the data key. You cannot use an asymmetric CMK to generate a data key. To get the type of your CMK, use the 'DescribeKey' operation.
-- If the operation succeeds, you will find the encrypted copy of the data key in the @CiphertextBlob@ field.
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
  ( -- * Creating a request
    GenerateDataKeyWithoutPlaintext (..),
    mkGenerateDataKeyWithoutPlaintext,

    -- ** Request lenses
    gdkwpKeySpec,
    gdkwpEncryptionContext,
    gdkwpNumberOfBytes,
    gdkwpGrantTokens,
    gdkwpKeyId,

    -- * Destructuring the response
    GenerateDataKeyWithoutPlaintextResponse (..),
    mkGenerateDataKeyWithoutPlaintextResponse,

    -- ** Response lenses
    gdkwprsKeyId,
    gdkwprsCiphertextBlob,
    gdkwprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateDataKeyWithoutPlaintext' smart constructor.
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'
  { keySpec ::
      Lude.Maybe DataKeySpec,
    encryptionContext ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    numberOfBytes ::
      Lude.Maybe Lude.Natural,
    grantTokens ::
      Lude.Maybe [Lude.Text],
    keyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKeyWithoutPlaintext' with the minimum fields required to make a request.
--
-- * 'encryptionContext' - Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyId' - The identifier of the symmetric customer master key (CMK) that encrypts the data key.
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
-- * 'keySpec' - The length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
-- * 'numberOfBytes' - The length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
mkGenerateDataKeyWithoutPlaintext ::
  -- | 'keyId'
  Lude.Text ->
  GenerateDataKeyWithoutPlaintext
mkGenerateDataKeyWithoutPlaintext pKeyId_ =
  GenerateDataKeyWithoutPlaintext'
    { keySpec = Lude.Nothing,
      encryptionContext = Lude.Nothing,
      numberOfBytes = Lude.Nothing,
      grantTokens = Lude.Nothing,
      keyId = pKeyId_
    }

-- | The length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- /Note:/ Consider using 'keySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpKeySpec :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Lude.Maybe DataKeySpec)
gdkwpKeySpec = Lens.lens (keySpec :: GenerateDataKeyWithoutPlaintext -> Lude.Maybe DataKeySpec) (\s a -> s {keySpec = a} :: GenerateDataKeyWithoutPlaintext)
{-# DEPRECATED gdkwpKeySpec "Use generic-lens or generic-optics with 'keySpec' instead." #-}

-- | Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpEncryptionContext :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdkwpEncryptionContext = Lens.lens (encryptionContext :: GenerateDataKeyWithoutPlaintext -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContext = a} :: GenerateDataKeyWithoutPlaintext)
{-# DEPRECATED gdkwpEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | The length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
--
-- /Note:/ Consider using 'numberOfBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpNumberOfBytes :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Lude.Maybe Lude.Natural)
gdkwpNumberOfBytes = Lens.lens (numberOfBytes :: GenerateDataKeyWithoutPlaintext -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfBytes = a} :: GenerateDataKeyWithoutPlaintext)
{-# DEPRECATED gdkwpNumberOfBytes "Use generic-lens or generic-optics with 'numberOfBytes' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpGrantTokens :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Lude.Maybe [Lude.Text])
gdkwpGrantTokens = Lens.lens (grantTokens :: GenerateDataKeyWithoutPlaintext -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: GenerateDataKeyWithoutPlaintext)
{-# DEPRECATED gdkwpGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | The identifier of the symmetric customer master key (CMK) that encrypts the data key.
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
gdkwpKeyId :: Lens.Lens' GenerateDataKeyWithoutPlaintext Lude.Text
gdkwpKeyId = Lens.lens (keyId :: GenerateDataKeyWithoutPlaintext -> Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyWithoutPlaintext)
{-# DEPRECATED gdkwpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest GenerateDataKeyWithoutPlaintext where
  type
    Rs GenerateDataKeyWithoutPlaintext =
      GenerateDataKeyWithoutPlaintextResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateDataKeyWithoutPlaintextResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "CiphertextBlob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateDataKeyWithoutPlaintext where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "TrentService.GenerateDataKeyWithoutPlaintext" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateDataKeyWithoutPlaintext where
  toJSON GenerateDataKeyWithoutPlaintext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeySpec" Lude..=) Lude.<$> keySpec,
            ("EncryptionContext" Lude..=) Lude.<$> encryptionContext,
            ("NumberOfBytes" Lude..=) Lude.<$> numberOfBytes,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            Lude.Just ("KeyId" Lude..= keyId)
          ]
      )

instance Lude.ToPath GenerateDataKeyWithoutPlaintext where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateDataKeyWithoutPlaintext where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateDataKeyWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'
  { keyId ::
      Lude.Maybe
        Lude.Text,
    ciphertextBlob ::
      Lude.Maybe
        Lude.Base64,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKeyWithoutPlaintextResponse' with the minimum fields required to make a request.
--
-- * 'ciphertextBlob' - The encrypted data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
-- * 'responseStatus' - The response status code.
mkGenerateDataKeyWithoutPlaintextResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateDataKeyWithoutPlaintextResponse
mkGenerateDataKeyWithoutPlaintextResponse pResponseStatus_ =
  GenerateDataKeyWithoutPlaintextResponse'
    { keyId = Lude.Nothing,
      ciphertextBlob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwprsKeyId :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse (Lude.Maybe Lude.Text)
gdkwprsKeyId = Lens.lens (keyId :: GenerateDataKeyWithoutPlaintextResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyWithoutPlaintextResponse)
{-# DEPRECATED gdkwprsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The encrypted data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwprsCiphertextBlob :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse (Lude.Maybe Lude.Base64)
gdkwprsCiphertextBlob = Lens.lens (ciphertextBlob :: GenerateDataKeyWithoutPlaintextResponse -> Lude.Maybe Lude.Base64) (\s a -> s {ciphertextBlob = a} :: GenerateDataKeyWithoutPlaintextResponse)
{-# DEPRECATED gdkwprsCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwprsResponseStatus :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse Lude.Int
gdkwprsResponseStatus = Lens.lens (responseStatus :: GenerateDataKeyWithoutPlaintextResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateDataKeyWithoutPlaintextResponse)
{-# DEPRECATED gdkwprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
