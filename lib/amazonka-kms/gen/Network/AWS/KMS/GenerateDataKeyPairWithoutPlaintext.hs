{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique asymmetric data key pair. The @GenerateDataKeyPairWithoutPlaintext@ operation returns a plaintext public key and a copy of the private key that is encrypted under the symmetric CMK you specify. Unlike 'GenerateDataKeyPair' , this operation does not return a plaintext private key.
--
-- To generate a data key pair, you must specify a symmetric customer master key (CMK) to encrypt the private key in the data key pair. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the @KeySpec@ field in the 'DescribeKey' response.
-- You can use the public key that @GenerateDataKeyPairWithoutPlaintext@ returns to encrypt data or verify a signature outside of AWS KMS. Then, store the encrypted private key with the data. When you are ready to decrypt data or sign a message, you can use the 'Decrypt' operation to decrypt the encrypted private key.
-- @GenerateDataKeyPairWithoutPlaintext@ returns a unique data key pair for each request. The bytes in the key are not related to the caller or CMK that is used to encrypt the private key.
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
  ( -- * Creating a request
    GenerateDataKeyPairWithoutPlaintext (..),
    mkGenerateDataKeyPairWithoutPlaintext,

    -- ** Request lenses
    gdkpwpEncryptionContext,
    gdkpwpGrantTokens,
    gdkpwpKeyId,
    gdkpwpKeyPairSpec,

    -- * Destructuring the response
    GenerateDataKeyPairWithoutPlaintextResponse (..),
    mkGenerateDataKeyPairWithoutPlaintextResponse,

    -- ** Response lenses
    gdkpwprsKeyId,
    gdkpwprsPublicKey,
    gdkpwprsKeyPairSpec,
    gdkpwprsPrivateKeyCiphertextBlob,
    gdkpwprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateDataKeyPairWithoutPlaintext' smart constructor.
data GenerateDataKeyPairWithoutPlaintext = GenerateDataKeyPairWithoutPlaintext'
  { encryptionContext ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    grantTokens ::
      Lude.Maybe
        [Lude.Text],
    keyId :: Lude.Text,
    keyPairSpec ::
      DataKeyPairSpec
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKeyPairWithoutPlaintext' with the minimum fields required to make a request.
--
-- * 'encryptionContext' - Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyId' - Specifies the CMK that encrypts the private key in the data key pair. You must specify a symmetric CMK. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
--
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
-- * 'keyPairSpec' - Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
mkGenerateDataKeyPairWithoutPlaintext ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'keyPairSpec'
  DataKeyPairSpec ->
  GenerateDataKeyPairWithoutPlaintext
mkGenerateDataKeyPairWithoutPlaintext pKeyId_ pKeyPairSpec_ =
  GenerateDataKeyPairWithoutPlaintext'
    { encryptionContext =
        Lude.Nothing,
      grantTokens = Lude.Nothing,
      keyId = pKeyId_,
      keyPairSpec = pKeyPairSpec_
    }

-- | Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpEncryptionContext :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdkpwpEncryptionContext = Lens.lens (encryptionContext :: GenerateDataKeyPairWithoutPlaintext -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContext = a} :: GenerateDataKeyPairWithoutPlaintext)
{-# DEPRECATED gdkpwpEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpGrantTokens :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext (Lude.Maybe [Lude.Text])
gdkpwpGrantTokens = Lens.lens (grantTokens :: GenerateDataKeyPairWithoutPlaintext -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: GenerateDataKeyPairWithoutPlaintext)
{-# DEPRECATED gdkpwpGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Specifies the CMK that encrypts the private key in the data key pair. You must specify a symmetric CMK. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
--
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
gdkpwpKeyId :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext Lude.Text
gdkpwpKeyId = Lens.lens (keyId :: GenerateDataKeyPairWithoutPlaintext -> Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyPairWithoutPlaintext)
{-# DEPRECATED gdkpwpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwpKeyPairSpec :: Lens.Lens' GenerateDataKeyPairWithoutPlaintext DataKeyPairSpec
gdkpwpKeyPairSpec = Lens.lens (keyPairSpec :: GenerateDataKeyPairWithoutPlaintext -> DataKeyPairSpec) (\s a -> s {keyPairSpec = a} :: GenerateDataKeyPairWithoutPlaintext)
{-# DEPRECATED gdkpwpKeyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead." #-}

instance Lude.AWSRequest GenerateDataKeyPairWithoutPlaintext where
  type
    Rs GenerateDataKeyPairWithoutPlaintext =
      GenerateDataKeyPairWithoutPlaintextResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateDataKeyPairWithoutPlaintextResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "PublicKey")
            Lude.<*> (x Lude..?> "KeyPairSpec")
            Lude.<*> (x Lude..?> "PrivateKeyCiphertextBlob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateDataKeyPairWithoutPlaintext where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "TrentService.GenerateDataKeyPairWithoutPlaintext" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateDataKeyPairWithoutPlaintext where
  toJSON GenerateDataKeyPairWithoutPlaintext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EncryptionContext" Lude..=) Lude.<$> encryptionContext,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("KeyPairSpec" Lude..= keyPairSpec)
          ]
      )

instance Lude.ToPath GenerateDataKeyPairWithoutPlaintext where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateDataKeyPairWithoutPlaintext where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateDataKeyPairWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyPairWithoutPlaintextResponse = GenerateDataKeyPairWithoutPlaintextResponse'
  { keyId ::
      Lude.Maybe
        Lude.Text,
    publicKey ::
      Lude.Maybe
        Lude.Base64,
    keyPairSpec ::
      Lude.Maybe
        DataKeyPairSpec,
    privateKeyCiphertextBlob ::
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

-- | Creates a value of 'GenerateDataKeyPairWithoutPlaintextResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
-- * 'keyPairSpec' - The type of data key pair that was generated.
-- * 'privateKeyCiphertextBlob' - The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'publicKey' - The public key (in plaintext).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'responseStatus' - The response status code.
mkGenerateDataKeyPairWithoutPlaintextResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateDataKeyPairWithoutPlaintextResponse
mkGenerateDataKeyPairWithoutPlaintextResponse pResponseStatus_ =
  GenerateDataKeyPairWithoutPlaintextResponse'
    { keyId =
        Lude.Nothing,
      publicKey = Lude.Nothing,
      keyPairSpec = Lude.Nothing,
      privateKeyCiphertextBlob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprsKeyId :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Lude.Maybe Lude.Text)
gdkpwprsKeyId = Lens.lens (keyId :: GenerateDataKeyPairWithoutPlaintextResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyPairWithoutPlaintextResponse)
{-# DEPRECATED gdkpwprsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The public key (in plaintext).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprsPublicKey :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Lude.Maybe Lude.Base64)
gdkpwprsPublicKey = Lens.lens (publicKey :: GenerateDataKeyPairWithoutPlaintextResponse -> Lude.Maybe Lude.Base64) (\s a -> s {publicKey = a} :: GenerateDataKeyPairWithoutPlaintextResponse)
{-# DEPRECATED gdkpwprsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The type of data key pair that was generated.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprsKeyPairSpec :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Lude.Maybe DataKeyPairSpec)
gdkpwprsKeyPairSpec = Lens.lens (keyPairSpec :: GenerateDataKeyPairWithoutPlaintextResponse -> Lude.Maybe DataKeyPairSpec) (\s a -> s {keyPairSpec = a} :: GenerateDataKeyPairWithoutPlaintextResponse)
{-# DEPRECATED gdkpwprsKeyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead." #-}

-- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKeyCiphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprsPrivateKeyCiphertextBlob :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse (Lude.Maybe Lude.Base64)
gdkpwprsPrivateKeyCiphertextBlob = Lens.lens (privateKeyCiphertextBlob :: GenerateDataKeyPairWithoutPlaintextResponse -> Lude.Maybe Lude.Base64) (\s a -> s {privateKeyCiphertextBlob = a} :: GenerateDataKeyPairWithoutPlaintextResponse)
{-# DEPRECATED gdkpwprsPrivateKeyCiphertextBlob "Use generic-lens or generic-optics with 'privateKeyCiphertextBlob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpwprsResponseStatus :: Lens.Lens' GenerateDataKeyPairWithoutPlaintextResponse Lude.Int
gdkpwprsResponseStatus = Lens.lens (responseStatus :: GenerateDataKeyPairWithoutPlaintextResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateDataKeyPairWithoutPlaintextResponse)
{-# DEPRECATED gdkpwprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
