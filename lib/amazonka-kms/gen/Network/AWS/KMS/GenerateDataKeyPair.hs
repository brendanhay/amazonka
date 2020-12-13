{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique asymmetric data key pair. The @GenerateDataKeyPair@ operation returns a plaintext public key, a plaintext private key, and a copy of the private key that is encrypted under the symmetric CMK you specify. You can use the data key pair to perform asymmetric cryptography outside of AWS KMS.
--
-- @GenerateDataKeyPair@ returns a unique data key pair for each request. The bytes in the keys are not related to the caller or the CMK that is used to encrypt the private key.
-- You can use the public key that @GenerateDataKeyPair@ returns to encrypt data or verify a signature outside of AWS KMS. Then, store the encrypted private key with the data. When you are ready to decrypt data or sign a message, you can use the 'Decrypt' operation to decrypt the encrypted private key.
-- To generate a data key pair, you must specify a symmetric customer master key (CMK) to encrypt the private key in a data key pair. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
-- If you are using the data key pair to encrypt data, or for any operation where you don't immediately need a private key, consider using the 'GenerateDataKeyPairWithoutPlaintext' operation. @GenerateDataKeyPairWithoutPlaintext@ returns a plaintext public key and an encrypted private key, but omits the plaintext private key that you need only to decrypt ciphertext or sign a message. Later, when you need to decrypt the data or sign a message, use the 'Decrypt' operation to decrypt the encrypted private key in the data key pair.
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyPair
  ( -- * Creating a request
    GenerateDataKeyPair (..),
    mkGenerateDataKeyPair,

    -- ** Request lenses
    gdkpKeyId,
    gdkpEncryptionContext,
    gdkpGrantTokens,
    gdkpKeyPairSpec,

    -- * Destructuring the response
    GenerateDataKeyPairResponse (..),
    mkGenerateDataKeyPairResponse,

    -- ** Response lenses
    gdkprsKeyId,
    gdkprsPublicKey,
    gdkprsPrivateKeyPlaintext,
    gdkprsKeyPairSpec,
    gdkprsPrivateKeyCiphertextBlob,
    gdkprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateDataKeyPair' smart constructor.
data GenerateDataKeyPair = GenerateDataKeyPair'
  { -- | Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
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
    -- | Specifies the encryption context that will be used when encrypting the private key in the data key pair.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
    encryptionContext :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Lude.Maybe [Lude.Text],
    -- | Determines the type of data key pair that is generated.
    --
    -- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
    keyPairSpec :: DataKeyPairSpec
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKeyPair' with the minimum fields required to make a request.
--
-- * 'keyId' - Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
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
-- * 'encryptionContext' - Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyPairSpec' - Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
mkGenerateDataKeyPair ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'keyPairSpec'
  DataKeyPairSpec ->
  GenerateDataKeyPair
mkGenerateDataKeyPair pKeyId_ pKeyPairSpec_ =
  GenerateDataKeyPair'
    { keyId = pKeyId_,
      encryptionContext = Lude.Nothing,
      grantTokens = Lude.Nothing,
      keyPairSpec = pKeyPairSpec_
    }

-- | Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
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
gdkpKeyId :: Lens.Lens' GenerateDataKeyPair Lude.Text
gdkpKeyId = Lens.lens (keyId :: GenerateDataKeyPair -> Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyPair)
{-# DEPRECATED gdkpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpEncryptionContext :: Lens.Lens' GenerateDataKeyPair (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdkpEncryptionContext = Lens.lens (encryptionContext :: GenerateDataKeyPair -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {encryptionContext = a} :: GenerateDataKeyPair)
{-# DEPRECATED gdkpEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpGrantTokens :: Lens.Lens' GenerateDataKeyPair (Lude.Maybe [Lude.Text])
gdkpGrantTokens = Lens.lens (grantTokens :: GenerateDataKeyPair -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: GenerateDataKeyPair)
{-# DEPRECATED gdkpGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Determines the type of data key pair that is generated.
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpKeyPairSpec :: Lens.Lens' GenerateDataKeyPair DataKeyPairSpec
gdkpKeyPairSpec = Lens.lens (keyPairSpec :: GenerateDataKeyPair -> DataKeyPairSpec) (\s a -> s {keyPairSpec = a} :: GenerateDataKeyPair)
{-# DEPRECATED gdkpKeyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead." #-}

instance Lude.AWSRequest GenerateDataKeyPair where
  type Rs GenerateDataKeyPair = GenerateDataKeyPairResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateDataKeyPairResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "PublicKey")
            Lude.<*> (x Lude..?> "PrivateKeyPlaintext")
            Lude.<*> (x Lude..?> "KeyPairSpec")
            Lude.<*> (x Lude..?> "PrivateKeyCiphertextBlob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateDataKeyPair where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GenerateDataKeyPair" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateDataKeyPair where
  toJSON GenerateDataKeyPair' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            ("EncryptionContext" Lude..=) Lude.<$> encryptionContext,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            Lude.Just ("KeyPairSpec" Lude..= keyPairSpec)
          ]
      )

instance Lude.ToPath GenerateDataKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateDataKeyPair where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateDataKeyPairResponse' smart constructor.
data GenerateDataKeyPairResponse = GenerateDataKeyPairResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
    keyId :: Lude.Maybe Lude.Text,
    -- | The public key (in plaintext).
    publicKey :: Lude.Maybe Lude.Base64,
    -- | The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    privateKeyPlaintext :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | The type of data key pair that was generated.
    keyPairSpec :: Lude.Maybe DataKeyPairSpec,
    -- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    privateKeyCiphertextBlob :: Lude.Maybe Lude.Base64,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
-- * 'publicKey' - The public key (in plaintext).
-- * 'privateKeyPlaintext' - The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
-- * 'keyPairSpec' - The type of data key pair that was generated.
-- * 'privateKeyCiphertextBlob' - The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
-- * 'responseStatus' - The response status code.
mkGenerateDataKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateDataKeyPairResponse
mkGenerateDataKeyPairResponse pResponseStatus_ =
  GenerateDataKeyPairResponse'
    { keyId = Lude.Nothing,
      publicKey = Lude.Nothing,
      privateKeyPlaintext = Lude.Nothing,
      keyPairSpec = Lude.Nothing,
      privateKeyCiphertextBlob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprsKeyId :: Lens.Lens' GenerateDataKeyPairResponse (Lude.Maybe Lude.Text)
gdkprsKeyId = Lens.lens (keyId :: GenerateDataKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GenerateDataKeyPairResponse)
{-# DEPRECATED gdkprsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The public key (in plaintext).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprsPublicKey :: Lens.Lens' GenerateDataKeyPairResponse (Lude.Maybe Lude.Base64)
gdkprsPublicKey = Lens.lens (publicKey :: GenerateDataKeyPairResponse -> Lude.Maybe Lude.Base64) (\s a -> s {publicKey = a} :: GenerateDataKeyPairResponse)
{-# DEPRECATED gdkprsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKeyPlaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprsPrivateKeyPlaintext :: Lens.Lens' GenerateDataKeyPairResponse (Lude.Maybe (Lude.Sensitive Lude.Base64))
gdkprsPrivateKeyPlaintext = Lens.lens (privateKeyPlaintext :: GenerateDataKeyPairResponse -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {privateKeyPlaintext = a} :: GenerateDataKeyPairResponse)
{-# DEPRECATED gdkprsPrivateKeyPlaintext "Use generic-lens or generic-optics with 'privateKeyPlaintext' instead." #-}

-- | The type of data key pair that was generated.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprsKeyPairSpec :: Lens.Lens' GenerateDataKeyPairResponse (Lude.Maybe DataKeyPairSpec)
gdkprsKeyPairSpec = Lens.lens (keyPairSpec :: GenerateDataKeyPairResponse -> Lude.Maybe DataKeyPairSpec) (\s a -> s {keyPairSpec = a} :: GenerateDataKeyPairResponse)
{-# DEPRECATED gdkprsKeyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead." #-}

-- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKeyCiphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprsPrivateKeyCiphertextBlob :: Lens.Lens' GenerateDataKeyPairResponse (Lude.Maybe Lude.Base64)
gdkprsPrivateKeyCiphertextBlob = Lens.lens (privateKeyCiphertextBlob :: GenerateDataKeyPairResponse -> Lude.Maybe Lude.Base64) (\s a -> s {privateKeyCiphertextBlob = a} :: GenerateDataKeyPairResponse)
{-# DEPRECATED gdkprsPrivateKeyCiphertextBlob "Use generic-lens or generic-optics with 'privateKeyCiphertextBlob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprsResponseStatus :: Lens.Lens' GenerateDataKeyPairResponse Lude.Int
gdkprsResponseStatus = Lens.lens (responseStatus :: GenerateDataKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateDataKeyPairResponse)
{-# DEPRECATED gdkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
