{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the public key of an asymmetric CMK. Unlike the private key of a asymmetric CMK, which never leaves AWS KMS unencrypted, callers with @kms:GetPublicKey@ permission can download the public key of an asymmetric CMK. You can share the public key to allow others to encrypt messages and verify signatures outside of AWS KMS. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
--
-- You do not need to download the public key. Instead, you can use the public key within AWS KMS by calling the 'Encrypt' , 'ReEncrypt' , or 'Verify' operations with the identifier of an asymmetric CMK. When you use the public key within AWS KMS, you benefit from the authentication, authorization, and logging that are part of every AWS KMS operation. You also reduce of risk of encrypting data that cannot be decrypted. These features are not effective outside of AWS KMS. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/download-public-key.html#download-public-key-considerations Special Considerations for Downloading Public Keys> .
-- To help you use the public key safely outside of AWS KMS, @GetPublicKey@ returns important information about the public key in the response, including:
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-CustomerMasterKeySpec CustomerMasterKeySpec> : The type of key material in the public key, such as @RSA_4096@ or @ECC_NIST_P521@ .
--
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-KeyUsage KeyUsage> : Whether the key is used for encryption or signing.
--
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-EncryptionAlgorithms EncryptionAlgorithms> or <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-SigningAlgorithms SigningAlgorithms> : A list of the encryption algorithms or the signing algorithms for the key.
--
--
-- Although AWS KMS cannot enforce these restrictions on external operations, it is crucial that you use this information to prevent the public key from being used improperly. For example, you can prevent a public signing key from being used encrypt data, or prevent a public key from being used with an encryption algorithm that is not supported by AWS KMS. You can also avoid errors, such as using the wrong signing algorithm in a verification operation.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GetPublicKey
  ( -- * Creating a request
    GetPublicKey (..),
    mkGetPublicKey,

    -- ** Request lenses
    gpkKeyId,
    gpkGrantTokens,

    -- * Destructuring the response
    GetPublicKeyResponse (..),
    mkGetPublicKeyResponse,

    -- ** Response lenses
    gpkrsKeyId,
    gpkrsCustomerMasterKeySpec,
    gpkrsEncryptionAlgorithms,
    gpkrsPublicKey,
    gpkrsSigningAlgorithms,
    gpkrsKeyUsage,
    gpkrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPublicKey' smart constructor.
data GetPublicKey = GetPublicKey'
  { -- | Identifies the asymmetric CMK that includes the public key.
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
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicKey' with the minimum fields required to make a request.
--
-- * 'keyId' - Identifies the asymmetric CMK that includes the public key.
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
mkGetPublicKey ::
  -- | 'keyId'
  Lude.Text ->
  GetPublicKey
mkGetPublicKey pKeyId_ =
  GetPublicKey' {keyId = pKeyId_, grantTokens = Lude.Nothing}

-- | Identifies the asymmetric CMK that includes the public key.
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
gpkKeyId :: Lens.Lens' GetPublicKey Lude.Text
gpkKeyId = Lens.lens (keyId :: GetPublicKey -> Lude.Text) (\s a -> s {keyId = a} :: GetPublicKey)
{-# DEPRECATED gpkKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkGrantTokens :: Lens.Lens' GetPublicKey (Lude.Maybe [Lude.Text])
gpkGrantTokens = Lens.lens (grantTokens :: GetPublicKey -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: GetPublicKey)
{-# DEPRECATED gpkGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

instance Lude.AWSRequest GetPublicKey where
  type Rs GetPublicKey = GetPublicKeyResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPublicKeyResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "CustomerMasterKeySpec")
            Lude.<*> (x Lude..?> "EncryptionAlgorithms" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PublicKey")
            Lude.<*> (x Lude..?> "SigningAlgorithms" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "KeyUsage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPublicKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GetPublicKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPublicKey where
  toJSON GetPublicKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            ("GrantTokens" Lude..=) Lude.<$> grantTokens
          ]
      )

instance Lude.ToPath GetPublicKey where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPublicKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
    keyId :: Lude.Maybe Lude.Text,
    -- | The type of the of the public key that was downloaded.
    customerMasterKeySpec :: Lude.Maybe CustomerMasterKeySpec,
    -- | The encryption algorithms that AWS KMS supports for this key.
    --
    -- This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted.
    -- This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
    encryptionAlgorithms :: Lude.Maybe [EncryptionAlgorithmSpec],
    -- | The exported public key.
    --
    -- The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    publicKey :: Lude.Maybe Lude.Base64,
    -- | The signing algorithms that AWS KMS supports for this key.
    --
    -- This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
    signingAlgorithms :: Lude.Maybe [SigningAlgorithmSpec],
    -- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .
    --
    -- This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted.
    keyUsage :: Lude.Maybe KeyUsageType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicKeyResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
-- * 'customerMasterKeySpec' - The type of the of the public key that was downloaded.
-- * 'encryptionAlgorithms' - The encryption algorithms that AWS KMS supports for this key.
--
-- This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted.
-- This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
-- * 'publicKey' - The exported public key.
--
-- The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
--
-- * 'signingAlgorithms' - The signing algorithms that AWS KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
-- * 'keyUsage' - The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted.
-- * 'responseStatus' - The response status code.
mkGetPublicKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPublicKeyResponse
mkGetPublicKeyResponse pResponseStatus_ =
  GetPublicKeyResponse'
    { keyId = Lude.Nothing,
      customerMasterKeySpec = Lude.Nothing,
      encryptionAlgorithms = Lude.Nothing,
      publicKey = Lude.Nothing,
      signingAlgorithms = Lude.Nothing,
      keyUsage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsKeyId :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe Lude.Text)
gpkrsKeyId = Lens.lens (keyId :: GetPublicKeyResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The type of the of the public key that was downloaded.
--
-- /Note:/ Consider using 'customerMasterKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsCustomerMasterKeySpec :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe CustomerMasterKeySpec)
gpkrsCustomerMasterKeySpec = Lens.lens (customerMasterKeySpec :: GetPublicKeyResponse -> Lude.Maybe CustomerMasterKeySpec) (\s a -> s {customerMasterKeySpec = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsCustomerMasterKeySpec "Use generic-lens or generic-optics with 'customerMasterKeySpec' instead." #-}

-- | The encryption algorithms that AWS KMS supports for this key.
--
-- This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted.
-- This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
--
-- /Note:/ Consider using 'encryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsEncryptionAlgorithms :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe [EncryptionAlgorithmSpec])
gpkrsEncryptionAlgorithms = Lens.lens (encryptionAlgorithms :: GetPublicKeyResponse -> Lude.Maybe [EncryptionAlgorithmSpec]) (\s a -> s {encryptionAlgorithms = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsEncryptionAlgorithms "Use generic-lens or generic-optics with 'encryptionAlgorithms' instead." #-}

-- | The exported public key.
--
-- The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.

----
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsPublicKey :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe Lude.Base64)
gpkrsPublicKey = Lens.lens (publicKey :: GetPublicKeyResponse -> Lude.Maybe Lude.Base64) (\s a -> s {publicKey = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The signing algorithms that AWS KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
--
-- /Note:/ Consider using 'signingAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsSigningAlgorithms :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe [SigningAlgorithmSpec])
gpkrsSigningAlgorithms = Lens.lens (signingAlgorithms :: GetPublicKeyResponse -> Lude.Maybe [SigningAlgorithmSpec]) (\s a -> s {signingAlgorithms = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsSigningAlgorithms "Use generic-lens or generic-optics with 'signingAlgorithms' instead." #-}

-- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted.
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsKeyUsage :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe KeyUsageType)
gpkrsKeyUsage = Lens.lens (keyUsage :: GetPublicKeyResponse -> Lude.Maybe KeyUsageType) (\s a -> s {keyUsage = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsKeyUsage "Use generic-lens or generic-optics with 'keyUsage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsResponseStatus :: Lens.Lens' GetPublicKeyResponse Lude.Int
gpkrsResponseStatus = Lens.lens (responseStatus :: GetPublicKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
