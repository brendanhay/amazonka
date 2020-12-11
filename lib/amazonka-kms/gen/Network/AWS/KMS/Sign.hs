{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Sign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://en.wikipedia.org/wiki/Digital_signature digital signature> for a message or message digest by using the private key in an asymmetric CMK. To verify the signature, use the 'Verify' operation, or use the public key in the same asymmetric CMK outside of AWS KMS. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
--
-- Digital signatures are generated and verified by using asymmetric key pair, such as an RSA or ECC pair that is represented by an asymmetric customer master key (CMK). The key owner (or an authorized user) uses their private key to sign a message. Anyone with the public key can verify that the message was signed with that particular private key and that the message hasn't changed since it was signed.
-- To use the @Sign@ operation, provide the following information:
--
--     * Use the @KeyId@ parameter to identify an asymmetric CMK with a @KeyUsage@ value of @SIGN_VERIFY@ . To get the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation. The caller must have @kms:Sign@ permission on the CMK.
--
--
--     * Use the @Message@ parameter to specify the message or message digest to sign. You can submit messages of up to 4096 bytes. To sign a larger message, generate a hash digest of the message, and then provide the hash digest in the @Message@ parameter. To indicate whether the message is a full message or a digest, use the @MessageType@ parameter.
--
--
--     * Choose a signing algorithm that is compatible with the CMK.
--
--
-- /Important:/ When signing a message, be sure to record the CMK and the signing algorithm. This information is required to verify the signature.
-- To verify the signature that this operation generates, use the 'Verify' operation. Or use the 'GetPublicKey' operation to download the public key and then use the public key to verify the signature outside of AWS KMS.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.Sign
  ( -- * Creating a request
    Sign (..),
    mkSign,

    -- ** Request lenses
    sMessageType,
    sGrantTokens,
    sKeyId,
    sMessage,
    sSigningAlgorithm,

    -- * Destructuring the response
    SignResponse (..),
    mkSignResponse,

    -- ** Response lenses
    srsSigningAlgorithm,
    srsSignature,
    srsKeyId,
    srsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSign' smart constructor.
data Sign = Sign'
  { messageType :: Lude.Maybe MessageType,
    grantTokens :: Lude.Maybe [Lude.Text],
    keyId :: Lude.Text,
    message :: Lude.Sensitive Lude.Base64,
    signingAlgorithm :: SigningAlgorithmSpec
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Sign' with the minimum fields required to make a request.
--
-- * 'grantTokens' - A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
-- * 'keyId' - Identifies an asymmetric CMK. AWS KMS uses the private key in the asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
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
-- * 'message' - Specifies the message or message digest to sign. Messages can be 0-4096 bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, AWS KMS generates a hash digest of the message and then signs it.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'messageType' - Tells AWS KMS whether the value of the @Message@ parameter is a message or message digest. The default value, RAW, indicates a message. To indicate a message digest, enter @DIGEST@ .
-- * 'signingAlgorithm' - Specifies the signing algorithm to use when signing the message.
--
-- Choose an algorithm that is compatible with the type and size of the specified asymmetric CMK.
mkSign ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'message'
  Lude.Sensitive Lude.Base64 ->
  -- | 'signingAlgorithm'
  SigningAlgorithmSpec ->
  Sign
mkSign pKeyId_ pMessage_ pSigningAlgorithm_ =
  Sign'
    { messageType = Lude.Nothing,
      grantTokens = Lude.Nothing,
      keyId = pKeyId_,
      message = pMessage_,
      signingAlgorithm = pSigningAlgorithm_
    }

-- | Tells AWS KMS whether the value of the @Message@ parameter is a message or message digest. The default value, RAW, indicates a message. To indicate a message digest, enter @DIGEST@ .
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageType :: Lens.Lens' Sign (Lude.Maybe MessageType)
sMessageType = Lens.lens (messageType :: Sign -> Lude.Maybe MessageType) (\s a -> s {messageType = a} :: Sign)
{-# DEPRECATED sMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sGrantTokens :: Lens.Lens' Sign (Lude.Maybe [Lude.Text])
sGrantTokens = Lens.lens (grantTokens :: Sign -> Lude.Maybe [Lude.Text]) (\s a -> s {grantTokens = a} :: Sign)
{-# DEPRECATED sGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Identifies an asymmetric CMK. AWS KMS uses the private key in the asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
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
sKeyId :: Lens.Lens' Sign Lude.Text
sKeyId = Lens.lens (keyId :: Sign -> Lude.Text) (\s a -> s {keyId = a} :: Sign)
{-# DEPRECATED sKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the message or message digest to sign. Messages can be 0-4096 bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, AWS KMS generates a hash digest of the message and then signs it.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessage :: Lens.Lens' Sign (Lude.Sensitive Lude.Base64)
sMessage = Lens.lens (message :: Sign -> Lude.Sensitive Lude.Base64) (\s a -> s {message = a} :: Sign)
{-# DEPRECATED sMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Specifies the signing algorithm to use when signing the message.
--
-- Choose an algorithm that is compatible with the type and size of the specified asymmetric CMK.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSigningAlgorithm :: Lens.Lens' Sign SigningAlgorithmSpec
sSigningAlgorithm = Lens.lens (signingAlgorithm :: Sign -> SigningAlgorithmSpec) (\s a -> s {signingAlgorithm = a} :: Sign)
{-# DEPRECATED sSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

instance Lude.AWSRequest Sign where
  type Rs Sign = SignResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SignResponse'
            Lude.<$> (x Lude..?> "SigningAlgorithm")
            Lude.<*> (x Lude..?> "Signature")
            Lude.<*> (x Lude..?> "KeyId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Sign where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("TrentService.Sign" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON Sign where
  toJSON Sign' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MessageType" Lude..=) Lude.<$> messageType,
            ("GrantTokens" Lude..=) Lude.<$> grantTokens,
            Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("Message" Lude..= message),
            Lude.Just ("SigningAlgorithm" Lude..= signingAlgorithm)
          ]
      )

instance Lude.ToPath Sign where
  toPath = Lude.const "/"

instance Lude.ToQuery Sign where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSignResponse' smart constructor.
data SignResponse = SignResponse'
  { signingAlgorithm ::
      Lude.Maybe SigningAlgorithmSpec,
    signature :: Lude.Maybe Lude.Base64,
    keyId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SignResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK that was used to sign the message.
-- * 'responseStatus' - The response status code.
-- * 'signature' - The cryptographic signature that was generated for the message.
--
--
--     * When used with the supported RSA signing algorithms, the encoding of this value is defined by <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017> .
--
--
--     * When used with the @ECDSA_SHA_256@ , @ECDSA_SHA_384@ , or @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded object as defined by ANS X9.62–2005 and <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3> . This is the most commonly used signature format and is appropriate for most uses.
--
--
-- When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'signingAlgorithm' - The signing algorithm that was used to sign the message.
mkSignResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SignResponse
mkSignResponse pResponseStatus_ =
  SignResponse'
    { signingAlgorithm = Lude.Nothing,
      signature = Lude.Nothing,
      keyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The signing algorithm that was used to sign the message.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsSigningAlgorithm :: Lens.Lens' SignResponse (Lude.Maybe SigningAlgorithmSpec)
srsSigningAlgorithm = Lens.lens (signingAlgorithm :: SignResponse -> Lude.Maybe SigningAlgorithmSpec) (\s a -> s {signingAlgorithm = a} :: SignResponse)
{-# DEPRECATED srsSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

-- | The cryptographic signature that was generated for the message.
--
--
--     * When used with the supported RSA signing algorithms, the encoding of this value is defined by <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017> .
--
--
--     * When used with the @ECDSA_SHA_256@ , @ECDSA_SHA_384@ , or @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded object as defined by ANS X9.62–2005 and <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3> . This is the most commonly used signature format and is appropriate for most uses.
--
--
-- When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsSignature :: Lens.Lens' SignResponse (Lude.Maybe Lude.Base64)
srsSignature = Lens.lens (signature :: SignResponse -> Lude.Maybe Lude.Base64) (\s a -> s {signature = a} :: SignResponse)
{-# DEPRECATED srsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK that was used to sign the message.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKeyId :: Lens.Lens' SignResponse (Lude.Maybe Lude.Text)
srsKeyId = Lens.lens (keyId :: SignResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: SignResponse)
{-# DEPRECATED srsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SignResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SignResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SignResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
