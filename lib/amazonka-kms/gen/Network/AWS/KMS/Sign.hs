{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--
-- Digital signatures are generated and verified by using asymmetric key pair, such as an RSA or ECC pair that is represented by an asymmetric customer master key (CMK). The key owner (or an authorized user) uses their private key to sign a message. Anyone with the public key can verify that the message was signed with that particular private key and that the message hasn't changed since it was signed.
--
-- To use the @Sign@ operation, provide the following information:
--
--     * Use the @KeyId@ parameter to identify an asymmetric CMK with a @KeyUsage@ value of @SIGN_VERIFY@ . To get the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation. The caller must have @kms:Sign@ permission on the CMK.
--
--     * Use the @Message@ parameter to specify the message or message digest to sign. You can submit messages of up to 4096 bytes. To sign a larger message, generate a hash digest of the message, and then provide the hash digest in the @Message@ parameter. To indicate whether the message is a full message or a digest, use the @MessageType@ parameter.
--
--     * Choose a signing algorithm that is compatible with the CMK.
--
--
--
-- /Important:/ When signing a message, be sure to record the CMK and the signing algorithm. This information is required to verify the signature.
--
-- To verify the signature that this operation generates, use the 'Verify' operation. Or use the 'GetPublicKey' operation to download the public key and then use the public key to verify the signature outside of AWS KMS.
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.Sign
  ( -- * Creating a Request
    sign,
    Sign,

    -- * Request Lenses
    sMessageType,
    sGrantTokens,
    sKeyId,
    sMessage,
    sSigningAlgorithm,

    -- * Destructuring the Response
    signResponse,
    SignResponse,

    -- * Response Lenses
    srsSigningAlgorithm,
    srsSignature,
    srsKeyId,
    srsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sign' smart constructor.
data Sign = Sign'
  { _sMessageType :: !(Maybe MessageType),
    _sGrantTokens :: !(Maybe [Text]),
    _sKeyId :: !Text,
    _sMessage :: !(Sensitive Base64),
    _sSigningAlgorithm :: !SigningAlgorithmSpec
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Sign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sMessageType' - Tells AWS KMS whether the value of the @Message@ parameter is a message or message digest. The default value, RAW, indicates a message. To indicate a message digest, enter @DIGEST@ .
--
-- * 'sGrantTokens' - A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'sKeyId' - Identifies an asymmetric CMK. AWS KMS uses the private key in the asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- * 'sMessage' - Specifies the message or message digest to sign. Messages can be 0-4096 bytes. To sign a larger message, provide the message digest. If you provide a message, AWS KMS generates a hash digest of the message and then signs it.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'sSigningAlgorithm' - Specifies the signing algorithm to use when signing the message.  Choose an algorithm that is compatible with the type and size of the specified asymmetric CMK.
sign ::
  -- | 'sKeyId'
  Text ->
  -- | 'sMessage'
  ByteString ->
  -- | 'sSigningAlgorithm'
  SigningAlgorithmSpec ->
  Sign
sign pKeyId_ pMessage_ pSigningAlgorithm_ =
  Sign'
    { _sMessageType = Nothing,
      _sGrantTokens = Nothing,
      _sKeyId = pKeyId_,
      _sMessage = _Sensitive . _Base64 # pMessage_,
      _sSigningAlgorithm = pSigningAlgorithm_
    }

-- | Tells AWS KMS whether the value of the @Message@ parameter is a message or message digest. The default value, RAW, indicates a message. To indicate a message digest, enter @DIGEST@ .
sMessageType :: Lens' Sign (Maybe MessageType)
sMessageType = lens _sMessageType (\s a -> s {_sMessageType = a})

-- | A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
sGrantTokens :: Lens' Sign [Text]
sGrantTokens = lens _sGrantTokens (\s a -> s {_sGrantTokens = a}) . _Default . _Coerce

-- | Identifies an asymmetric CMK. AWS KMS uses the private key in the asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
sKeyId :: Lens' Sign Text
sKeyId = lens _sKeyId (\s a -> s {_sKeyId = a})

-- | Specifies the message or message digest to sign. Messages can be 0-4096 bytes. To sign a larger message, provide the message digest. If you provide a message, AWS KMS generates a hash digest of the message and then signs it.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
sMessage :: Lens' Sign ByteString
sMessage = lens _sMessage (\s a -> s {_sMessage = a}) . _Sensitive . _Base64

-- | Specifies the signing algorithm to use when signing the message.  Choose an algorithm that is compatible with the type and size of the specified asymmetric CMK.
sSigningAlgorithm :: Lens' Sign SigningAlgorithmSpec
sSigningAlgorithm = lens _sSigningAlgorithm (\s a -> s {_sSigningAlgorithm = a})

instance AWSRequest Sign where
  type Rs Sign = SignResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          SignResponse'
            <$> (x .?> "SigningAlgorithm")
            <*> (x .?> "Signature")
            <*> (x .?> "KeyId")
            <*> (pure (fromEnum s))
      )

instance Hashable Sign

instance NFData Sign

instance ToHeaders Sign where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("TrentService.Sign" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON Sign where
  toJSON Sign' {..} =
    object
      ( catMaybes
          [ ("MessageType" .=) <$> _sMessageType,
            ("GrantTokens" .=) <$> _sGrantTokens,
            Just ("KeyId" .= _sKeyId),
            Just ("Message" .= _sMessage),
            Just ("SigningAlgorithm" .= _sSigningAlgorithm)
          ]
      )

instance ToPath Sign where
  toPath = const "/"

instance ToQuery Sign where
  toQuery = const mempty

-- | /See:/ 'signResponse' smart constructor.
data SignResponse = SignResponse'
  { _srsSigningAlgorithm ::
      !(Maybe SigningAlgorithmSpec),
    _srsSignature :: !(Maybe Base64),
    _srsKeyId :: !(Maybe Text),
    _srsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsSigningAlgorithm' - The signing algorithm that was used to sign the message.
--
-- * 'srsSignature' - The cryptographic signature that was generated for the message.      * When used with the supported RSA signing algorithms, the encoding of this value is defined by <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017> .     * When used with the @ECDSA_SHA_256@ , @ECDSA_SHA_384@ , or @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded object as defined by ANS X9.62–2005 and <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3> . This is the most commonly used signature format and is appropriate for most uses.  When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'srsKeyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK that was used to sign the message.
--
-- * 'srsResponseStatus' - -- | The response status code.
signResponse ::
  -- | 'srsResponseStatus'
  Int ->
  SignResponse
signResponse pResponseStatus_ =
  SignResponse'
    { _srsSigningAlgorithm = Nothing,
      _srsSignature = Nothing,
      _srsKeyId = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | The signing algorithm that was used to sign the message.
srsSigningAlgorithm :: Lens' SignResponse (Maybe SigningAlgorithmSpec)
srsSigningAlgorithm = lens _srsSigningAlgorithm (\s a -> s {_srsSigningAlgorithm = a})

-- | The cryptographic signature that was generated for the message.      * When used with the supported RSA signing algorithms, the encoding of this value is defined by <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017> .     * When used with the @ECDSA_SHA_256@ , @ECDSA_SHA_384@ , or @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded object as defined by ANS X9.62–2005 and <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3> . This is the most commonly used signature format and is appropriate for most uses.  When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
srsSignature :: Lens' SignResponse (Maybe ByteString)
srsSignature = lens _srsSignature (\s a -> s {_srsSignature = a}) . mapping _Base64

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK that was used to sign the message.
srsKeyId :: Lens' SignResponse (Maybe Text)
srsKeyId = lens _srsKeyId (\s a -> s {_srsKeyId = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' SignResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData SignResponse
