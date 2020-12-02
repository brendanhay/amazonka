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
-- Module      : Network.AWS.KMS.GetPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the public key of an asymmetric CMK. Unlike the private key of a asymmetric CMK, which never leaves AWS KMS unencrypted, callers with @kms:GetPublicKey@ permission can download the public key of an asymmetric CMK. You can share the public key to allow others to encrypt messages and verify signatures outside of AWS KMS. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
--
--
-- You do not need to download the public key. Instead, you can use the public key within AWS KMS by calling the 'Encrypt' , 'ReEncrypt' , or 'Verify' operations with the identifier of an asymmetric CMK. When you use the public key within AWS KMS, you benefit from the authentication, authorization, and logging that are part of every AWS KMS operation. You also reduce of risk of encrypting data that cannot be decrypted. These features are not effective outside of AWS KMS. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/download-public-key.html#download-public-key-considerations Special Considerations for Downloading Public Keys> .
--
-- To help you use the public key safely outside of AWS KMS, @GetPublicKey@ returns important information about the public key in the response, including:
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-CustomerMasterKeySpec CustomerMasterKeySpec> : The type of key material in the public key, such as @RSA_4096@ or @ECC_NIST_P521@ .
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-KeyUsage KeyUsage> : Whether the key is used for encryption or signing.
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-EncryptionAlgorithms EncryptionAlgorithms> or <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-SigningAlgorithms SigningAlgorithms> : A list of the encryption algorithms or the signing algorithms for the key.
--
--
--
-- Although AWS KMS cannot enforce these restrictions on external operations, it is crucial that you use this information to prevent the public key from being used improperly. For example, you can prevent a public signing key from being used encrypt data, or prevent a public key from being used with an encryption algorithm that is not supported by AWS KMS. You can also avoid errors, such as using the wrong signing algorithm in a verification operation.
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GetPublicKey
  ( -- * Creating a Request
    getPublicKey,
    GetPublicKey,

    -- * Request Lenses
    gpkGrantTokens,
    gpkKeyId,

    -- * Destructuring the Response
    getPublicKeyResponse,
    GetPublicKeyResponse,

    -- * Response Lenses
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPublicKey' smart constructor.
data GetPublicKey = GetPublicKey'
  { _gpkGrantTokens ::
      !(Maybe [Text]),
    _gpkKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkGrantTokens' - A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gpkKeyId' - Identifies the asymmetric CMK that includes the public key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
getPublicKey ::
  -- | 'gpkKeyId'
  Text ->
  GetPublicKey
getPublicKey pKeyId_ =
  GetPublicKey' {_gpkGrantTokens = Nothing, _gpkKeyId = pKeyId_}

-- | A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
gpkGrantTokens :: Lens' GetPublicKey [Text]
gpkGrantTokens = lens _gpkGrantTokens (\s a -> s {_gpkGrantTokens = a}) . _Default . _Coerce

-- | Identifies the asymmetric CMK that includes the public key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
gpkKeyId :: Lens' GetPublicKey Text
gpkKeyId = lens _gpkKeyId (\s a -> s {_gpkKeyId = a})

instance AWSRequest GetPublicKey where
  type Rs GetPublicKey = GetPublicKeyResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          GetPublicKeyResponse'
            <$> (x .?> "KeyId")
            <*> (x .?> "CustomerMasterKeySpec")
            <*> (x .?> "EncryptionAlgorithms" .!@ mempty)
            <*> (x .?> "PublicKey")
            <*> (x .?> "SigningAlgorithms" .!@ mempty)
            <*> (x .?> "KeyUsage")
            <*> (pure (fromEnum s))
      )

instance Hashable GetPublicKey

instance NFData GetPublicKey

instance ToHeaders GetPublicKey where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("TrentService.GetPublicKey" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetPublicKey where
  toJSON GetPublicKey' {..} =
    object
      ( catMaybes
          [ ("GrantTokens" .=) <$> _gpkGrantTokens,
            Just ("KeyId" .= _gpkKeyId)
          ]
      )

instance ToPath GetPublicKey where
  toPath = const "/"

instance ToQuery GetPublicKey where
  toQuery = const mempty

-- | /See:/ 'getPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { _gpkrsKeyId ::
      !(Maybe Text),
    _gpkrsCustomerMasterKeySpec ::
      !(Maybe CustomerMasterKeySpec),
    _gpkrsEncryptionAlgorithms ::
      !(Maybe [EncryptionAlgorithmSpec]),
    _gpkrsPublicKey :: !(Maybe Base64),
    _gpkrsSigningAlgorithms ::
      !(Maybe [SigningAlgorithmSpec]),
    _gpkrsKeyUsage :: !(Maybe KeyUsageType),
    _gpkrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkrsKeyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
--
-- * 'gpkrsCustomerMasterKeySpec' - The type of the of the public key that was downloaded.
--
-- * 'gpkrsEncryptionAlgorithms' - The encryption algorithms that AWS KMS supports for this key.  This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted.  This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
--
-- * 'gpkrsPublicKey' - The exported public key.  The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gpkrsSigningAlgorithms' - The signing algorithms that AWS KMS supports for this key. This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
--
-- * 'gpkrsKeyUsage' - The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .  This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted.
--
-- * 'gpkrsResponseStatus' - -- | The response status code.
getPublicKeyResponse ::
  -- | 'gpkrsResponseStatus'
  Int ->
  GetPublicKeyResponse
getPublicKeyResponse pResponseStatus_ =
  GetPublicKeyResponse'
    { _gpkrsKeyId = Nothing,
      _gpkrsCustomerMasterKeySpec = Nothing,
      _gpkrsEncryptionAlgorithms = Nothing,
      _gpkrsPublicKey = Nothing,
      _gpkrsSigningAlgorithms = Nothing,
      _gpkrsKeyUsage = Nothing,
      _gpkrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
gpkrsKeyId :: Lens' GetPublicKeyResponse (Maybe Text)
gpkrsKeyId = lens _gpkrsKeyId (\s a -> s {_gpkrsKeyId = a})

-- | The type of the of the public key that was downloaded.
gpkrsCustomerMasterKeySpec :: Lens' GetPublicKeyResponse (Maybe CustomerMasterKeySpec)
gpkrsCustomerMasterKeySpec = lens _gpkrsCustomerMasterKeySpec (\s a -> s {_gpkrsCustomerMasterKeySpec = a})

-- | The encryption algorithms that AWS KMS supports for this key.  This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted.  This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
gpkrsEncryptionAlgorithms :: Lens' GetPublicKeyResponse [EncryptionAlgorithmSpec]
gpkrsEncryptionAlgorithms = lens _gpkrsEncryptionAlgorithms (\s a -> s {_gpkrsEncryptionAlgorithms = a}) . _Default . _Coerce

-- | The exported public key.  The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gpkrsPublicKey :: Lens' GetPublicKeyResponse (Maybe ByteString)
gpkrsPublicKey = lens _gpkrsPublicKey (\s a -> s {_gpkrsPublicKey = a}) . mapping _Base64

-- | The signing algorithms that AWS KMS supports for this key. This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
gpkrsSigningAlgorithms :: Lens' GetPublicKeyResponse [SigningAlgorithmSpec]
gpkrsSigningAlgorithms = lens _gpkrsSigningAlgorithms (\s a -> s {_gpkrsSigningAlgorithms = a}) . _Default . _Coerce

-- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ .  This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted.
gpkrsKeyUsage :: Lens' GetPublicKeyResponse (Maybe KeyUsageType)
gpkrsKeyUsage = lens _gpkrsKeyUsage (\s a -> s {_gpkrsKeyUsage = a})

-- | -- | The response status code.
gpkrsResponseStatus :: Lens' GetPublicKeyResponse Int
gpkrsResponseStatus = lens _gpkrsResponseStatus (\s a -> s {_gpkrsResponseStatus = a})

instance NFData GetPublicKeyResponse
