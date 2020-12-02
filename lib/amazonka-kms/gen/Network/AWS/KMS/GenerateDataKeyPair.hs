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
-- Module      : Network.AWS.KMS.GenerateDataKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique asymmetric data key pair. The @GenerateDataKeyPair@ operation returns a plaintext public key, a plaintext private key, and a copy of the private key that is encrypted under the symmetric CMK you specify. You can use the data key pair to perform asymmetric cryptography outside of AWS KMS.
--
--
-- @GenerateDataKeyPair@ returns a unique data key pair for each request. The bytes in the keys are not related to the caller or the CMK that is used to encrypt the private key.
--
-- You can use the public key that @GenerateDataKeyPair@ returns to encrypt data or verify a signature outside of AWS KMS. Then, store the encrypted private key with the data. When you are ready to decrypt data or sign a message, you can use the 'Decrypt' operation to decrypt the encrypted private key.
--
-- To generate a data key pair, you must specify a symmetric customer master key (CMK) to encrypt the private key in a data key pair. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
--
-- If you are using the data key pair to encrypt data, or for any operation where you don't immediately need a private key, consider using the 'GenerateDataKeyPairWithoutPlaintext' operation. @GenerateDataKeyPairWithoutPlaintext@ returns a plaintext public key and an encrypted private key, but omits the plaintext private key that you need only to decrypt ciphertext or sign a message. Later, when you need to decrypt the data or sign a message, use the 'Decrypt' operation to decrypt the encrypted private key in the data key pair.
--
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyPair
  ( -- * Creating a Request
    generateDataKeyPair,
    GenerateDataKeyPair,

    -- * Request Lenses
    gdkpEncryptionContext,
    gdkpGrantTokens,
    gdkpKeyId,
    gdkpKeyPairSpec,

    -- * Destructuring the Response
    generateDataKeyPairResponse,
    GenerateDataKeyPairResponse,

    -- * Response Lenses
    gdkprsKeyId,
    gdkprsPublicKey,
    gdkprsPrivateKeyPlaintext,
    gdkprsKeyPairSpec,
    gdkprsPrivateKeyCiphertextBlob,
    gdkprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateDataKeyPair' smart constructor.
data GenerateDataKeyPair = GenerateDataKeyPair'
  { _gdkpEncryptionContext ::
      !(Maybe (Map Text (Text))),
    _gdkpGrantTokens :: !(Maybe [Text]),
    _gdkpKeyId :: !Text,
    _gdkpKeyPairSpec :: !DataKeyPairSpec
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateDataKeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkpEncryptionContext' - Specifies the encryption context that will be used when encrypting the private key in the data key pair. An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkpGrantTokens' - A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkpKeyId' - Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- * 'gdkpKeyPairSpec' - Determines the type of data key pair that is generated.  The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
generateDataKeyPair ::
  -- | 'gdkpKeyId'
  Text ->
  -- | 'gdkpKeyPairSpec'
  DataKeyPairSpec ->
  GenerateDataKeyPair
generateDataKeyPair pKeyId_ pKeyPairSpec_ =
  GenerateDataKeyPair'
    { _gdkpEncryptionContext = Nothing,
      _gdkpGrantTokens = Nothing,
      _gdkpKeyId = pKeyId_,
      _gdkpKeyPairSpec = pKeyPairSpec_
    }

-- | Specifies the encryption context that will be used when encrypting the private key in the data key pair. An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
gdkpEncryptionContext :: Lens' GenerateDataKeyPair (HashMap Text (Text))
gdkpEncryptionContext = lens _gdkpEncryptionContext (\s a -> s {_gdkpEncryptionContext = a}) . _Default . _Map

-- | A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
gdkpGrantTokens :: Lens' GenerateDataKeyPair [Text]
gdkpGrantTokens = lens _gdkpGrantTokens (\s a -> s {_gdkpGrantTokens = a}) . _Default . _Coerce

-- | Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
gdkpKeyId :: Lens' GenerateDataKeyPair Text
gdkpKeyId = lens _gdkpKeyId (\s a -> s {_gdkpKeyId = a})

-- | Determines the type of data key pair that is generated.  The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
gdkpKeyPairSpec :: Lens' GenerateDataKeyPair DataKeyPairSpec
gdkpKeyPairSpec = lens _gdkpKeyPairSpec (\s a -> s {_gdkpKeyPairSpec = a})

instance AWSRequest GenerateDataKeyPair where
  type Rs GenerateDataKeyPair = GenerateDataKeyPairResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          GenerateDataKeyPairResponse'
            <$> (x .?> "KeyId")
            <*> (x .?> "PublicKey")
            <*> (x .?> "PrivateKeyPlaintext")
            <*> (x .?> "KeyPairSpec")
            <*> (x .?> "PrivateKeyCiphertextBlob")
            <*> (pure (fromEnum s))
      )

instance Hashable GenerateDataKeyPair

instance NFData GenerateDataKeyPair

instance ToHeaders GenerateDataKeyPair where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.GenerateDataKeyPair" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GenerateDataKeyPair where
  toJSON GenerateDataKeyPair' {..} =
    object
      ( catMaybes
          [ ("EncryptionContext" .=) <$> _gdkpEncryptionContext,
            ("GrantTokens" .=) <$> _gdkpGrantTokens,
            Just ("KeyId" .= _gdkpKeyId),
            Just ("KeyPairSpec" .= _gdkpKeyPairSpec)
          ]
      )

instance ToPath GenerateDataKeyPair where
  toPath = const "/"

instance ToQuery GenerateDataKeyPair where
  toQuery = const mempty

-- | /See:/ 'generateDataKeyPairResponse' smart constructor.
data GenerateDataKeyPairResponse = GenerateDataKeyPairResponse'
  { _gdkprsKeyId ::
      !(Maybe Text),
    _gdkprsPublicKey :: !(Maybe Base64),
    _gdkprsPrivateKeyPlaintext ::
      !(Maybe (Sensitive Base64)),
    _gdkprsKeyPairSpec ::
      !(Maybe DataKeyPairSpec),
    _gdkprsPrivateKeyCiphertextBlob ::
      !(Maybe Base64),
    _gdkprsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateDataKeyPairResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkprsKeyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
--
-- * 'gdkprsPublicKey' - The public key (in plaintext).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkprsPrivateKeyPlaintext' - The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkprsKeyPairSpec' - The type of data key pair that was generated.
--
-- * 'gdkprsPrivateKeyCiphertextBlob' - The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkprsResponseStatus' - -- | The response status code.
generateDataKeyPairResponse ::
  -- | 'gdkprsResponseStatus'
  Int ->
  GenerateDataKeyPairResponse
generateDataKeyPairResponse pResponseStatus_ =
  GenerateDataKeyPairResponse'
    { _gdkprsKeyId = Nothing,
      _gdkprsPublicKey = Nothing,
      _gdkprsPrivateKeyPlaintext = Nothing,
      _gdkprsKeyPairSpec = Nothing,
      _gdkprsPrivateKeyCiphertextBlob = Nothing,
      _gdkprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
gdkprsKeyId :: Lens' GenerateDataKeyPairResponse (Maybe Text)
gdkprsKeyId = lens _gdkprsKeyId (\s a -> s {_gdkprsKeyId = a})

-- | The public key (in plaintext).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkprsPublicKey :: Lens' GenerateDataKeyPairResponse (Maybe ByteString)
gdkprsPublicKey = lens _gdkprsPublicKey (\s a -> s {_gdkprsPublicKey = a}) . mapping _Base64

-- | The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkprsPrivateKeyPlaintext :: Lens' GenerateDataKeyPairResponse (Maybe ByteString)
gdkprsPrivateKeyPlaintext = lens _gdkprsPrivateKeyPlaintext (\s a -> s {_gdkprsPrivateKeyPlaintext = a}) . mapping (_Sensitive . _Base64)

-- | The type of data key pair that was generated.
gdkprsKeyPairSpec :: Lens' GenerateDataKeyPairResponse (Maybe DataKeyPairSpec)
gdkprsKeyPairSpec = lens _gdkprsKeyPairSpec (\s a -> s {_gdkprsKeyPairSpec = a})

-- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkprsPrivateKeyCiphertextBlob :: Lens' GenerateDataKeyPairResponse (Maybe ByteString)
gdkprsPrivateKeyCiphertextBlob = lens _gdkprsPrivateKeyCiphertextBlob (\s a -> s {_gdkprsPrivateKeyCiphertextBlob = a}) . mapping _Base64

-- | -- | The response status code.
gdkprsResponseStatus :: Lens' GenerateDataKeyPairResponse Int
gdkprsResponseStatus = lens _gdkprsResponseStatus (\s a -> s {_gdkprsResponseStatus = a})

instance NFData GenerateDataKeyPairResponse
