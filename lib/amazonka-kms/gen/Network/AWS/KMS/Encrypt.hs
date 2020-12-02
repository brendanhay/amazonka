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
--     * You can use the @Encrypt@ operation to move encrypted data from one AWS Region to another. For example, in Region A, generate a data key and use the plaintext key to encrypt your data. Then, in Region A, use the @Encrypt@ operation to encrypt the plaintext data key under a CMK in Region B. Now, you can move the encrypted data and the encrypted data key to Region B. When necessary, you can decrypt the encrypted data key and the encrypted data entirely within in Region B.
--
--
--
-- You don't need to use the @Encrypt@ operation to encrypt a data key. The 'GenerateDataKey' and 'GenerateDataKeyPair' operations return a plaintext data key and an encrypted copy of that data key.
--
-- When you encrypt data, you must specify a symmetric or asymmetric CMK to use in the encryption operation. The CMK must have a @KeyUsage@ value of @ENCRYPT_DECRYPT.@ To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
--
-- If you use a symmetric CMK, you can use an encryption context to add additional security to your encryption operation. If you specify an @EncryptionContext@ when encrypting data, you must specify the same encryption context (a case-sensitive exact match) when decrypting the data. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- If you specify an asymmetric CMK, you must also specify the encryption algorithm. The algorithm must be compatible with the CMK type.
--
-- /Important:/ When you use an asymmetric CMK to encrypt or reencrypt data, be sure to record the CMK and encryption algorithm that you choose. You will be required to provide the same CMK and encryption algorithm when you decrypt the data. If the CMK and algorithm do not match the values used to encrypt the data, the decrypt operation fails.
--
-- You are not required to supply the CMK ID and encryption algorithm when you decrypt with symmetric CMKs because AWS KMS stores this information in the ciphertext blob. AWS KMS cannot store metadata in ciphertext generated with asymmetric keys. The standard format for asymmetric key ciphertext does not include configurable fields.
--
-- The maximum size of the data that you can encrypt varies with the type of CMK and the encryption algorithm that you choose.
--
--     * Symmetric CMKs
--
--     * @SYMMETRIC_DEFAULT@ : 4096 bytes
--
--
--
--     * @RSA_2048@
--
--     * @RSAES_OAEP_SHA_1@ : 214 bytes
--
--     * @RSAES_OAEP_SHA_256@ : 190 bytes
--
--
--
--     * @RSA_3072@
--
--     * @RSAES_OAEP_SHA_1@ : 342 bytes
--
--     * @RSAES_OAEP_SHA_256@ : 318 bytes
--
--
--
--     * @RSA_4096@
--
--     * @RSAES_OAEP_SHA_1@ : 470 bytes
--
--     * @RSAES_OAEP_SHA_256@ : 446 bytes
--
--
--
--
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
module Network.AWS.KMS.Encrypt
  ( -- * Creating a Request
    encrypt,
    Encrypt,

    -- * Request Lenses
    eEncryptionContext,
    eGrantTokens,
    eEncryptionAlgorithm,
    eKeyId,
    ePlaintext,

    -- * Destructuring the Response
    encryptResponse,
    EncryptResponse,

    -- * Response Lenses
    ersKeyId,
    ersEncryptionAlgorithm,
    ersCiphertextBlob,
    ersResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'encrypt' smart constructor.
data Encrypt = Encrypt'
  { _eEncryptionContext ::
      !(Maybe (Map Text (Text))),
    _eGrantTokens :: !(Maybe [Text]),
    _eEncryptionAlgorithm :: !(Maybe EncryptionAlgorithmSpec),
    _eKeyId :: !Text,
    _ePlaintext :: !(Sensitive Base64)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Encrypt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eEncryptionContext' - Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.  An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'eGrantTokens' - A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'eEncryptionAlgorithm' - Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify. This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
--
-- * 'eKeyId' - A unique identifier for the customer master key (CMK). To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- * 'ePlaintext' - Data to be encrypted.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
encrypt ::
  -- | 'eKeyId'
  Text ->
  -- | 'ePlaintext'
  ByteString ->
  Encrypt
encrypt pKeyId_ pPlaintext_ =
  Encrypt'
    { _eEncryptionContext = Nothing,
      _eGrantTokens = Nothing,
      _eEncryptionAlgorithm = Nothing,
      _eKeyId = pKeyId_,
      _ePlaintext = _Sensitive . _Base64 # pPlaintext_
    }

-- | Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.  An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
eEncryptionContext :: Lens' Encrypt (HashMap Text (Text))
eEncryptionContext = lens _eEncryptionContext (\s a -> s {_eEncryptionContext = a}) . _Default . _Map

-- | A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
eGrantTokens :: Lens' Encrypt [Text]
eGrantTokens = lens _eGrantTokens (\s a -> s {_eGrantTokens = a}) . _Default . _Coerce

-- | Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify. This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
eEncryptionAlgorithm :: Lens' Encrypt (Maybe EncryptionAlgorithmSpec)
eEncryptionAlgorithm = lens _eEncryptionAlgorithm (\s a -> s {_eEncryptionAlgorithm = a})

-- | A unique identifier for the customer master key (CMK). To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
eKeyId :: Lens' Encrypt Text
eKeyId = lens _eKeyId (\s a -> s {_eKeyId = a})

-- | Data to be encrypted.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ePlaintext :: Lens' Encrypt ByteString
ePlaintext = lens _ePlaintext (\s a -> s {_ePlaintext = a}) . _Sensitive . _Base64

instance AWSRequest Encrypt where
  type Rs Encrypt = EncryptResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          EncryptResponse'
            <$> (x .?> "KeyId")
            <*> (x .?> "EncryptionAlgorithm")
            <*> (x .?> "CiphertextBlob")
            <*> (pure (fromEnum s))
      )

instance Hashable Encrypt

instance NFData Encrypt

instance ToHeaders Encrypt where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("TrentService.Encrypt" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON Encrypt where
  toJSON Encrypt' {..} =
    object
      ( catMaybes
          [ ("EncryptionContext" .=) <$> _eEncryptionContext,
            ("GrantTokens" .=) <$> _eGrantTokens,
            ("EncryptionAlgorithm" .=) <$> _eEncryptionAlgorithm,
            Just ("KeyId" .= _eKeyId),
            Just ("Plaintext" .= _ePlaintext)
          ]
      )

instance ToPath Encrypt where
  toPath = const "/"

instance ToQuery Encrypt where
  toQuery = const mempty

-- | /See:/ 'encryptResponse' smart constructor.
data EncryptResponse = EncryptResponse'
  { _ersKeyId :: !(Maybe Text),
    _ersEncryptionAlgorithm :: !(Maybe EncryptionAlgorithmSpec),
    _ersCiphertextBlob :: !(Maybe Base64),
    _ersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ersKeyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
--
-- * 'ersEncryptionAlgorithm' - The encryption algorithm that was used to encrypt the plaintext.
--
-- * 'ersCiphertextBlob' - The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'ersResponseStatus' - -- | The response status code.
encryptResponse ::
  -- | 'ersResponseStatus'
  Int ->
  EncryptResponse
encryptResponse pResponseStatus_ =
  EncryptResponse'
    { _ersKeyId = Nothing,
      _ersEncryptionAlgorithm = Nothing,
      _ersCiphertextBlob = Nothing,
      _ersResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
ersKeyId :: Lens' EncryptResponse (Maybe Text)
ersKeyId = lens _ersKeyId (\s a -> s {_ersKeyId = a})

-- | The encryption algorithm that was used to encrypt the plaintext.
ersEncryptionAlgorithm :: Lens' EncryptResponse (Maybe EncryptionAlgorithmSpec)
ersEncryptionAlgorithm = lens _ersEncryptionAlgorithm (\s a -> s {_ersEncryptionAlgorithm = a})

-- | The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ersCiphertextBlob :: Lens' EncryptResponse (Maybe ByteString)
ersCiphertextBlob = lens _ersCiphertextBlob (\s a -> s {_ersCiphertextBlob = a}) . mapping _Base64

-- | -- | The response status code.
ersResponseStatus :: Lens' EncryptResponse Int
ersResponseStatus = lens _ersResponseStatus (\s a -> s {_ersResponseStatus = a})

instance NFData EncryptResponse
