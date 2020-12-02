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
-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key. This operation returns a data key that is encrypted under a customer master key (CMK) that you specify. To request an asymmetric data key pair, use the 'GenerateDataKeyPair' or 'GenerateDataKeyPairWithoutPlaintext' operations.
--
--
-- @GenerateDataKeyWithoutPlaintext@ is identical to the 'GenerateDataKey' operation except that returns only the encrypted copy of the data key. This operation is useful for systems that need to encrypt data at some point, but not immediately. When you need to encrypt the data, you call the 'Decrypt' operation on the encrypted copy of the key.
--
-- It's also useful in distributed systems with different levels of trust. For example, you might store encrypted data in containers. One component of your system creates new containers and stores an encrypted data key with each container. Then, a different component puts the data into the containers. That component first decrypts the data key, uses the plaintext data key to encrypt data, puts the encrypted data into the container, and then destroys the plaintext data key. In this system, the component that creates the containers never sees the plaintext data key.
--
-- @GenerateDataKeyWithoutPlaintext@ returns a unique data key for each request. The bytes in the keys are not related to the caller or CMK that is used to encrypt the private key.
--
-- To generate a data key, you must specify the symmetric customer master key (CMK) that is used to encrypt the data key. You cannot use an asymmetric CMK to generate a data key. To get the type of your CMK, use the 'DescribeKey' operation.
--
-- If the operation succeeds, you will find the encrypted copy of the data key in the @CiphertextBlob@ field.
--
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
  ( -- * Creating a Request
    generateDataKeyWithoutPlaintext,
    GenerateDataKeyWithoutPlaintext,

    -- * Request Lenses
    gdkwpKeySpec,
    gdkwpEncryptionContext,
    gdkwpNumberOfBytes,
    gdkwpGrantTokens,
    gdkwpKeyId,

    -- * Destructuring the Response
    generateDataKeyWithoutPlaintextResponse,
    GenerateDataKeyWithoutPlaintextResponse,

    -- * Response Lenses
    gdkwprsKeyId,
    gdkwprsCiphertextBlob,
    gdkwprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateDataKeyWithoutPlaintext' smart constructor.
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'
  { _gdkwpKeySpec ::
      !(Maybe DataKeySpec),
    _gdkwpEncryptionContext ::
      !(Maybe (Map Text (Text))),
    _gdkwpNumberOfBytes ::
      !(Maybe Nat),
    _gdkwpGrantTokens ::
      !(Maybe [Text]),
    _gdkwpKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateDataKeyWithoutPlaintext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkwpKeySpec' - The length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- * 'gdkwpEncryptionContext' - Specifies the encryption context that will be used when encrypting the data key. An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkwpNumberOfBytes' - The length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
--
-- * 'gdkwpGrantTokens' - A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkwpKeyId' - The identifier of the symmetric customer master key (CMK) that encrypts the data key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
generateDataKeyWithoutPlaintext ::
  -- | 'gdkwpKeyId'
  Text ->
  GenerateDataKeyWithoutPlaintext
generateDataKeyWithoutPlaintext pKeyId_ =
  GenerateDataKeyWithoutPlaintext'
    { _gdkwpKeySpec = Nothing,
      _gdkwpEncryptionContext = Nothing,
      _gdkwpNumberOfBytes = Nothing,
      _gdkwpGrantTokens = Nothing,
      _gdkwpKeyId = pKeyId_
    }

-- | The length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
gdkwpKeySpec :: Lens' GenerateDataKeyWithoutPlaintext (Maybe DataKeySpec)
gdkwpKeySpec = lens _gdkwpKeySpec (\s a -> s {_gdkwpKeySpec = a})

-- | Specifies the encryption context that will be used when encrypting the data key. An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
gdkwpEncryptionContext :: Lens' GenerateDataKeyWithoutPlaintext (HashMap Text (Text))
gdkwpEncryptionContext = lens _gdkwpEncryptionContext (\s a -> s {_gdkwpEncryptionContext = a}) . _Default . _Map

-- | The length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
gdkwpNumberOfBytes :: Lens' GenerateDataKeyWithoutPlaintext (Maybe Natural)
gdkwpNumberOfBytes = lens _gdkwpNumberOfBytes (\s a -> s {_gdkwpNumberOfBytes = a}) . mapping _Nat

-- | A list of grant tokens. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
gdkwpGrantTokens :: Lens' GenerateDataKeyWithoutPlaintext [Text]
gdkwpGrantTokens = lens _gdkwpGrantTokens (\s a -> s {_gdkwpGrantTokens = a}) . _Default . _Coerce

-- | The identifier of the symmetric customer master key (CMK) that encrypts the data key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
gdkwpKeyId :: Lens' GenerateDataKeyWithoutPlaintext Text
gdkwpKeyId = lens _gdkwpKeyId (\s a -> s {_gdkwpKeyId = a})

instance AWSRequest GenerateDataKeyWithoutPlaintext where
  type
    Rs GenerateDataKeyWithoutPlaintext =
      GenerateDataKeyWithoutPlaintextResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          GenerateDataKeyWithoutPlaintextResponse'
            <$> (x .?> "KeyId")
            <*> (x .?> "CiphertextBlob")
            <*> (pure (fromEnum s))
      )

instance Hashable GenerateDataKeyWithoutPlaintext

instance NFData GenerateDataKeyWithoutPlaintext

instance ToHeaders GenerateDataKeyWithoutPlaintext where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.GenerateDataKeyWithoutPlaintext" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GenerateDataKeyWithoutPlaintext where
  toJSON GenerateDataKeyWithoutPlaintext' {..} =
    object
      ( catMaybes
          [ ("KeySpec" .=) <$> _gdkwpKeySpec,
            ("EncryptionContext" .=) <$> _gdkwpEncryptionContext,
            ("NumberOfBytes" .=) <$> _gdkwpNumberOfBytes,
            ("GrantTokens" .=) <$> _gdkwpGrantTokens,
            Just ("KeyId" .= _gdkwpKeyId)
          ]
      )

instance ToPath GenerateDataKeyWithoutPlaintext where
  toPath = const "/"

instance ToQuery GenerateDataKeyWithoutPlaintext where
  toQuery = const mempty

-- | /See:/ 'generateDataKeyWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'
  { _gdkwprsKeyId ::
      !( Maybe
           Text
       ),
    _gdkwprsCiphertextBlob ::
      !( Maybe
           Base64
       ),
    _gdkwprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateDataKeyWithoutPlaintextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkwprsKeyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
--
-- * 'gdkwprsCiphertextBlob' - The encrypted data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkwprsResponseStatus' - -- | The response status code.
generateDataKeyWithoutPlaintextResponse ::
  -- | 'gdkwprsResponseStatus'
  Int ->
  GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintextResponse pResponseStatus_ =
  GenerateDataKeyWithoutPlaintextResponse'
    { _gdkwprsKeyId = Nothing,
      _gdkwprsCiphertextBlob = Nothing,
      _gdkwprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
gdkwprsKeyId :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Text)
gdkwprsKeyId = lens _gdkwprsKeyId (\s a -> s {_gdkwprsKeyId = a})

-- | The encrypted data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkwprsCiphertextBlob :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe ByteString)
gdkwprsCiphertextBlob = lens _gdkwprsCiphertextBlob (\s a -> s {_gdkwprsCiphertextBlob = a}) . mapping _Base64

-- | -- | The response status code.
gdkwprsResponseStatus :: Lens' GenerateDataKeyWithoutPlaintextResponse Int
gdkwprsResponseStatus = lens _gdkwprsResponseStatus (\s a -> s {_gdkwprsResponseStatus = a})

instance NFData GenerateDataKeyWithoutPlaintextResponse
