{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a data encryption key that you can use in your application to encrypt data locally.
--
--
-- You must specify the customer master key (CMK) under which to generate the data key. You must also specify the length of the data key using either the @KeySpec@ or @NumberOfBytes@ field. You must specify one field or the other, but not both. For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use @KeySpec@ . To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
--
-- This operation returns a plaintext copy of the data key in the @Plaintext@ field of the response, and an encrypted copy of the data key in the @CiphertextBlob@ field. The data key is encrypted under the CMK specified in the @KeyId@ field of the request.
--
-- We recommend that you use the following pattern to encrypt data locally in your application:
--
--     * Use this operation (@GenerateDataKey@ ) to get a data encryption key.
--
--     * Use the plaintext data encryption key (returned in the @Plaintext@ field of the response) to encrypt data locally, then erase the plaintext data key from memory.
--
--     * Store the encrypted data key (returned in the @CiphertextBlob@ field of the response) alongside the locally encrypted data.
--
--
--
-- To decrypt data locally:
--
--     * Use the 'Decrypt' operation to decrypt the encrypted data key into a plaintext copy of the data key.
--
--     * Use the plaintext data key to decrypt data locally, then erase the plaintext data key from memory.
--
--
--
-- To return only an encrypted copy of the data key, use 'GenerateDataKeyWithoutPlaintext' . To return a random byte string that is cryptographically secure, use 'GenerateRandom' .
--
-- If you use the optional @EncryptionContext@ field, you must store at least enough information to be able to reconstruct the full encryption context when you later send the ciphertext to the 'Decrypt' operation. It is a good practice to choose an encryption context that you can reconstruct on the fly to better secure the ciphertext. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
module Network.AWS.KMS.GenerateDataKey
    (
    -- * Creating a Request
      generateDataKey
    , GenerateDataKey
    -- * Request Lenses
    , gdkKeySpec
    , gdkEncryptionContext
    , gdkNumberOfBytes
    , gdkGrantTokens
    , gdkKeyId

    -- * Destructuring the Response
    , generateDataKeyResponse
    , GenerateDataKeyResponse
    -- * Response Lenses
    , gdkrsResponseStatus
    , gdkrsKeyId
    , gdkrsPlaintext
    , gdkrsCiphertextBlob
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateDataKey' smart constructor.
data GenerateDataKey = GenerateDataKey'
  { _gdkKeySpec           :: !(Maybe DataKeySpec)
  , _gdkEncryptionContext :: !(Maybe (Map Text Text))
  , _gdkNumberOfBytes     :: !(Maybe Nat)
  , _gdkGrantTokens       :: !(Maybe [Text])
  , _gdkKeyId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateDataKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkKeySpec' - The length of the data encryption key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- * 'gdkEncryptionContext' - A set of key-value pairs that represents additional authenticated data. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkNumberOfBytes' - The length of the data encryption key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
--
-- * 'gdkGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkKeyId' - The identifier of the CMK under which to generate and encrypt the data encryption key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
generateDataKey
    :: Text -- ^ 'gdkKeyId'
    -> GenerateDataKey
generateDataKey pKeyId_ =
  GenerateDataKey'
    { _gdkKeySpec = Nothing
    , _gdkEncryptionContext = Nothing
    , _gdkNumberOfBytes = Nothing
    , _gdkGrantTokens = Nothing
    , _gdkKeyId = pKeyId_
    }


-- | The length of the data encryption key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
gdkKeySpec :: Lens' GenerateDataKey (Maybe DataKeySpec)
gdkKeySpec = lens _gdkKeySpec (\ s a -> s{_gdkKeySpec = a})

-- | A set of key-value pairs that represents additional authenticated data. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
gdkEncryptionContext :: Lens' GenerateDataKey (HashMap Text Text)
gdkEncryptionContext = lens _gdkEncryptionContext (\ s a -> s{_gdkEncryptionContext = a}) . _Default . _Map

-- | The length of the data encryption key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
gdkNumberOfBytes :: Lens' GenerateDataKey (Maybe Natural)
gdkNumberOfBytes = lens _gdkNumberOfBytes (\ s a -> s{_gdkNumberOfBytes = a}) . mapping _Nat

-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
gdkGrantTokens :: Lens' GenerateDataKey [Text]
gdkGrantTokens = lens _gdkGrantTokens (\ s a -> s{_gdkGrantTokens = a}) . _Default . _Coerce

-- | The identifier of the CMK under which to generate and encrypt the data encryption key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
gdkKeyId :: Lens' GenerateDataKey Text
gdkKeyId = lens _gdkKeyId (\ s a -> s{_gdkKeyId = a})

instance AWSRequest GenerateDataKey where
        type Rs GenerateDataKey = GenerateDataKeyResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 GenerateDataKeyResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "KeyId") <*>
                     (x .:> "Plaintext")
                     <*> (x .:> "CiphertextBlob"))

instance Hashable GenerateDataKey where

instance NFData GenerateDataKey where

instance ToHeaders GenerateDataKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GenerateDataKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateDataKey where
        toJSON GenerateDataKey'{..}
          = object
              (catMaybes
                 [("KeySpec" .=) <$> _gdkKeySpec,
                  ("EncryptionContext" .=) <$> _gdkEncryptionContext,
                  ("NumberOfBytes" .=) <$> _gdkNumberOfBytes,
                  ("GrantTokens" .=) <$> _gdkGrantTokens,
                  Just ("KeyId" .= _gdkKeyId)])

instance ToPath GenerateDataKey where
        toPath = const "/"

instance ToQuery GenerateDataKey where
        toQuery = const mempty

-- | /See:/ 'generateDataKeyResponse' smart constructor.
data GenerateDataKeyResponse = GenerateDataKeyResponse'
  { _gdkrsResponseStatus :: !Int
  , _gdkrsKeyId          :: !Text
  , _gdkrsPlaintext      :: !(Sensitive Base64)
  , _gdkrsCiphertextBlob :: !Base64
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateDataKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkrsResponseStatus' - -- | The response status code.
--
-- * 'gdkrsKeyId' - The identifier of the CMK under which the data encryption key was generated and encrypted.
--
-- * 'gdkrsPlaintext' - The data encryption key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded. Use this data key for local encryption and decryption, then remove it from memory as soon as possible.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkrsCiphertextBlob' - The encrypted data encryption key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
generateDataKeyResponse
    :: Int -- ^ 'gdkrsResponseStatus'
    -> Text -- ^ 'gdkrsKeyId'
    -> ByteString -- ^ 'gdkrsPlaintext'
    -> ByteString -- ^ 'gdkrsCiphertextBlob'
    -> GenerateDataKeyResponse
generateDataKeyResponse pResponseStatus_ pKeyId_ pPlaintext_ pCiphertextBlob_ =
  GenerateDataKeyResponse'
    { _gdkrsResponseStatus = pResponseStatus_
    , _gdkrsKeyId = pKeyId_
    , _gdkrsPlaintext = _Sensitive . _Base64 # pPlaintext_
    , _gdkrsCiphertextBlob = _Base64 # pCiphertextBlob_
    }


-- | -- | The response status code.
gdkrsResponseStatus :: Lens' GenerateDataKeyResponse Int
gdkrsResponseStatus = lens _gdkrsResponseStatus (\ s a -> s{_gdkrsResponseStatus = a})

-- | The identifier of the CMK under which the data encryption key was generated and encrypted.
gdkrsKeyId :: Lens' GenerateDataKeyResponse Text
gdkrsKeyId = lens _gdkrsKeyId (\ s a -> s{_gdkrsKeyId = a})

-- | The data encryption key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded. Use this data key for local encryption and decryption, then remove it from memory as soon as possible.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkrsPlaintext :: Lens' GenerateDataKeyResponse ByteString
gdkrsPlaintext = lens _gdkrsPlaintext (\ s a -> s{_gdkrsPlaintext = a}) . _Sensitive . _Base64

-- | The encrypted data encryption key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkrsCiphertextBlob :: Lens' GenerateDataKeyResponse ByteString
gdkrsCiphertextBlob = lens _gdkrsCiphertextBlob (\ s a -> s{_gdkrsCiphertextBlob = a}) . _Base64

instance NFData GenerateDataKeyResponse where
