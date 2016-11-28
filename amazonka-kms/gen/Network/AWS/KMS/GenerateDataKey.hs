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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a data key that you can use in your application to locally encrypt data. This call returns a plaintext version of the key in the @Plaintext@ field of the response object and an encrypted copy of the key in the @CiphertextBlob@ field. The key is encrypted by using the master key specified by the @KeyId@ field. To decrypt the encrypted key, pass it to the @Decrypt@ API.
--
--
-- We recommend that you use the following pattern to locally encrypt data: call the @GenerateDataKey@ API, use the key returned in the @Plaintext@ response field to locally encrypt data, and then erase the plaintext data key from memory. Store the encrypted data key (contained in the @CiphertextBlob@ field) alongside of the locally encrypted data.
--
-- If you decide to use the optional @EncryptionContext@ parameter, you must also store the context in full or at least store enough information along with the encrypted data to be able to reconstruct the context when submitting the ciphertext to the @Decrypt@ API. It is a good practice to choose a context that you can reconstruct on the fly to better secure the ciphertext. For more information about how this parameter is used, see <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context> .
--
-- To decrypt data, pass the encrypted data key to the @Decrypt@ API. @Decrypt@ uses the associated master key to decrypt the encrypted data key and returns it as plaintext. Use the plaintext data key to locally decrypt your data and then erase the key from memory. You must specify the encryption context, if any, that you specified when you generated the key. The encryption context is logged by CloudTrail, and you can use this log to help track the use of particular data.
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

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateDataKey' smart constructor.
data GenerateDataKey = GenerateDataKey'
    { _gdkKeySpec           :: !(Maybe DataKeySpec)
    , _gdkEncryptionContext :: !(Maybe (Map Text Text))
    , _gdkNumberOfBytes     :: !(Maybe Nat)
    , _gdkGrantTokens       :: !(Maybe [Text])
    , _gdkKeyId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateDataKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkKeySpec' - Value that identifies the encryption algorithm and key size to generate a data key for. Currently this can be AES_128 or AES_256.
--
-- * 'gdkEncryptionContext' - Name/value pair that contains additional data to be authenticated during the encryption and decryption processes that use the key. This value is logged by AWS CloudTrail to provide context around the data encrypted by the key.
--
-- * 'gdkNumberOfBytes' - Integer that contains the number of bytes to generate. Common values are 128, 256, 512, and 1024. 1024 is the current limit. We recommend that you use the @KeySpec@ parameter instead.
--
-- * 'gdkGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkKeyId' - A unique identifier for the customer master key. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Alias ARN Example - arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012     * Alias Name Example - alias/MyAliasName
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

-- | Value that identifies the encryption algorithm and key size to generate a data key for. Currently this can be AES_128 or AES_256.
gdkKeySpec :: Lens' GenerateDataKey (Maybe DataKeySpec)
gdkKeySpec = lens _gdkKeySpec (\ s a -> s{_gdkKeySpec = a});

-- | Name/value pair that contains additional data to be authenticated during the encryption and decryption processes that use the key. This value is logged by AWS CloudTrail to provide context around the data encrypted by the key.
gdkEncryptionContext :: Lens' GenerateDataKey (HashMap Text Text)
gdkEncryptionContext = lens _gdkEncryptionContext (\ s a -> s{_gdkEncryptionContext = a}) . _Default . _Map;

-- | Integer that contains the number of bytes to generate. Common values are 128, 256, 512, and 1024. 1024 is the current limit. We recommend that you use the @KeySpec@ parameter instead.
gdkNumberOfBytes :: Lens' GenerateDataKey (Maybe Natural)
gdkNumberOfBytes = lens _gdkNumberOfBytes (\ s a -> s{_gdkNumberOfBytes = a}) . mapping _Nat;

-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
gdkGrantTokens :: Lens' GenerateDataKey [Text]
gdkGrantTokens = lens _gdkGrantTokens (\ s a -> s{_gdkGrantTokens = a}) . _Default . _Coerce;

-- | A unique identifier for the customer master key. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Alias ARN Example - arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012     * Alias Name Example - alias/MyAliasName
gdkKeyId :: Lens' GenerateDataKey Text
gdkKeyId = lens _gdkKeyId (\ s a -> s{_gdkKeyId = a});

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

instance Hashable GenerateDataKey

instance NFData GenerateDataKey

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateDataKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkrsResponseStatus' - -- | The response status code.
--
-- * 'gdkrsKeyId' - System generated unique identifier of the key to be used to decrypt the encrypted copy of the data key.
--
-- * 'gdkrsPlaintext' - Plaintext that contains the data key. Use this for encryption and decryption and then remove it from memory as soon as possible.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkrsCiphertextBlob' - Ciphertext that contains the encrypted data key. You must store the blob and enough information to reconstruct the encryption context so that the data encrypted by using the key can later be decrypted. You must provide both the ciphertext blob and the encryption context to the 'Decrypt' API to recover the plaintext data key and decrypt the object. If you are using the CLI, the value is Base64 encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
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
gdkrsResponseStatus = lens _gdkrsResponseStatus (\ s a -> s{_gdkrsResponseStatus = a});

-- | System generated unique identifier of the key to be used to decrypt the encrypted copy of the data key.
gdkrsKeyId :: Lens' GenerateDataKeyResponse Text
gdkrsKeyId = lens _gdkrsKeyId (\ s a -> s{_gdkrsKeyId = a});

-- | Plaintext that contains the data key. Use this for encryption and decryption and then remove it from memory as soon as possible.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkrsPlaintext :: Lens' GenerateDataKeyResponse ByteString
gdkrsPlaintext = lens _gdkrsPlaintext (\ s a -> s{_gdkrsPlaintext = a}) . _Sensitive . _Base64;

-- | Ciphertext that contains the encrypted data key. You must store the blob and enough information to reconstruct the encryption context so that the data encrypted by using the key can later be decrypted. You must provide both the ciphertext blob and the encryption context to the 'Decrypt' API to recover the plaintext data key and decrypt the object. If you are using the CLI, the value is Base64 encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkrsCiphertextBlob :: Lens' GenerateDataKeyResponse ByteString
gdkrsCiphertextBlob = lens _gdkrsCiphertextBlob (\ s a -> s{_gdkrsCiphertextBlob = a}) . _Base64;

instance NFData GenerateDataKeyResponse
