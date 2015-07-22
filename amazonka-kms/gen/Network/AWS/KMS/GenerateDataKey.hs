{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generates a data key that you can use in your application to locally
-- encrypt data. This call returns a plaintext version of the key in the
-- @Plaintext@ field of the response object and an encrypted copy of the
-- key in the @CiphertextBlob@ field. The key is encrypted by using the
-- master key specified by the @KeyId@ field. To decrypt the encrypted key,
-- pass it to the @Decrypt@ API.
--
-- We recommend that you use the following pattern to locally encrypt data:
-- call the @GenerateDataKey@ API, use the key returned in the @Plaintext@
-- response field to locally encrypt data, and then erase the plaintext
-- data key from memory. Store the encrypted data key (contained in the
-- @CiphertextBlob@ field) alongside of the locally encrypted data.
--
-- You should not call the @Encrypt@ function to re-encrypt your data keys
-- within a region. @GenerateDataKey@ always returns the data key encrypted
-- and tied to the customer master key that will be used to decrypt it.
-- There is no need to decrypt it twice.
--
-- If you decide to use the optional @EncryptionContext@ parameter, you
-- must also store the context in full or at least store enough information
-- along with the encrypted data to be able to reconstruct the context when
-- submitting the ciphertext to the @Decrypt@ API. It is a good practice to
-- choose a context that you can reconstruct on the fly to better secure
-- the ciphertext. For more information about how this parameter is used,
-- see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
--
-- To decrypt data, pass the encrypted data key to the @Decrypt@ API.
-- @Decrypt@ uses the associated master key to decrypt the encrypted data
-- key and returns it as plaintext. Use the plaintext data key to locally
-- decrypt your data and then erase the key from memory. You must specify
-- the encryption context, if any, that you specified when you generated
-- the key. The encryption context is logged by CloudTrail, and you can use
-- this log to help track the use of particular data.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey.html>
module Network.AWS.KMS.GenerateDataKey
    (
    -- * Request
      GenerateDataKey
    -- ** Request constructor
    , generateDataKey
    -- ** Request lenses
    , gdkrqKeySpec
    , gdkrqEncryptionContext
    , gdkrqNumberOfBytes
    , gdkrqGrantTokens
    , gdkrqKeyId

    -- * Response
    , GenerateDataKeyResponse
    -- ** Response constructor
    , generateDataKeyResponse
    -- ** Response lenses
    , gdkrsKeyId
    , gdkrsPlaintext
    , gdkrsCiphertextBlob
    , gdkrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateDataKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkrqKeySpec'
--
-- * 'gdkrqEncryptionContext'
--
-- * 'gdkrqNumberOfBytes'
--
-- * 'gdkrqGrantTokens'
--
-- * 'gdkrqKeyId'
data GenerateDataKey = GenerateDataKey'
    { _gdkrqKeySpec           :: !(Maybe DataKeySpec)
    , _gdkrqEncryptionContext :: !(Maybe (Map Text Text))
    , _gdkrqNumberOfBytes     :: !(Maybe Nat)
    , _gdkrqGrantTokens       :: !(Maybe [Text])
    , _gdkrqKeyId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GenerateDataKey' smart constructor.
generateDataKey :: Text -> GenerateDataKey
generateDataKey pKeyId =
    GenerateDataKey'
    { _gdkrqKeySpec = Nothing
    , _gdkrqEncryptionContext = Nothing
    , _gdkrqNumberOfBytes = Nothing
    , _gdkrqGrantTokens = Nothing
    , _gdkrqKeyId = pKeyId
    }

-- | Value that identifies the encryption algorithm and key size to generate
-- a data key for. Currently this can be AES_128 or AES_256.
gdkrqKeySpec :: Lens' GenerateDataKey (Maybe DataKeySpec)
gdkrqKeySpec = lens _gdkrqKeySpec (\ s a -> s{_gdkrqKeySpec = a});

-- | Name\/value pair that contains additional data to be authenticated
-- during the encryption and decryption processes that use the key. This
-- value is logged by AWS CloudTrail to provide context around the data
-- encrypted by the key.
gdkrqEncryptionContext :: Lens' GenerateDataKey (HashMap Text Text)
gdkrqEncryptionContext = lens _gdkrqEncryptionContext (\ s a -> s{_gdkrqEncryptionContext = a}) . _Default . _Map;

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, and 1024. 1024 is the current limit. We recommend that
-- you use the @KeySpec@ parameter instead.
gdkrqNumberOfBytes :: Lens' GenerateDataKey (Maybe Natural)
gdkrqNumberOfBytes = lens _gdkrqNumberOfBytes (\ s a -> s{_gdkrqNumberOfBytes = a}) . mapping _Nat;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
gdkrqGrantTokens :: Lens' GenerateDataKey [Text]
gdkrqGrantTokens = lens _gdkrqGrantTokens (\ s a -> s{_gdkrqGrantTokens = a}) . _Default;

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier, a fully specified ARN to either an alias or
-- a key, or an alias name prefixed by \"alias\/\".
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Alias ARN Example -
--     arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
-- -   Alias Name Example - alias\/MyAliasName
gdkrqKeyId :: Lens' GenerateDataKey Text
gdkrqKeyId = lens _gdkrqKeyId (\ s a -> s{_gdkrqKeyId = a});

instance AWSRequest GenerateDataKey where
        type Sv GenerateDataKey = KMS
        type Rs GenerateDataKey = GenerateDataKeyResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GenerateDataKeyResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "Plaintext") <*>
                     (x .?> "CiphertextBlob")
                     <*> (pure (fromEnum s)))

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
              ["KeySpec" .= _gdkrqKeySpec,
               "EncryptionContext" .= _gdkrqEncryptionContext,
               "NumberOfBytes" .= _gdkrqNumberOfBytes,
               "GrantTokens" .= _gdkrqGrantTokens,
               "KeyId" .= _gdkrqKeyId]

instance ToPath GenerateDataKey where
        toPath = const "/"

instance ToQuery GenerateDataKey where
        toQuery = const mempty

-- | /See:/ 'generateDataKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkrsKeyId'
--
-- * 'gdkrsPlaintext'
--
-- * 'gdkrsCiphertextBlob'
--
-- * 'gdkrsStatus'
data GenerateDataKeyResponse = GenerateDataKeyResponse'
    { _gdkrsKeyId          :: !(Maybe Text)
    , _gdkrsPlaintext      :: !(Maybe (Sensitive Base64))
    , _gdkrsCiphertextBlob :: !(Maybe Base64)
    , _gdkrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GenerateDataKeyResponse' smart constructor.
generateDataKeyResponse :: Int -> GenerateDataKeyResponse
generateDataKeyResponse pStatus =
    GenerateDataKeyResponse'
    { _gdkrsKeyId = Nothing
    , _gdkrsPlaintext = Nothing
    , _gdkrsCiphertextBlob = Nothing
    , _gdkrsStatus = pStatus
    }

-- | System generated unique identifier of the key to be used to decrypt the
-- encrypted copy of the data key.
gdkrsKeyId :: Lens' GenerateDataKeyResponse (Maybe Text)
gdkrsKeyId = lens _gdkrsKeyId (\ s a -> s{_gdkrsKeyId = a});

-- | Plaintext that contains the data key. Use this for encryption and
-- decryption and then remove it from memory as soon as possible.
gdkrsPlaintext :: Lens' GenerateDataKeyResponse (Maybe Base64)
gdkrsPlaintext = lens _gdkrsPlaintext (\ s a -> s{_gdkrsPlaintext = a}) . mapping _Sensitive;

-- | Ciphertext that contains the encrypted data key. You must store the blob
-- and enough information to reconstruct the encryption context so that the
-- data encrypted by using the key can later be decrypted. You must provide
-- both the ciphertext blob and the encryption context to the Decrypt API
-- to recover the plaintext data key and decrypt the object.
--
-- If you are using the CLI, the value is Base64 encoded. Otherwise, it is
-- not encoded.
gdkrsCiphertextBlob :: Lens' GenerateDataKeyResponse (Maybe Base64)
gdkrsCiphertextBlob = lens _gdkrsCiphertextBlob (\ s a -> s{_gdkrsCiphertextBlob = a});

-- | FIXME: Undocumented member.
gdkrsStatus :: Lens' GenerateDataKeyResponse Int
gdkrsStatus = lens _gdkrsStatus (\ s a -> s{_gdkrsStatus = a});
