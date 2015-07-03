{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Generates a data key that you can use in your application to locally
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
    , gdkKeySpec
    , gdkEncryptionContext
    , gdkNumberOfBytes
    , gdkGrantTokens
    , gdkKeyId

    -- * Response
    , GenerateDataKeyResponse
    -- ** Response constructor
    , generateDataKeyResponse
    -- ** Response lenses
    , gdkrKeyId
    , gdkrPlaintext
    , gdkrCiphertextBlob
    , gdkrStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateDataKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkKeySpec'
--
-- * 'gdkEncryptionContext'
--
-- * 'gdkNumberOfBytes'
--
-- * 'gdkGrantTokens'
--
-- * 'gdkKeyId'
data GenerateDataKey = GenerateDataKey'
    { _gdkKeySpec           :: !(Maybe DataKeySpec)
    , _gdkEncryptionContext :: !(Maybe (Map Text Text))
    , _gdkNumberOfBytes     :: !(Maybe Nat)
    , _gdkGrantTokens       :: !(Maybe [Text])
    , _gdkKeyId             :: !Text
    } deriving (Eq,Read,Show)

-- | 'GenerateDataKey' smart constructor.
generateDataKey :: Text -> GenerateDataKey
generateDataKey pKeyId =
    GenerateDataKey'
    { _gdkKeySpec = Nothing
    , _gdkEncryptionContext = Nothing
    , _gdkNumberOfBytes = Nothing
    , _gdkGrantTokens = Nothing
    , _gdkKeyId = pKeyId
    }

-- | Value that identifies the encryption algorithm and key size to generate
-- a data key for. Currently this can be AES_128 or AES_256.
gdkKeySpec :: Lens' GenerateDataKey (Maybe DataKeySpec)
gdkKeySpec = lens _gdkKeySpec (\ s a -> s{_gdkKeySpec = a});

-- | Name\/value pair that contains additional data to be authenticated
-- during the encryption and decryption processes that use the key. This
-- value is logged by AWS CloudTrail to provide context around the data
-- encrypted by the key.
gdkEncryptionContext :: Lens' GenerateDataKey (HashMap Text Text)
gdkEncryptionContext = lens _gdkEncryptionContext (\ s a -> s{_gdkEncryptionContext = a}) . _Default . _Map;

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, and 1024. 1024 is the current limit. We recommend that
-- you use the @KeySpec@ parameter instead.
gdkNumberOfBytes :: Lens' GenerateDataKey (Maybe Natural)
gdkNumberOfBytes = lens _gdkNumberOfBytes (\ s a -> s{_gdkNumberOfBytes = a}) . mapping _Nat;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
gdkGrantTokens :: Lens' GenerateDataKey [Text]
gdkGrantTokens = lens _gdkGrantTokens (\ s a -> s{_gdkGrantTokens = a}) . _Default;

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
gdkKeyId :: Lens' GenerateDataKey Text
gdkKeyId = lens _gdkKeyId (\ s a -> s{_gdkKeyId = a});

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
              ["KeySpec" .= _gdkKeySpec,
               "EncryptionContext" .= _gdkEncryptionContext,
               "NumberOfBytes" .= _gdkNumberOfBytes,
               "GrantTokens" .= _gdkGrantTokens,
               "KeyId" .= _gdkKeyId]

instance ToPath GenerateDataKey where
        toPath = const "/"

instance ToQuery GenerateDataKey where
        toQuery = const mempty

-- | /See:/ 'generateDataKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkrKeyId'
--
-- * 'gdkrPlaintext'
--
-- * 'gdkrCiphertextBlob'
--
-- * 'gdkrStatus'
data GenerateDataKeyResponse = GenerateDataKeyResponse'
    { _gdkrKeyId          :: !(Maybe Text)
    , _gdkrPlaintext      :: !(Maybe (Sensitive Base64))
    , _gdkrCiphertextBlob :: !(Maybe Base64)
    , _gdkrStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'GenerateDataKeyResponse' smart constructor.
generateDataKeyResponse :: Int -> GenerateDataKeyResponse
generateDataKeyResponse pStatus =
    GenerateDataKeyResponse'
    { _gdkrKeyId = Nothing
    , _gdkrPlaintext = Nothing
    , _gdkrCiphertextBlob = Nothing
    , _gdkrStatus = pStatus
    }

-- | System generated unique identifier of the key to be used to decrypt the
-- encrypted copy of the data key.
gdkrKeyId :: Lens' GenerateDataKeyResponse (Maybe Text)
gdkrKeyId = lens _gdkrKeyId (\ s a -> s{_gdkrKeyId = a});

-- | Plaintext that contains the data key. Use this for encryption and
-- decryption and then remove it from memory as soon as possible.
gdkrPlaintext :: Lens' GenerateDataKeyResponse (Maybe Base64)
gdkrPlaintext = lens _gdkrPlaintext (\ s a -> s{_gdkrPlaintext = a}) . mapping _Sensitive;

-- | Ciphertext that contains the encrypted data key. You must store the blob
-- and enough information to reconstruct the encryption context so that the
-- data encrypted by using the key can later be decrypted. You must provide
-- both the ciphertext blob and the encryption context to the Decrypt API
-- to recover the plaintext data key and decrypt the object.
--
-- If you are using the CLI, the value is Base64 encoded. Otherwise, it is
-- not encoded.
gdkrCiphertextBlob :: Lens' GenerateDataKeyResponse (Maybe Base64)
gdkrCiphertextBlob = lens _gdkrCiphertextBlob (\ s a -> s{_gdkrCiphertextBlob = a});

-- | FIXME: Undocumented member.
gdkrStatus :: Lens' GenerateDataKeyResponse Int
gdkrStatus = lens _gdkrStatus (\ s a -> s{_gdkrStatus = a});
