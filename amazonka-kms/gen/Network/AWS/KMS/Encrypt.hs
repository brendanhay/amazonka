{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.Encrypt
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Encrypts plaintext into ciphertext by using a customer master key. The
-- @Encrypt@ function has two primary use cases:
--
-- -   You can encrypt up to 4 KB of arbitrary data such as an RSA key, a
--     database password, or other sensitive customer information.
-- -   If you are moving encrypted data from one region to another, you can
--     use this API to encrypt in the new region the plaintext data key
--     that was used to encrypt the data in the original region. This
--     provides you with an encrypted copy of the data key that can be
--     decrypted in the new region and used there to decrypt the encrypted
--     data.
--
-- Unless you are moving encrypted data from one region to another, you
-- don\'t use this function to encrypt a generated data key within a
-- region. You retrieve data keys already encrypted by calling the
-- GenerateDataKey or GenerateDataKeyWithoutPlaintext function. Data keys
-- don\'t need to be encrypted again by calling @Encrypt@.
--
-- If you want to encrypt data locally in your application, you can use the
-- @GenerateDataKey@ function to return a plaintext data encryption key and
-- a copy of the key encrypted under the customer master key (CMK) of your
-- choosing.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html>
module Network.AWS.KMS.Encrypt
    (
    -- * Request
      Encrypt
    -- ** Request constructor
    , encrypt
    -- ** Request lenses
    , encEncryptionContext
    , encGrantTokens
    , encKeyId
    , encPlaintext

    -- * Response
    , EncryptResponse
    -- ** Response constructor
    , encryptResponse
    -- ** Response lenses
    , erKeyId
    , erCiphertextBlob
    ) where

import Network.AWS.KMS.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'encrypt' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'encEncryptionContext'
--
-- * 'encGrantTokens'
--
-- * 'encKeyId'
--
-- * 'encPlaintext'
data Encrypt = Encrypt'{_encEncryptionContext :: Maybe (Map Text Text), _encGrantTokens :: Maybe [Text], _encKeyId :: Text, _encPlaintext :: Sensitive Base64} deriving (Eq, Read, Show)

-- | 'Encrypt' smart constructor.
encrypt :: Text -> Base64 -> Encrypt
encrypt pKeyId pPlaintext = Encrypt'{_encEncryptionContext = Nothing, _encGrantTokens = Nothing, _encKeyId = pKeyId, _encPlaintext = _Sensitive # pPlaintext};

-- | Name\/value pair that specifies the encryption context to be used for
-- authenticated encryption. If used here, the same value must be supplied
-- to the @Decrypt@ API or decryption will fail. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
encEncryptionContext :: Lens' Encrypt (HashMap Text Text)
encEncryptionContext = lens _encEncryptionContext (\ s a -> s{_encEncryptionContext = a}) . _Default . _Map;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
encGrantTokens :: Lens' Encrypt [Text]
encGrantTokens = lens _encGrantTokens (\ s a -> s{_encGrantTokens = a}) . _Default;

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
encKeyId :: Lens' Encrypt Text
encKeyId = lens _encKeyId (\ s a -> s{_encKeyId = a});

-- | Data to be encrypted.
encPlaintext :: Lens' Encrypt Base64
encPlaintext = lens _encPlaintext (\ s a -> s{_encPlaintext = a}) . _Sensitive;

instance AWSRequest Encrypt where
        type Sv Encrypt = KMS
        type Rs Encrypt = EncryptResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 EncryptResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "CiphertextBlob"))

instance ToHeaders Encrypt where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.Encrypt" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON Encrypt where
        toJSON Encrypt'{..}
          = object
              ["EncryptionContext" .= _encEncryptionContext,
               "GrantTokens" .= _encGrantTokens,
               "KeyId" .= _encKeyId, "Plaintext" .= _encPlaintext]

instance ToPath Encrypt where
        toPath = const "/"

instance ToQuery Encrypt where
        toQuery = const mempty

-- | /See:/ 'encryptResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erKeyId'
--
-- * 'erCiphertextBlob'
data EncryptResponse = EncryptResponse'{_erKeyId :: Maybe Text, _erCiphertextBlob :: Maybe Base64} deriving (Eq, Read, Show)

-- | 'EncryptResponse' smart constructor.
encryptResponse :: EncryptResponse
encryptResponse = EncryptResponse'{_erKeyId = Nothing, _erCiphertextBlob = Nothing};

-- | The ID of the key used during encryption.
erKeyId :: Lens' EncryptResponse (Maybe Text)
erKeyId = lens _erKeyId (\ s a -> s{_erKeyId = a});

-- | The encrypted plaintext. If you are using the CLI, the value is Base64
-- encoded. Otherwise, it is not encoded.
erCiphertextBlob :: Lens' EncryptResponse (Maybe Base64)
erCiphertextBlob = lens _erCiphertextBlob (\ s a -> s{_erCiphertextBlob = a});
