{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Encrypt
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Encrypts plaintext into ciphertext by using a customer master key. The
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
    , erqEncryptionContext
    , erqGrantTokens
    , erqKeyId
    , erqPlaintext

    -- * Response
    , EncryptResponse
    -- ** Response constructor
    , encryptResponse
    -- ** Response lenses
    , ersKeyId
    , ersCiphertextBlob
    , ersStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'encrypt' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erqEncryptionContext'
--
-- * 'erqGrantTokens'
--
-- * 'erqKeyId'
--
-- * 'erqPlaintext'
data Encrypt = Encrypt'
    { _erqEncryptionContext :: !(Maybe (Map Text Text))
    , _erqGrantTokens       :: !(Maybe [Text])
    , _erqKeyId             :: !Text
    , _erqPlaintext         :: !(Sensitive Base64)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Encrypt' smart constructor.
encrypt :: Text -> Base64 -> Encrypt
encrypt pKeyId pPlaintext =
    Encrypt'
    { _erqEncryptionContext = Nothing
    , _erqGrantTokens = Nothing
    , _erqKeyId = pKeyId
    , _erqPlaintext = _Sensitive # pPlaintext
    }

-- | Name\/value pair that specifies the encryption context to be used for
-- authenticated encryption. If used here, the same value must be supplied
-- to the @Decrypt@ API or decryption will fail. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
erqEncryptionContext :: Lens' Encrypt (HashMap Text Text)
erqEncryptionContext = lens _erqEncryptionContext (\ s a -> s{_erqEncryptionContext = a}) . _Default . _Map;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
erqGrantTokens :: Lens' Encrypt [Text]
erqGrantTokens = lens _erqGrantTokens (\ s a -> s{_erqGrantTokens = a}) . _Default;

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
erqKeyId :: Lens' Encrypt Text
erqKeyId = lens _erqKeyId (\ s a -> s{_erqKeyId = a});

-- | Data to be encrypted.
erqPlaintext :: Lens' Encrypt Base64
erqPlaintext = lens _erqPlaintext (\ s a -> s{_erqPlaintext = a}) . _Sensitive;

instance AWSRequest Encrypt where
        type Sv Encrypt = KMS
        type Rs Encrypt = EncryptResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 EncryptResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "CiphertextBlob") <*>
                     (pure (fromEnum s)))

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
              ["EncryptionContext" .= _erqEncryptionContext,
               "GrantTokens" .= _erqGrantTokens,
               "KeyId" .= _erqKeyId, "Plaintext" .= _erqPlaintext]

instance ToPath Encrypt where
        toPath = const "/"

instance ToQuery Encrypt where
        toQuery = const mempty

-- | /See:/ 'encryptResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ersKeyId'
--
-- * 'ersCiphertextBlob'
--
-- * 'ersStatus'
data EncryptResponse = EncryptResponse'
    { _ersKeyId          :: !(Maybe Text)
    , _ersCiphertextBlob :: !(Maybe Base64)
    , _ersStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EncryptResponse' smart constructor.
encryptResponse :: Int -> EncryptResponse
encryptResponse pStatus =
    EncryptResponse'
    { _ersKeyId = Nothing
    , _ersCiphertextBlob = Nothing
    , _ersStatus = pStatus
    }

-- | The ID of the key used during encryption.
ersKeyId :: Lens' EncryptResponse (Maybe Text)
ersKeyId = lens _ersKeyId (\ s a -> s{_ersKeyId = a});

-- | The encrypted plaintext. If you are using the CLI, the value is Base64
-- encoded. Otherwise, it is not encoded.
ersCiphertextBlob :: Lens' EncryptResponse (Maybe Base64)
ersCiphertextBlob = lens _ersCiphertextBlob (\ s a -> s{_ersCiphertextBlob = a});

-- | FIXME: Undocumented member.
ersStatus :: Lens' EncryptResponse Int
ersStatus = lens _ersStatus (\ s a -> s{_ersStatus = a});
