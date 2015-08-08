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
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html AWS API Reference> for Encrypt.
module Network.AWS.KMS.Encrypt
    (
    -- * Creating a Request
      Encrypt
    , encrypt
    -- * Request Lenses
    , eEncryptionContext
    , eGrantTokens
    , eKeyId
    , ePlaintext

    -- * Destructuring the Response
    , EncryptResponse
    , encryptResponse
    -- * Response Lenses
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
-- * 'eEncryptionContext'
--
-- * 'eGrantTokens'
--
-- * 'eKeyId'
--
-- * 'ePlaintext'
data Encrypt = Encrypt'
    { _eEncryptionContext :: !(Maybe (Map Text Text))
    , _eGrantTokens       :: !(Maybe [Text])
    , _eKeyId             :: !Text
    , _ePlaintext         :: !(Sensitive Base64)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Encrypt' smart constructor.
encrypt :: Text -> ByteString -> Encrypt
encrypt pKeyId_ pPlaintext_ =
    Encrypt'
    { _eEncryptionContext = Nothing
    , _eGrantTokens = Nothing
    , _eKeyId = pKeyId_
    , _ePlaintext = _Sensitive . _Base64 # pPlaintext_
    }

-- | Name\/value pair that specifies the encryption context to be used for
-- authenticated encryption. If used here, the same value must be supplied
-- to the @Decrypt@ API or decryption will fail. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
eEncryptionContext :: Lens' Encrypt (HashMap Text Text)
eEncryptionContext = lens _eEncryptionContext (\ s a -> s{_eEncryptionContext = a}) . _Default . _Map;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
eGrantTokens :: Lens' Encrypt [Text]
eGrantTokens = lens _eGrantTokens (\ s a -> s{_eGrantTokens = a}) . _Default . _Coerce;

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
eKeyId :: Lens' Encrypt Text
eKeyId = lens _eKeyId (\ s a -> s{_eKeyId = a});

-- | Data to be encrypted.
ePlaintext :: Lens' Encrypt ByteString
ePlaintext = lens _ePlaintext (\ s a -> s{_ePlaintext = a}) . _Sensitive . _Base64;

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
              ["EncryptionContext" .= _eEncryptionContext,
               "GrantTokens" .= _eGrantTokens, "KeyId" .= _eKeyId,
               "Plaintext" .= _ePlaintext]

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
encryptResponse pStatus_ =
    EncryptResponse'
    { _ersKeyId = Nothing
    , _ersCiphertextBlob = Nothing
    , _ersStatus = pStatus_
    }

-- | The ID of the key used during encryption.
ersKeyId :: Lens' EncryptResponse (Maybe Text)
ersKeyId = lens _ersKeyId (\ s a -> s{_ersKeyId = a});

-- | The encrypted plaintext. If you are using the CLI, the value is Base64
-- encoded. Otherwise, it is not encoded.
ersCiphertextBlob :: Lens' EncryptResponse (Maybe ByteString)
ersCiphertextBlob = lens _ersCiphertextBlob (\ s a -> s{_ersCiphertextBlob = a}) . mapping _Base64;

-- | Undocumented member.
ersStatus :: Lens' EncryptResponse Int
ersStatus = lens _ersStatus (\ s a -> s{_ersStatus = a});
