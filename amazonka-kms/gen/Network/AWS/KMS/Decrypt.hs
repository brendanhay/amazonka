{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Decrypt
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext. Ciphertext is plaintext that has been previously
-- encrypted by using any of the following functions:
--
-- -   GenerateDataKey
-- -   GenerateDataKeyWithoutPlaintext
-- -   Encrypt
--
-- Note that if a caller has been granted access permissions to all keys
-- (through, for example, IAM user policies that grant @Decrypt@ permission
-- on all resources), then ciphertext encrypted by using keys in other
-- accounts where the key grants access to the caller can be decrypted. To
-- remedy this, we recommend that you do not grant @Decrypt@ access in an
-- IAM user policy. Instead grant @Decrypt@ access only in key policies. If
-- you must grant @Decrypt@ access in an IAM user policy, you should scope
-- the resource to specific keys or to specific trusted accounts.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_Decrypt.html>
module Network.AWS.KMS.Decrypt
    (
    -- * Request
      Decrypt
    -- ** Request constructor
    , decrypt
    -- ** Request lenses
    , drqEncryptionContext
    , drqGrantTokens
    , drqCiphertextBlob

    -- * Response
    , DecryptResponse
    -- ** Response constructor
    , decryptResponse
    -- ** Response lenses
    , drsKeyId
    , drsPlaintext
    , drsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'decrypt' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqEncryptionContext'
--
-- * 'drqGrantTokens'
--
-- * 'drqCiphertextBlob'
data Decrypt = Decrypt'
    { _drqEncryptionContext :: !(Maybe (Map Text Text))
    , _drqGrantTokens       :: !(Maybe [Text])
    , _drqCiphertextBlob    :: !Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Decrypt' smart constructor.
decrypt :: Base64 -> Decrypt
decrypt pCiphertextBlob_ =
    Decrypt'
    { _drqEncryptionContext = Nothing
    , _drqGrantTokens = Nothing
    , _drqCiphertextBlob = pCiphertextBlob_
    }

-- | The encryption context. If this was specified in the Encrypt function,
-- it must be specified here or the decryption operation will fail. For
-- more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
drqEncryptionContext :: Lens' Decrypt (HashMap Text Text)
drqEncryptionContext = lens _drqEncryptionContext (\ s a -> s{_drqEncryptionContext = a}) . _Default . _Map;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
drqGrantTokens :: Lens' Decrypt [Text]
drqGrantTokens = lens _drqGrantTokens (\ s a -> s{_drqGrantTokens = a}) . _Default;

-- | Ciphertext to be decrypted. The blob includes metadata.
drqCiphertextBlob :: Lens' Decrypt Base64
drqCiphertextBlob = lens _drqCiphertextBlob (\ s a -> s{_drqCiphertextBlob = a});

instance AWSRequest Decrypt where
        type Sv Decrypt = KMS
        type Rs Decrypt = DecryptResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DecryptResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "Plaintext") <*>
                     (pure (fromEnum s)))

instance ToHeaders Decrypt where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.Decrypt" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON Decrypt where
        toJSON Decrypt'{..}
          = object
              ["EncryptionContext" .= _drqEncryptionContext,
               "GrantTokens" .= _drqGrantTokens,
               "CiphertextBlob" .= _drqCiphertextBlob]

instance ToPath Decrypt where
        toPath = const "/"

instance ToQuery Decrypt where
        toQuery = const mempty

-- | /See:/ 'decryptResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsKeyId'
--
-- * 'drsPlaintext'
--
-- * 'drsStatus'
data DecryptResponse = DecryptResponse'
    { _drsKeyId     :: !(Maybe Text)
    , _drsPlaintext :: !(Maybe (Sensitive Base64))
    , _drsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DecryptResponse' smart constructor.
decryptResponse :: Int -> DecryptResponse
decryptResponse pStatus_ =
    DecryptResponse'
    { _drsKeyId = Nothing
    , _drsPlaintext = Nothing
    , _drsStatus = pStatus_
    }

-- | ARN of the key used to perform the decryption. This value is returned if
-- no errors are encountered during the operation.
drsKeyId :: Lens' DecryptResponse (Maybe Text)
drsKeyId = lens _drsKeyId (\ s a -> s{_drsKeyId = a});

-- | Decrypted plaintext data. This value may not be returned if the customer
-- master key is not available or if you didn\'t have permission to use it.
drsPlaintext :: Lens' DecryptResponse (Maybe Base64)
drsPlaintext = lens _drsPlaintext (\ s a -> s{_drsPlaintext = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
drsStatus :: Lens' DecryptResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
