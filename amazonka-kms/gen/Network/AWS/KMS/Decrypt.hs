{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.Decrypt
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

-- | Decrypts ciphertext. Ciphertext is plaintext that has been previously
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
    , decEncryptionContext
    , decGrantTokens
    , decCiphertextBlob

    -- * Response
    , DecryptResponse
    -- ** Response constructor
    , decryptResponse
    -- ** Response lenses
    , drKeyId
    , drPlaintext
    , drStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'decrypt' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decEncryptionContext'
--
-- * 'decGrantTokens'
--
-- * 'decCiphertextBlob'
data Decrypt = Decrypt'
    { _decEncryptionContext :: !(Maybe (Map Text Text))
    , _decGrantTokens       :: !(Maybe [Text])
    , _decCiphertextBlob    :: !Base64
    } deriving (Eq,Read,Show)

-- | 'Decrypt' smart constructor.
decrypt :: Base64 -> Decrypt
decrypt pCiphertextBlob =
    Decrypt'
    { _decEncryptionContext = Nothing
    , _decGrantTokens = Nothing
    , _decCiphertextBlob = pCiphertextBlob
    }

-- | The encryption context. If this was specified in the Encrypt function,
-- it must be specified here or the decryption operation will fail. For
-- more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
decEncryptionContext :: Lens' Decrypt (HashMap Text Text)
decEncryptionContext = lens _decEncryptionContext (\ s a -> s{_decEncryptionContext = a}) . _Default . _Map;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
decGrantTokens :: Lens' Decrypt [Text]
decGrantTokens = lens _decGrantTokens (\ s a -> s{_decGrantTokens = a}) . _Default;

-- | Ciphertext to be decrypted. The blob includes metadata.
decCiphertextBlob :: Lens' Decrypt Base64
decCiphertextBlob = lens _decCiphertextBlob (\ s a -> s{_decCiphertextBlob = a});

instance AWSRequest Decrypt where
        type Sv Decrypt = KMS
        type Rs Decrypt = DecryptResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DecryptResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "Plaintext") <*> (pure s))

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
              ["EncryptionContext" .= _decEncryptionContext,
               "GrantTokens" .= _decGrantTokens,
               "CiphertextBlob" .= _decCiphertextBlob]

instance ToPath Decrypt where
        toPath = const "/"

instance ToQuery Decrypt where
        toQuery = const mempty

-- | /See:/ 'decryptResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drKeyId'
--
-- * 'drPlaintext'
--
-- * 'drStatus'
data DecryptResponse = DecryptResponse'
    { _drKeyId     :: !(Maybe Text)
    , _drPlaintext :: !(Maybe (Sensitive Base64))
    , _drStatus    :: !Status
    } deriving (Eq,Show)

-- | 'DecryptResponse' smart constructor.
decryptResponse :: Status -> DecryptResponse
decryptResponse pStatus =
    DecryptResponse'
    { _drKeyId = Nothing
    , _drPlaintext = Nothing
    , _drStatus = pStatus
    }

-- | ARN of the key used to perform the decryption. This value is returned if
-- no errors are encountered during the operation.
drKeyId :: Lens' DecryptResponse (Maybe Text)
drKeyId = lens _drKeyId (\ s a -> s{_drKeyId = a});

-- | Decrypted plaintext data. This value may not be returned if the customer
-- master key is not available or if you didn\'t have permission to use it.
drPlaintext :: Lens' DecryptResponse (Maybe Base64)
drPlaintext = lens _drPlaintext (\ s a -> s{_drPlaintext = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
drStatus :: Lens' DecryptResponse Status
drStatus = lens _drStatus (\ s a -> s{_drStatus = a});
