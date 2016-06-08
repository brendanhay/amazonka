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
-- Module      : Network.AWS.KMS.Decrypt
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext. Ciphertext is plaintext that has been previously encrypted by using any of the following functions:
--
-- -   < GenerateDataKey>
-- -   < GenerateDataKeyWithoutPlaintext>
-- -   < Encrypt>
--
-- Note that if a caller has been granted access permissions to all keys (through, for example, IAM user policies that grant 'Decrypt' permission on all resources), then ciphertext encrypted by using keys in other accounts where the key grants access to the caller can be decrypted. To remedy this, we recommend that you do not grant 'Decrypt' access in an IAM user policy. Instead grant 'Decrypt' access only in key policies. If you must grant 'Decrypt' access in an IAM user policy, you should scope the resource to specific keys or to specific trusted accounts.
module Network.AWS.KMS.Decrypt
    (
    -- * Creating a Request
      decrypt
    , Decrypt
    -- * Request Lenses
    , decEncryptionContext
    , decGrantTokens
    , decCiphertextBlob

    -- * Destructuring the Response
    , decryptResponse
    , DecryptResponse
    -- * Response Lenses
    , drsKeyId
    , drsPlaintext
    , drsResponseStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'decrypt' smart constructor.
data Decrypt = Decrypt'
    { _decEncryptionContext :: !(Maybe (Map Text Text))
    , _decGrantTokens       :: !(Maybe [Text])
    , _decCiphertextBlob    :: !Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Decrypt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decEncryptionContext'
--
-- * 'decGrantTokens'
--
-- * 'decCiphertextBlob'
decrypt
    :: ByteString -- ^ 'decCiphertextBlob'
    -> Decrypt
decrypt pCiphertextBlob_ =
    Decrypt'
    { _decEncryptionContext = Nothing
    , _decGrantTokens = Nothing
    , _decCiphertextBlob = _Base64 # pCiphertextBlob_
    }

-- | The encryption context. If this was specified in the < Encrypt> function, it must be specified here or the decryption operation will fail. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
decEncryptionContext :: Lens' Decrypt (HashMap Text Text)
decEncryptionContext = lens _decEncryptionContext (\ s a -> s{_decEncryptionContext = a}) . _Default . _Map;

-- | A list of grant tokens.
--
-- For more information, go to <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/.
decGrantTokens :: Lens' Decrypt [Text]
decGrantTokens = lens _decGrantTokens (\ s a -> s{_decGrantTokens = a}) . _Default . _Coerce;

-- | Ciphertext to be decrypted. The blob includes metadata.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
decCiphertextBlob :: Lens' Decrypt ByteString
decCiphertextBlob = lens _decCiphertextBlob (\ s a -> s{_decCiphertextBlob = a}) . _Base64;

instance AWSRequest Decrypt where
        type Rs Decrypt = DecryptResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 DecryptResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "Plaintext") <*>
                     (pure (fromEnum s)))

instance Hashable Decrypt

instance NFData Decrypt

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
              (catMaybes
                 [("EncryptionContext" .=) <$> _decEncryptionContext,
                  ("GrantTokens" .=) <$> _decGrantTokens,
                  Just ("CiphertextBlob" .= _decCiphertextBlob)])

instance ToPath Decrypt where
        toPath = const "/"

instance ToQuery Decrypt where
        toQuery = const mempty

-- | /See:/ 'decryptResponse' smart constructor.
data DecryptResponse = DecryptResponse'
    { _drsKeyId          :: !(Maybe Text)
    , _drsPlaintext      :: !(Maybe (Sensitive Base64))
    , _drsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DecryptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsKeyId'
--
-- * 'drsPlaintext'
--
-- * 'drsResponseStatus'
decryptResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DecryptResponse
decryptResponse pResponseStatus_ =
    DecryptResponse'
    { _drsKeyId = Nothing
    , _drsPlaintext = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | ARN of the key used to perform the decryption. This value is returned if no errors are encountered during the operation.
drsKeyId :: Lens' DecryptResponse (Maybe Text)
drsKeyId = lens _drsKeyId (\ s a -> s{_drsKeyId = a});

-- | Decrypted plaintext data. This value may not be returned if the customer master key is not available or if you didn\'t have permission to use it.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
drsPlaintext :: Lens' DecryptResponse (Maybe ByteString)
drsPlaintext = lens _drsPlaintext (\ s a -> s{_drsPlaintext = a}) . mapping (_Sensitive . _Base64);

-- | The response status code.
drsResponseStatus :: Lens' DecryptResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

instance NFData DecryptResponse
