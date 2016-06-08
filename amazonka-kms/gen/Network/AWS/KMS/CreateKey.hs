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
-- Module      : Network.AWS.KMS.CreateKey
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customer master key. Customer master keys can be used to encrypt small amounts of data (less than 4K) directly, but they are most commonly used to encrypt or envelope data keys that are then used to encrypt customer data. For more information about data keys, see < GenerateDataKey> and < GenerateDataKeyWithoutPlaintext>.
module Network.AWS.KMS.CreateKey
    (
    -- * Creating a Request
      createKey
    , CreateKey
    -- * Request Lenses
    , ckKeyUsage
    , ckPolicy
    , ckDescription

    -- * Destructuring the Response
    , createKeyResponse
    , CreateKeyResponse
    -- * Response Lenses
    , ckrsKeyMetadata
    , ckrsResponseStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createKey' smart constructor.
data CreateKey = CreateKey'
    { _ckKeyUsage    :: !(Maybe KeyUsageType)
    , _ckPolicy      :: !(Maybe Text)
    , _ckDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckKeyUsage'
--
-- * 'ckPolicy'
--
-- * 'ckDescription'
createKey
    :: CreateKey
createKey =
    CreateKey'
    { _ckKeyUsage = Nothing
    , _ckPolicy = Nothing
    , _ckDescription = Nothing
    }

-- | Specifies the intended use of the key. Currently this defaults to ENCRYPT\/DECRYPT, and only symmetric encryption and decryption are supported.
ckKeyUsage :: Lens' CreateKey (Maybe KeyUsageType)
ckKeyUsage = lens _ckKeyUsage (\ s a -> s{_ckKeyUsage = a});

-- | Policy to attach to the key. This is required and delegates back to the account. The key is the root of trust. The policy size limit is 32 KiB (32768 bytes).
ckPolicy :: Lens' CreateKey (Maybe Text)
ckPolicy = lens _ckPolicy (\ s a -> s{_ckPolicy = a});

-- | Description of the key. We recommend that you choose a description that helps your customer decide whether the key is appropriate for a task.
ckDescription :: Lens' CreateKey (Maybe Text)
ckDescription = lens _ckDescription (\ s a -> s{_ckDescription = a});

instance AWSRequest CreateKey where
        type Rs CreateKey = CreateKeyResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 CreateKeyResponse' <$>
                   (x .?> "KeyMetadata") <*> (pure (fromEnum s)))

instance Hashable CreateKey

instance NFData CreateKey

instance ToHeaders CreateKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.CreateKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateKey where
        toJSON CreateKey'{..}
          = object
              (catMaybes
                 [("KeyUsage" .=) <$> _ckKeyUsage,
                  ("Policy" .=) <$> _ckPolicy,
                  ("Description" .=) <$> _ckDescription])

instance ToPath CreateKey where
        toPath = const "/"

instance ToQuery CreateKey where
        toQuery = const mempty

-- | /See:/ 'createKeyResponse' smart constructor.
data CreateKeyResponse = CreateKeyResponse'
    { _ckrsKeyMetadata    :: !(Maybe KeyMetadata)
    , _ckrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckrsKeyMetadata'
--
-- * 'ckrsResponseStatus'
createKeyResponse
    :: Int -- ^ 'ckrsResponseStatus'
    -> CreateKeyResponse
createKeyResponse pResponseStatus_ =
    CreateKeyResponse'
    { _ckrsKeyMetadata = Nothing
    , _ckrsResponseStatus = pResponseStatus_
    }

-- | Metadata associated with the key.
ckrsKeyMetadata :: Lens' CreateKeyResponse (Maybe KeyMetadata)
ckrsKeyMetadata = lens _ckrsKeyMetadata (\ s a -> s{_ckrsKeyMetadata = a});

-- | The response status code.
ckrsResponseStatus :: Lens' CreateKeyResponse Int
ckrsResponseStatus = lens _ckrsResponseStatus (\ s a -> s{_ckrsResponseStatus = a});

instance NFData CreateKeyResponse
