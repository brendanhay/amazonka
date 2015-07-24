{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a customer master key. Customer master keys can be used to
-- encrypt small amounts of data (less than 4K) directly, but they are most
-- commonly used to encrypt or envelope data keys that are then used to
-- encrypt customer data. For more information about data keys, see
-- GenerateDataKey and GenerateDataKeyWithoutPlaintext.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html>
module Network.AWS.KMS.CreateKey
    (
    -- * Request
      CreateKey
    -- ** Request constructor
    , createKey
    -- ** Request lenses
    , ckKeyUsage
    , ckPolicy
    , ckDescription

    -- * Response
    , CreateKeyResponse
    -- ** Response constructor
    , createKeyResponse
    -- ** Response lenses
    , ckrsKeyMetadata
    , ckrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckKeyUsage'
--
-- * 'ckPolicy'
--
-- * 'ckDescription'
data CreateKey = CreateKey'
    { _ckKeyUsage    :: !(Maybe KeyUsageType)
    , _ckPolicy      :: !(Maybe Text)
    , _ckDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateKey' smart constructor.
createKey :: CreateKey
createKey =
    CreateKey'
    { _ckKeyUsage = Nothing
    , _ckPolicy = Nothing
    , _ckDescription = Nothing
    }

-- | Specifies the intended use of the key. Currently this defaults to
-- ENCRYPT\/DECRYPT, and only symmetric encryption and decryption are
-- supported.
ckKeyUsage :: Lens' CreateKey (Maybe KeyUsageType)
ckKeyUsage = lens _ckKeyUsage (\ s a -> s{_ckKeyUsage = a});

-- | Policy to be attached to the key. This is required and delegates back to
-- the account. The key is the root of trust.
ckPolicy :: Lens' CreateKey (Maybe Text)
ckPolicy = lens _ckPolicy (\ s a -> s{_ckPolicy = a});

-- | Description of the key. We recommend that you choose a description that
-- helps your customer decide whether the key is appropriate for a task.
ckDescription :: Lens' CreateKey (Maybe Text)
ckDescription = lens _ckDescription (\ s a -> s{_ckDescription = a});

instance AWSRequest CreateKey where
        type Sv CreateKey = KMS
        type Rs CreateKey = CreateKeyResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateKeyResponse' <$>
                   (x .?> "KeyMetadata") <*> (pure (fromEnum s)))

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
              ["KeyUsage" .= _ckKeyUsage, "Policy" .= _ckPolicy,
               "Description" .= _ckDescription]

instance ToPath CreateKey where
        toPath = const "/"

instance ToQuery CreateKey where
        toQuery = const mempty

-- | /See:/ 'createKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckrsKeyMetadata'
--
-- * 'ckrsStatus'
data CreateKeyResponse = CreateKeyResponse'
    { _ckrsKeyMetadata :: !(Maybe KeyMetadata)
    , _ckrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateKeyResponse' smart constructor.
createKeyResponse :: Int -> CreateKeyResponse
createKeyResponse pStatus_ =
    CreateKeyResponse'
    { _ckrsKeyMetadata = Nothing
    , _ckrsStatus = pStatus_
    }

-- | Metadata associated with the key.
ckrsKeyMetadata :: Lens' CreateKeyResponse (Maybe KeyMetadata)
ckrsKeyMetadata = lens _ckrsKeyMetadata (\ s a -> s{_ckrsKeyMetadata = a});

-- | FIXME: Undocumented member.
ckrsStatus :: Lens' CreateKeyResponse Int
ckrsStatus = lens _ckrsStatus (\ s a -> s{_ckrsStatus = a});
