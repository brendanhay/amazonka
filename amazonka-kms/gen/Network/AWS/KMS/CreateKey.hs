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
    , ckrqKeyUsage
    , ckrqPolicy
    , ckrqDescription

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
-- * 'ckrqKeyUsage'
--
-- * 'ckrqPolicy'
--
-- * 'ckrqDescription'
data CreateKey = CreateKey'
    { _ckrqKeyUsage    :: !(Maybe KeyUsageType)
    , _ckrqPolicy      :: !(Maybe Text)
    , _ckrqDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateKey' smart constructor.
createKey :: CreateKey
createKey =
    CreateKey'
    { _ckrqKeyUsage = Nothing
    , _ckrqPolicy = Nothing
    , _ckrqDescription = Nothing
    }

-- | Specifies the intended use of the key. Currently this defaults to
-- ENCRYPT\/DECRYPT, and only symmetric encryption and decryption are
-- supported.
ckrqKeyUsage :: Lens' CreateKey (Maybe KeyUsageType)
ckrqKeyUsage = lens _ckrqKeyUsage (\ s a -> s{_ckrqKeyUsage = a});

-- | Policy to be attached to the key. This is required and delegates back to
-- the account. The key is the root of trust.
ckrqPolicy :: Lens' CreateKey (Maybe Text)
ckrqPolicy = lens _ckrqPolicy (\ s a -> s{_ckrqPolicy = a});

-- | Description of the key. We recommend that you choose a description that
-- helps your customer decide whether the key is appropriate for a task.
ckrqDescription :: Lens' CreateKey (Maybe Text)
ckrqDescription = lens _ckrqDescription (\ s a -> s{_ckrqDescription = a});

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
              ["KeyUsage" .= _ckrqKeyUsage,
               "Policy" .= _ckrqPolicy,
               "Description" .= _ckrqDescription]

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
