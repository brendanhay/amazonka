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
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a Boolean value that indicates whether key rotation is enabled
-- for the specified key.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyRotationStatus.html AWS API Reference> for GetKeyRotationStatus.
module Network.AWS.KMS.GetKeyRotationStatus
    (
    -- * Creating a Request
      GetKeyRotationStatus
    , getKeyRotationStatus
    -- * Request Lenses
    , gkrsKeyId

    -- * Destructuring the Response
    , GetKeyRotationStatusResponse
    , getKeyRotationStatusResponse
    -- * Response Lenses
    , gkrsrsKeyRotationEnabled
    , gkrsrsStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyRotationStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkrsKeyId'
newtype GetKeyRotationStatus = GetKeyRotationStatus'
    { _gkrsKeyId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetKeyRotationStatus' smart constructor.
getKeyRotationStatus :: Text -> GetKeyRotationStatus
getKeyRotationStatus pKeyId_ = 
    GetKeyRotationStatus'
    { _gkrsKeyId = pKeyId_
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
gkrsKeyId :: Lens' GetKeyRotationStatus Text
gkrsKeyId = lens _gkrsKeyId (\ s a -> s{_gkrsKeyId = a});

instance AWSRequest GetKeyRotationStatus where
        type Sv GetKeyRotationStatus = KMS
        type Rs GetKeyRotationStatus =
             GetKeyRotationStatusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetKeyRotationStatusResponse' <$>
                   (x .?> "KeyRotationEnabled") <*> (pure (fromEnum s)))

instance ToHeaders GetKeyRotationStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GetKeyRotationStatus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetKeyRotationStatus where
        toJSON GetKeyRotationStatus'{..}
          = object ["KeyId" .= _gkrsKeyId]

instance ToPath GetKeyRotationStatus where
        toPath = const "/"

instance ToQuery GetKeyRotationStatus where
        toQuery = const mempty

-- | /See:/ 'getKeyRotationStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkrsrsKeyRotationEnabled'
--
-- * 'gkrsrsStatus'
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
    { _gkrsrsKeyRotationEnabled :: !(Maybe Bool)
    , _gkrsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetKeyRotationStatusResponse' smart constructor.
getKeyRotationStatusResponse :: Int -> GetKeyRotationStatusResponse
getKeyRotationStatusResponse pStatus_ = 
    GetKeyRotationStatusResponse'
    { _gkrsrsKeyRotationEnabled = Nothing
    , _gkrsrsStatus = pStatus_
    }

-- | A Boolean value that specifies whether key rotation is enabled.
gkrsrsKeyRotationEnabled :: Lens' GetKeyRotationStatusResponse (Maybe Bool)
gkrsrsKeyRotationEnabled = lens _gkrsrsKeyRotationEnabled (\ s a -> s{_gkrsrsKeyRotationEnabled = a});

-- | Undocumented member.
gkrsrsStatus :: Lens' GetKeyRotationStatusResponse Int
gkrsrsStatus = lens _gkrsrsStatus (\ s a -> s{_gkrsrsStatus = a});
