{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Retrieves a Boolean value that indicates whether key rotation is enabled
-- for the specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyRotationStatus.html>
module Network.AWS.KMS.GetKeyRotationStatus
    (
    -- * Request
      GetKeyRotationStatus
    -- ** Request constructor
    , getKeyRotationStatus
    -- ** Request lenses
    , gkrsKeyId

    -- * Response
    , GetKeyRotationStatusResponse
    -- ** Response constructor
    , getKeyRotationStatusResponse
    -- ** Response lenses
    , gkrsrKeyRotationEnabled
    , gkrsrStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
getKeyRotationStatus pKeyId =
    GetKeyRotationStatus'
    { _gkrsKeyId = pKeyId
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
-- * 'gkrsrKeyRotationEnabled'
--
-- * 'gkrsrStatus'
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
    { _gkrsrKeyRotationEnabled :: !(Maybe Bool)
    , _gkrsrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetKeyRotationStatusResponse' smart constructor.
getKeyRotationStatusResponse :: Int -> GetKeyRotationStatusResponse
getKeyRotationStatusResponse pStatus =
    GetKeyRotationStatusResponse'
    { _gkrsrKeyRotationEnabled = Nothing
    , _gkrsrStatus = pStatus
    }

-- | A Boolean value that specifies whether key rotation is enabled.
gkrsrKeyRotationEnabled :: Lens' GetKeyRotationStatusResponse (Maybe Bool)
gkrsrKeyRotationEnabled = lens _gkrsrKeyRotationEnabled (\ s a -> s{_gkrsrKeyRotationEnabled = a});

-- | FIXME: Undocumented member.
gkrsrStatus :: Lens' GetKeyRotationStatusResponse Int
gkrsrStatus = lens _gkrsrStatus (\ s a -> s{_gkrsrStatus = a});
