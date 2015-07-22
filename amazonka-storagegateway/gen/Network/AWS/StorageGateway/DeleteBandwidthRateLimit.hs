{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the bandwidth rate limits of a gateway. You can
-- delete either the upload and download bandwidth rate limit, or you can
-- delete both. If you delete only one of the limits, the other limit
-- remains unchanged. To specify which gateway to work with, use the Amazon
-- Resource Name (ARN) of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteBandwidthRateLimit.html>
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    (
    -- * Request
      DeleteBandwidthRateLimit
    -- ** Request constructor
    , deleteBandwidthRateLimit
    -- ** Request lenses
    , delrqGatewayARN
    , delrqBandwidthType

    -- * Response
    , DeleteBandwidthRateLimitResponse
    -- ** Response constructor
    , deleteBandwidthRateLimitResponse
    -- ** Response lenses
    , delrsGatewayARN
    , delrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteBandwidthRateLimit' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrqGatewayARN'
--
-- * 'delrqBandwidthType'
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
    { _delrqGatewayARN    :: !Text
    , _delrqBandwidthType :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBandwidthRateLimit' smart constructor.
deleteBandwidthRateLimit :: Text -> Text -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN pBandwidthType =
    DeleteBandwidthRateLimit'
    { _delrqGatewayARN = pGatewayARN
    , _delrqBandwidthType = pBandwidthType
    }

-- | FIXME: Undocumented member.
delrqGatewayARN :: Lens' DeleteBandwidthRateLimit Text
delrqGatewayARN = lens _delrqGatewayARN (\ s a -> s{_delrqGatewayARN = a});

-- | FIXME: Undocumented member.
delrqBandwidthType :: Lens' DeleteBandwidthRateLimit Text
delrqBandwidthType = lens _delrqBandwidthType (\ s a -> s{_delrqBandwidthType = a});

instance AWSRequest DeleteBandwidthRateLimit where
        type Sv DeleteBandwidthRateLimit = StorageGateway
        type Rs DeleteBandwidthRateLimit =
             DeleteBandwidthRateLimitResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBandwidthRateLimitResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders DeleteBandwidthRateLimit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteBandwidthRateLimit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteBandwidthRateLimit where
        toJSON DeleteBandwidthRateLimit'{..}
          = object
              ["GatewayARN" .= _delrqGatewayARN,
               "BandwidthType" .= _delrqBandwidthType]

instance ToPath DeleteBandwidthRateLimit where
        toPath = const "/"

instance ToQuery DeleteBandwidthRateLimit where
        toQuery = const mempty

-- | A JSON object containing the of the gateway whose bandwidth rate
-- information was deleted.
--
-- /See:/ 'deleteBandwidthRateLimitResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrsGatewayARN'
--
-- * 'delrsStatus'
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
    { _delrsGatewayARN :: !(Maybe Text)
    , _delrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBandwidthRateLimitResponse' smart constructor.
deleteBandwidthRateLimitResponse :: Int -> DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse pStatus =
    DeleteBandwidthRateLimitResponse'
    { _delrsGatewayARN = Nothing
    , _delrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
delrsGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
delrsGatewayARN = lens _delrsGatewayARN (\ s a -> s{_delrsGatewayARN = a});

-- | FIXME: Undocumented member.
delrsStatus :: Lens' DeleteBandwidthRateLimitResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
