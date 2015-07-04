{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation deletes the bandwidth rate limits of a gateway. You can
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
    , delGatewayARN
    , delBandwidthType

    -- * Response
    , DeleteBandwidthRateLimitResponse
    -- ** Response constructor
    , deleteBandwidthRateLimitResponse
    -- ** Response lenses
    , deleGatewayARN
    , deleStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteBandwidthRateLimit' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delGatewayARN'
--
-- * 'delBandwidthType'
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
    { _delGatewayARN    :: !Text
    , _delBandwidthType :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBandwidthRateLimit' smart constructor.
deleteBandwidthRateLimit :: Text -> Text -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN pBandwidthType =
    DeleteBandwidthRateLimit'
    { _delGatewayARN = pGatewayARN
    , _delBandwidthType = pBandwidthType
    }

-- | FIXME: Undocumented member.
delGatewayARN :: Lens' DeleteBandwidthRateLimit Text
delGatewayARN = lens _delGatewayARN (\ s a -> s{_delGatewayARN = a});

-- | FIXME: Undocumented member.
delBandwidthType :: Lens' DeleteBandwidthRateLimit Text
delBandwidthType = lens _delBandwidthType (\ s a -> s{_delBandwidthType = a});

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
              ["GatewayARN" .= _delGatewayARN,
               "BandwidthType" .= _delBandwidthType]

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
-- * 'deleGatewayARN'
--
-- * 'deleStatus'
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
    { _deleGatewayARN :: !(Maybe Text)
    , _deleStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBandwidthRateLimitResponse' smart constructor.
deleteBandwidthRateLimitResponse :: Int -> DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse pStatus =
    DeleteBandwidthRateLimitResponse'
    { _deleGatewayARN = Nothing
    , _deleStatus = pStatus
    }

-- | FIXME: Undocumented member.
deleGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
deleGatewayARN = lens _deleGatewayARN (\ s a -> s{_deleGatewayARN = a});

-- | FIXME: Undocumented member.
deleStatus :: Lens' DeleteBandwidthRateLimitResponse Int
deleStatus = lens _deleStatus (\ s a -> s{_deleStatus = a});
