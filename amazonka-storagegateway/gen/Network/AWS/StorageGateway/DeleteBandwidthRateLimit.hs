{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
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
    , deleGatewayARN
    , deleBandwidthType

    -- * Response
    , DeleteBandwidthRateLimitResponse
    -- ** Response constructor
    , deleteBandwidthRateLimitResponse
    -- ** Response lenses
    , delGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteBandwidthRateLimit' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deleGatewayARN'
--
-- * 'deleBandwidthType'
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'{_deleGatewayARN :: Text, _deleBandwidthType :: Text} deriving (Eq, Read, Show)

-- | 'DeleteBandwidthRateLimit' smart constructor.
deleteBandwidthRateLimit :: Text -> Text -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN pBandwidthType = DeleteBandwidthRateLimit'{_deleGatewayARN = pGatewayARN, _deleBandwidthType = pBandwidthType};

-- | FIXME: Undocumented member.
deleGatewayARN :: Lens' DeleteBandwidthRateLimit Text
deleGatewayARN = lens _deleGatewayARN (\ s a -> s{_deleGatewayARN = a});

-- | FIXME: Undocumented member.
deleBandwidthType :: Lens' DeleteBandwidthRateLimit Text
deleBandwidthType = lens _deleBandwidthType (\ s a -> s{_deleBandwidthType = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteBandwidthRateLimit where
        type Sv DeleteBandwidthRateLimit = StorageGateway
        type Rs DeleteBandwidthRateLimit =
             DeleteBandwidthRateLimitResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBandwidthRateLimitResponse' <$>
                   (x .?> "GatewayARN"))

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
              ["GatewayARN" .= _deleGatewayARN,
               "BandwidthType" .= _deleBandwidthType]

instance ToPath DeleteBandwidthRateLimit where
        toPath = const "/"

instance ToQuery DeleteBandwidthRateLimit where
        toQuery = const mempty

-- | /See:/ 'deleteBandwidthRateLimitResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delGatewayARN'
newtype DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'{_delGatewayARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DeleteBandwidthRateLimitResponse' smart constructor.
deleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'{_delGatewayARN = Nothing};

-- | FIXME: Undocumented member.
delGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
delGatewayARN = lens _delGatewayARN (\ s a -> s{_delGatewayARN = a});
