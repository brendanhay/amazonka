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
    , deleteGatewayARN
    , deleteBandwidthType

    -- * Response
    , DeleteBandwidthRateLimitResponse
    -- ** Response constructor
    , deleteBandwidthRateLimitResponse
    -- ** Response lenses
    , delGatewayARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteBandwidthRateLimit' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deleteGatewayARN'
--
-- * 'deleteBandwidthType'
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'{_deleteGatewayARN :: Text, _deleteBandwidthType :: Text} deriving (Eq, Read, Show)

-- | 'DeleteBandwidthRateLimit' smart constructor.
deleteBandwidthRateLimit :: Text -> Text -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN pBandwidthType = DeleteBandwidthRateLimit'{_deleteGatewayARN = pGatewayARN, _deleteBandwidthType = pBandwidthType};

-- | FIXME: Undocumented member.
deleteGatewayARN :: Lens' DeleteBandwidthRateLimit Text
deleteGatewayARN = lens _deleteGatewayARN (\ s a -> s{_deleteGatewayARN = a});

-- | FIXME: Undocumented member.
deleteBandwidthType :: Lens' DeleteBandwidthRateLimit Text
deleteBandwidthType = lens _deleteBandwidthType (\ s a -> s{_deleteBandwidthType = a});

instance AWSRequest DeleteBandwidthRateLimit where
        type Sv DeleteBandwidthRateLimit = StorageGateway
        type Rs DeleteBandwidthRateLimit =
             DeleteBandwidthRateLimitResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBandwidthRateLimitResponse' <$>
                   x .:> "GatewayARN")

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
              ["GatewayARN" .= _deleteGatewayARN,
               "BandwidthType" .= _deleteBandwidthType]

instance ToPath DeleteBandwidthRateLimit where
        toPath = const "/"

instance ToQuery DeleteBandwidthRateLimit where
        toQuery = const mempty

-- | /See:/ 'deleteBandwidthRateLimitResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delGatewayARN'
newtype DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'{_delGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'DeleteBandwidthRateLimitResponse' smart constructor.
deleteBandwidthRateLimitResponse :: Text -> DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse pGatewayARN = DeleteBandwidthRateLimitResponse'{_delGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
delGatewayARN :: Lens' DeleteBandwidthRateLimitResponse Text
delGatewayARN = lens _delGatewayARN (\ s a -> s{_delGatewayARN = a});
