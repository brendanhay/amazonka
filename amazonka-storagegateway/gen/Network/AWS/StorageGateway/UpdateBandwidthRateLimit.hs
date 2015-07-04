{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimit
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

-- | This operation updates the bandwidth rate limits of a gateway. You can
-- update both the upload and download bandwidth rate limit or specify only
-- one of the two. If you don\'t set a bandwidth rate limit, the existing
-- rate limit remains.
--
-- By default, a gateway\'s bandwidth rate limits are not set. If you
-- don\'t set any limit, the gateway does not have any limitations on its
-- bandwidth usage and could potentially use the maximum available
-- bandwidth.
--
-- To specify which gateway to update, use the Amazon Resource Name (ARN)
-- of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateBandwidthRateLimit.html>
module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
    (
    -- * Request
      UpdateBandwidthRateLimit
    -- ** Request constructor
    , updateBandwidthRateLimit
    -- ** Request lenses
    , ubrlAverageUploadRateLimitInBitsPerSec
    , ubrlAverageDownloadRateLimitInBitsPerSec
    , ubrlGatewayARN

    -- * Response
    , UpdateBandwidthRateLimitResponse
    -- ** Response constructor
    , updateBandwidthRateLimitResponse
    -- ** Response lenses
    , ubrlrGatewayARN
    , ubrlrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec
-- -   UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec
--
-- /See:/ 'updateBandwidthRateLimit' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubrlAverageUploadRateLimitInBitsPerSec'
--
-- * 'ubrlAverageDownloadRateLimitInBitsPerSec'
--
-- * 'ubrlGatewayARN'
data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit'
    { _ubrlAverageUploadRateLimitInBitsPerSec   :: !(Maybe Nat)
    , _ubrlAverageDownloadRateLimitInBitsPerSec :: !(Maybe Nat)
    , _ubrlGatewayARN                           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBandwidthRateLimit' smart constructor.
updateBandwidthRateLimit :: Text -> UpdateBandwidthRateLimit
updateBandwidthRateLimit pGatewayARN =
    UpdateBandwidthRateLimit'
    { _ubrlAverageUploadRateLimitInBitsPerSec = Nothing
    , _ubrlAverageDownloadRateLimitInBitsPerSec = Nothing
    , _ubrlGatewayARN = pGatewayARN
    }

-- | The average upload bandwidth rate limit in bits per second.
ubrlAverageUploadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlAverageUploadRateLimitInBitsPerSec = lens _ubrlAverageUploadRateLimitInBitsPerSec (\ s a -> s{_ubrlAverageUploadRateLimitInBitsPerSec = a}) . mapping _Nat;

-- | The average download bandwidth rate limit in bits per second.
ubrlAverageDownloadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlAverageDownloadRateLimitInBitsPerSec = lens _ubrlAverageDownloadRateLimitInBitsPerSec (\ s a -> s{_ubrlAverageDownloadRateLimitInBitsPerSec = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
ubrlGatewayARN :: Lens' UpdateBandwidthRateLimit Text
ubrlGatewayARN = lens _ubrlGatewayARN (\ s a -> s{_ubrlGatewayARN = a});

instance AWSRequest UpdateBandwidthRateLimit where
        type Sv UpdateBandwidthRateLimit = StorageGateway
        type Rs UpdateBandwidthRateLimit =
             UpdateBandwidthRateLimitResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBandwidthRateLimitResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders UpdateBandwidthRateLimit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateBandwidthRateLimit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBandwidthRateLimit where
        toJSON UpdateBandwidthRateLimit'{..}
          = object
              ["AverageUploadRateLimitInBitsPerSec" .=
                 _ubrlAverageUploadRateLimitInBitsPerSec,
               "AverageDownloadRateLimitInBitsPerSec" .=
                 _ubrlAverageDownloadRateLimitInBitsPerSec,
               "GatewayARN" .= _ubrlGatewayARN]

instance ToPath UpdateBandwidthRateLimit where
        toPath = const "/"

instance ToQuery UpdateBandwidthRateLimit where
        toQuery = const mempty

-- | A JSON object containing the of the gateway whose throttle information
-- was updated.
--
-- /See:/ 'updateBandwidthRateLimitResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubrlrGatewayARN'
--
-- * 'ubrlrStatus'
data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse'
    { _ubrlrGatewayARN :: !(Maybe Text)
    , _ubrlrStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBandwidthRateLimitResponse' smart constructor.
updateBandwidthRateLimitResponse :: Int -> UpdateBandwidthRateLimitResponse
updateBandwidthRateLimitResponse pStatus =
    UpdateBandwidthRateLimitResponse'
    { _ubrlrGatewayARN = Nothing
    , _ubrlrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ubrlrGatewayARN :: Lens' UpdateBandwidthRateLimitResponse (Maybe Text)
ubrlrGatewayARN = lens _ubrlrGatewayARN (\ s a -> s{_ubrlrGatewayARN = a});

-- | FIXME: Undocumented member.
ubrlrStatus :: Lens' UpdateBandwidthRateLimitResponse Int
ubrlrStatus = lens _ubrlrStatus (\ s a -> s{_ubrlrStatus = a});
