{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimit
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the bandwidth rate limits of a gateway. You can
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
    , ubrlrqAverageUploadRateLimitInBitsPerSec
    , ubrlrqAverageDownloadRateLimitInBitsPerSec
    , ubrlrqGatewayARN

    -- * Response
    , UpdateBandwidthRateLimitResponse
    -- ** Response constructor
    , updateBandwidthRateLimitResponse
    -- ** Response lenses
    , ubrlrsGatewayARN
    , ubrlrsStatus
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
-- * 'ubrlrqAverageUploadRateLimitInBitsPerSec'
--
-- * 'ubrlrqAverageDownloadRateLimitInBitsPerSec'
--
-- * 'ubrlrqGatewayARN'
data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit'
    { _ubrlrqAverageUploadRateLimitInBitsPerSec   :: !(Maybe Nat)
    , _ubrlrqAverageDownloadRateLimitInBitsPerSec :: !(Maybe Nat)
    , _ubrlrqGatewayARN                           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBandwidthRateLimit' smart constructor.
updateBandwidthRateLimit :: Text -> UpdateBandwidthRateLimit
updateBandwidthRateLimit pGatewayARN =
    UpdateBandwidthRateLimit'
    { _ubrlrqAverageUploadRateLimitInBitsPerSec = Nothing
    , _ubrlrqAverageDownloadRateLimitInBitsPerSec = Nothing
    , _ubrlrqGatewayARN = pGatewayARN
    }

-- | The average upload bandwidth rate limit in bits per second.
ubrlrqAverageUploadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlrqAverageUploadRateLimitInBitsPerSec = lens _ubrlrqAverageUploadRateLimitInBitsPerSec (\ s a -> s{_ubrlrqAverageUploadRateLimitInBitsPerSec = a}) . mapping _Nat;

-- | The average download bandwidth rate limit in bits per second.
ubrlrqAverageDownloadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlrqAverageDownloadRateLimitInBitsPerSec = lens _ubrlrqAverageDownloadRateLimitInBitsPerSec (\ s a -> s{_ubrlrqAverageDownloadRateLimitInBitsPerSec = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
ubrlrqGatewayARN :: Lens' UpdateBandwidthRateLimit Text
ubrlrqGatewayARN = lens _ubrlrqGatewayARN (\ s a -> s{_ubrlrqGatewayARN = a});

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
                 _ubrlrqAverageUploadRateLimitInBitsPerSec,
               "AverageDownloadRateLimitInBitsPerSec" .=
                 _ubrlrqAverageDownloadRateLimitInBitsPerSec,
               "GatewayARN" .= _ubrlrqGatewayARN]

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
-- * 'ubrlrsGatewayARN'
--
-- * 'ubrlrsStatus'
data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse'
    { _ubrlrsGatewayARN :: !(Maybe Text)
    , _ubrlrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBandwidthRateLimitResponse' smart constructor.
updateBandwidthRateLimitResponse :: Int -> UpdateBandwidthRateLimitResponse
updateBandwidthRateLimitResponse pStatus =
    UpdateBandwidthRateLimitResponse'
    { _ubrlrsGatewayARN = Nothing
    , _ubrlrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ubrlrsGatewayARN :: Lens' UpdateBandwidthRateLimitResponse (Maybe Text)
ubrlrsGatewayARN = lens _ubrlrsGatewayARN (\ s a -> s{_ubrlrsGatewayARN = a});

-- | FIXME: Undocumented member.
ubrlrsStatus :: Lens' UpdateBandwidthRateLimitResponse Int
ubrlrsStatus = lens _ubrlrsStatus (\ s a -> s{_ubrlrsStatus = a});
