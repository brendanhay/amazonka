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
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateBandwidthRateLimit.html AWS API Reference> for UpdateBandwidthRateLimit.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
    (
    -- * Creating a Request
      UpdateBandwidthRateLimit
    , updateBandwidthRateLimit
    -- * Request Lenses
    , ubrlAverageUploadRateLimitInBitsPerSec
    , ubrlAverageDownloadRateLimitInBitsPerSec
    , ubrlGatewayARN

    -- * Destructuring the Response
    , UpdateBandwidthRateLimitResponse
    , updateBandwidthRateLimitResponse
    -- * Response Lenses
    , ubrlrsGatewayARN
    , ubrlrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

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
updateBandwidthRateLimit pGatewayARN_ =
    UpdateBandwidthRateLimit'
    { _ubrlAverageUploadRateLimitInBitsPerSec = Nothing
    , _ubrlAverageDownloadRateLimitInBitsPerSec = Nothing
    , _ubrlGatewayARN = pGatewayARN_
    }

-- | The average upload bandwidth rate limit in bits per second.
ubrlAverageUploadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlAverageUploadRateLimitInBitsPerSec = lens _ubrlAverageUploadRateLimitInBitsPerSec (\ s a -> s{_ubrlAverageUploadRateLimitInBitsPerSec = a}) . mapping _Nat;

-- | The average download bandwidth rate limit in bits per second.
ubrlAverageDownloadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlAverageDownloadRateLimitInBitsPerSec = lens _ubrlAverageDownloadRateLimitInBitsPerSec (\ s a -> s{_ubrlAverageDownloadRateLimitInBitsPerSec = a}) . mapping _Nat;

-- | Undocumented member.
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
-- * 'ubrlrsGatewayARN'
--
-- * 'ubrlrsStatus'
data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse'
    { _ubrlrsGatewayARN :: !(Maybe Text)
    , _ubrlrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBandwidthRateLimitResponse' smart constructor.
updateBandwidthRateLimitResponse :: Int -> UpdateBandwidthRateLimitResponse
updateBandwidthRateLimitResponse pStatus_ =
    UpdateBandwidthRateLimitResponse'
    { _ubrlrsGatewayARN = Nothing
    , _ubrlrsStatus = pStatus_
    }

-- | Undocumented member.
ubrlrsGatewayARN :: Lens' UpdateBandwidthRateLimitResponse (Maybe Text)
ubrlrsGatewayARN = lens _ubrlrsGatewayARN (\ s a -> s{_ubrlrsGatewayARN = a});

-- | Undocumented member.
ubrlrsStatus :: Lens' UpdateBandwidthRateLimitResponse Int
ubrlrsStatus = lens _ubrlrsStatus (\ s a -> s{_ubrlrsStatus = a});
