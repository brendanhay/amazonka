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
-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the bandwidth rate limits of a gateway. You can
-- delete either the upload and download bandwidth rate limit, or you can
-- delete both. If you delete only one of the limits, the other limit
-- remains unchanged. To specify which gateway to work with, use the Amazon
-- Resource Name (ARN) of the gateway in your request.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteBandwidthRateLimit.html AWS API Reference> for DeleteBandwidthRateLimit.
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    (
    -- * Creating a Request
      deleteBandwidthRateLimit
    , DeleteBandwidthRateLimit
    -- * Request Lenses
    , delGatewayARN
    , delBandwidthType

    -- * Destructuring the Response
    , deleteBandwidthRateLimitResponse
    , DeleteBandwidthRateLimitResponse
    -- * Response Lenses
    , delrsGatewayARN
    , delrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'deleteBandwidthRateLimit' smart constructor.
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
    { _delGatewayARN    :: !Text
    , _delBandwidthType :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBandwidthRateLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delGatewayARN'
--
-- * 'delBandwidthType'
deleteBandwidthRateLimit
    :: Text -- ^ 'delGatewayARN'
    -> Text -- ^ 'delBandwidthType'
    -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN_ pBandwidthType_ =
    DeleteBandwidthRateLimit'
    { _delGatewayARN = pGatewayARN_
    , _delBandwidthType = pBandwidthType_
    }

-- | Undocumented member.
delGatewayARN :: Lens' DeleteBandwidthRateLimit Text
delGatewayARN = lens _delGatewayARN (\ s a -> s{_delGatewayARN = a});

-- | Undocumented member.
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
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
    { _delrsGatewayARN :: !(Maybe Text)
    , _delrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsGatewayARN'
--
-- * 'delrsStatus'
deleteBandwidthRateLimitResponse
    :: Int -- ^ 'delrsStatus'
    -> DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse pStatus_ =
    DeleteBandwidthRateLimitResponse'
    { _delrsGatewayARN = Nothing
    , _delrsStatus = pStatus_
    }

-- | Undocumented member.
delrsGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
delrsGatewayARN = lens _delrsGatewayARN (\ s a -> s{_delrsGatewayARN = a});

-- | The response status code.
delrsStatus :: Lens' DeleteBandwidthRateLimitResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
