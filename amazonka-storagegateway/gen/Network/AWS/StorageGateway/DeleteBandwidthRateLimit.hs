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
    , dbrlbGatewayARN
    , dbrlbBandwidthType

    -- * Destructuring the Response
    , deleteBandwidthRateLimitResponse
    , DeleteBandwidthRateLimitResponse
    -- * Response Lenses
    , delrsGatewayARN
    , delrsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'deleteBandwidthRateLimit' smart constructor.
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
    { _dbrlbGatewayARN    :: !Text
    , _dbrlbBandwidthType :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBandwidthRateLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrlbGatewayARN'
--
-- * 'dbrlbBandwidthType'
deleteBandwidthRateLimit
    :: Text -- ^ 'dbrlbGatewayARN'
    -> Text -- ^ 'dbrlbBandwidthType'
    -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit pGatewayARN_ pBandwidthType_ =
    DeleteBandwidthRateLimit'
    { _dbrlbGatewayARN = pGatewayARN_
    , _dbrlbBandwidthType = pBandwidthType_
    }

-- | Undocumented member.
dbrlbGatewayARN :: Lens' DeleteBandwidthRateLimit Text
dbrlbGatewayARN = lens _dbrlbGatewayARN (\ s a -> s{_dbrlbGatewayARN = a});

-- | Undocumented member.
dbrlbBandwidthType :: Lens' DeleteBandwidthRateLimit Text
dbrlbBandwidthType = lens _dbrlbBandwidthType (\ s a -> s{_dbrlbBandwidthType = a});

instance AWSRequest DeleteBandwidthRateLimit where
        type Rs DeleteBandwidthRateLimit =
             DeleteBandwidthRateLimitResponse
        request = postJSON storageGateway
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
              (catMaybes
                 [Just ("GatewayARN" .= _dbrlbGatewayARN),
                  Just ("BandwidthType" .= _dbrlbBandwidthType)])

instance ToPath DeleteBandwidthRateLimit where
        toPath = const "/"

instance ToQuery DeleteBandwidthRateLimit where
        toQuery = const mempty

-- | A JSON object containing the of the gateway whose bandwidth rate
-- information was deleted.
--
-- /See:/ 'deleteBandwidthRateLimitResponse' smart constructor.
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
    { _delrsGatewayARN     :: !(Maybe Text)
    , _delrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsGatewayARN'
--
-- * 'delrsResponseStatus'
deleteBandwidthRateLimitResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse pResponseStatus_ =
    DeleteBandwidthRateLimitResponse'
    { _delrsGatewayARN = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
delrsGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
delrsGatewayARN = lens _delrsGatewayARN (\ s a -> s{_delrsGatewayARN = a});

-- | The response status code.
delrsResponseStatus :: Lens' DeleteBandwidthRateLimitResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});
