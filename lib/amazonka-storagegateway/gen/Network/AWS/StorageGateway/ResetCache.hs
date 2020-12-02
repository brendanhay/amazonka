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
-- Module      : Network.AWS.StorageGateway.ResetCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all cache disks that have encountered a error and makes the disks available for reconfiguration as cache storage. If your cache disk encounters a error, the gateway prevents read and write operations on virtual tapes in the gateway. For example, an error can occur when a disk is corrupted or removed from the gateway. When a cache is reset, the gateway loses its cache storage. At this point you can reconfigure the disks as cache disks. This operation is only supported in the cached volume and tape types.
--
--
-- /Important:/ If the cache disk you are resetting contains data that has not been uploaded to Amazon S3 yet, that data can be lost. After you reset cache disks, there will be no configured cache disks left in the gateway, so you must configure at least one new cache disk for your gateway to function properly.
--
module Network.AWS.StorageGateway.ResetCache
    (
    -- * Creating a Request
      resetCache
    , ResetCache
    -- * Request Lenses
    , rcGatewayARN

    -- * Destructuring the Response
    , resetCacheResponse
    , ResetCacheResponse
    -- * Response Lenses
    , rrsGatewayARN
    , rrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'resetCache' smart constructor.
newtype ResetCache = ResetCache'
  { _rcGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcGatewayARN' - Undocumented member.
resetCache
    :: Text -- ^ 'rcGatewayARN'
    -> ResetCache
resetCache pGatewayARN_ = ResetCache' {_rcGatewayARN = pGatewayARN_}


-- | Undocumented member.
rcGatewayARN :: Lens' ResetCache Text
rcGatewayARN = lens _rcGatewayARN (\ s a -> s{_rcGatewayARN = a})

instance AWSRequest ResetCache where
        type Rs ResetCache = ResetCacheResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ResetCacheResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable ResetCache where

instance NFData ResetCache where

instance ToHeaders ResetCache where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ResetCache" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResetCache where
        toJSON ResetCache'{..}
          = object
              (catMaybes [Just ("GatewayARN" .= _rcGatewayARN)])

instance ToPath ResetCache where
        toPath = const "/"

instance ToQuery ResetCache where
        toQuery = const mempty

-- | /See:/ 'resetCacheResponse' smart constructor.
data ResetCacheResponse = ResetCacheResponse'
  { _rrsGatewayARN     :: !(Maybe Text)
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetCacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsGatewayARN' - Undocumented member.
--
-- * 'rrsResponseStatus' - -- | The response status code.
resetCacheResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> ResetCacheResponse
resetCacheResponse pResponseStatus_ =
  ResetCacheResponse'
    {_rrsGatewayARN = Nothing, _rrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rrsGatewayARN :: Lens' ResetCacheResponse (Maybe Text)
rrsGatewayARN = lens _rrsGatewayARN (\ s a -> s{_rrsGatewayARN = a})

-- | -- | The response status code.
rrsResponseStatus :: Lens' ResetCacheResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData ResetCacheResponse where
