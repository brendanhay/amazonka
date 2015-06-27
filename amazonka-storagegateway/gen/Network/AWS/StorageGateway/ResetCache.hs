{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.ResetCache
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

-- | This operation resets all cache disks that have encountered a error and
-- makes the disks available for reconfiguration as cache storage. If your
-- cache disk encounters a error, the gateway prevents read and write
-- operations on virtual tapes in the gateway. For example, an error can
-- occur when a disk is corrupted or removed from the gateway. When a cache
-- is reset, the gateway loses its cache storage. At this point you can
-- reconfigure the disks as cache disks.
--
-- If the cache disk you are resetting contains data that has not been
-- uploaded to Amazon S3 yet, that data can be lost. After you reset cache
-- disks, there will be no configured cache disks left in the gateway, so
-- you must configure at least one new cache disk for your gateway to
-- function properly.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ResetCache.html>
module Network.AWS.StorageGateway.ResetCache
    (
    -- * Request
      ResetCache
    -- ** Request constructor
    , resetCache
    -- ** Request lenses
    , rcGatewayARN

    -- * Response
    , ResetCacheResponse
    -- ** Response constructor
    , resetCacheResponse
    -- ** Response lenses
    , rcrGatewayARN
    , rcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'resetCache' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcGatewayARN'
newtype ResetCache = ResetCache'
    { _rcGatewayARN :: Text
    } deriving (Eq,Read,Show)

-- | 'ResetCache' smart constructor.
resetCache :: Text -> ResetCache
resetCache pGatewayARN =
    ResetCache'
    { _rcGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
rcGatewayARN :: Lens' ResetCache Text
rcGatewayARN = lens _rcGatewayARN (\ s a -> s{_rcGatewayARN = a});

instance AWSRequest ResetCache where
        type Sv ResetCache = StorageGateway
        type Rs ResetCache = ResetCacheResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ResetCacheResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

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
          = object ["GatewayARN" .= _rcGatewayARN]

instance ToPath ResetCache where
        toPath = const "/"

instance ToQuery ResetCache where
        toQuery = const mempty

-- | /See:/ 'resetCacheResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcrGatewayARN'
--
-- * 'rcrStatus'
data ResetCacheResponse = ResetCacheResponse'
    { _rcrGatewayARN :: !(Maybe Text)
    , _rcrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'ResetCacheResponse' smart constructor.
resetCacheResponse :: Int -> ResetCacheResponse
resetCacheResponse pStatus =
    ResetCacheResponse'
    { _rcrGatewayARN = Nothing
    , _rcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
rcrGatewayARN :: Lens' ResetCacheResponse (Maybe Text)
rcrGatewayARN = lens _rcrGatewayARN (\ s a -> s{_rcrGatewayARN = a});

-- | FIXME: Undocumented member.
rcrStatus :: Lens' ResetCacheResponse Int
rcrStatus = lens _rcrStatus (\ s a -> s{_rcrStatus = a});
