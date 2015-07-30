{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ResetCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation resets all cache disks that have encountered a error and
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
    , rcrsGatewayARN
    , rcrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetCache' smart constructor.
resetCache :: Text -> ResetCache
resetCache pGatewayARN_ =
    ResetCache'
    { _rcGatewayARN = pGatewayARN_
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
-- * 'rcrsGatewayARN'
--
-- * 'rcrsStatus'
data ResetCacheResponse = ResetCacheResponse'
    { _rcrsGatewayARN :: !(Maybe Text)
    , _rcrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetCacheResponse' smart constructor.
resetCacheResponse :: Int -> ResetCacheResponse
resetCacheResponse pStatus_ =
    ResetCacheResponse'
    { _rcrsGatewayARN = Nothing
    , _rcrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rcrsGatewayARN :: Lens' ResetCacheResponse (Maybe Text)
rcrsGatewayARN = lens _rcrsGatewayARN (\ s a -> s{_rcrsGatewayARN = a});

-- | FIXME: Undocumented member.
rcrsStatus :: Lens' ResetCacheResponse Int
rcrsStatus = lens _rcrsStatus (\ s a -> s{_rcrsStatus = a});
