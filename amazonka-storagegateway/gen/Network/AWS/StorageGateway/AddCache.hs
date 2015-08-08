{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation configures one or more gateway local disks as cache for a
-- cached-volume gateway. This operation is supported only for the
-- gateway-cached volume architecture (see
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html Storage Gateway Concepts>).
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add cache, and one or more disk IDs that you want to
-- configure as cache.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_AddCache.html AWS API Reference> for AddCache.
module Network.AWS.StorageGateway.AddCache
    (
    -- * Creating a Request
      AddCache
    , addCache
    -- * Request Lenses
    , acGatewayARN
    , acDiskIds

    -- * Destructuring the Response
    , AddCacheResponse
    , addCacheResponse
    -- * Response Lenses
    , acrsGatewayARN
    , acrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'addCache' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acGatewayARN'
--
-- * 'acDiskIds'
data AddCache = AddCache'
    { _acGatewayARN :: !Text
    , _acDiskIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddCache' smart constructor.
addCache :: Text -> AddCache
addCache pGatewayARN_ =
    AddCache'
    { _acGatewayARN = pGatewayARN_
    , _acDiskIds = mempty
    }

-- | Undocumented member.
acGatewayARN :: Lens' AddCache Text
acGatewayARN = lens _acGatewayARN (\ s a -> s{_acGatewayARN = a});

-- | Undocumented member.
acDiskIds :: Lens' AddCache [Text]
acDiskIds = lens _acDiskIds (\ s a -> s{_acDiskIds = a}) . _Coerce;

instance AWSRequest AddCache where
        type Sv AddCache = StorageGateway
        type Rs AddCache = AddCacheResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddCacheResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders AddCache where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.AddCache" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddCache where
        toJSON AddCache'{..}
          = object
              ["GatewayARN" .= _acGatewayARN,
               "DiskIds" .= _acDiskIds]

instance ToPath AddCache where
        toPath = const "/"

instance ToQuery AddCache where
        toQuery = const mempty

-- | /See:/ 'addCacheResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acrsGatewayARN'
--
-- * 'acrsStatus'
data AddCacheResponse = AddCacheResponse'
    { _acrsGatewayARN :: !(Maybe Text)
    , _acrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddCacheResponse' smart constructor.
addCacheResponse :: Int -> AddCacheResponse
addCacheResponse pStatus_ =
    AddCacheResponse'
    { _acrsGatewayARN = Nothing
    , _acrsStatus = pStatus_
    }

-- | Undocumented member.
acrsGatewayARN :: Lens' AddCacheResponse (Maybe Text)
acrsGatewayARN = lens _acrsGatewayARN (\ s a -> s{_acrsGatewayARN = a});

-- | Undocumented member.
acrsStatus :: Lens' AddCacheResponse Int
acrsStatus = lens _acrsStatus (\ s a -> s{_acrsStatus = a});
