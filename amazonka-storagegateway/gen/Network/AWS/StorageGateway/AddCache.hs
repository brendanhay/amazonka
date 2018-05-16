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
-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as cache for a gateway. This operation is only supported in the cached volume, tape and file gateway type (see <http://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html Storage Gateway Concepts> ).
--
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add cache, and one or more disk IDs that you want to configure as cache.
--
module Network.AWS.StorageGateway.AddCache
    (
    -- * Creating a Request
      addCache
    , AddCache
    -- * Request Lenses
    , acGatewayARN
    , acDiskIds

    -- * Destructuring the Response
    , addCacheResponse
    , AddCacheResponse
    -- * Response Lenses
    , acrsGatewayARN
    , acrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'addCache' smart constructor.
data AddCache = AddCache'
  { _acGatewayARN :: !Text
  , _acDiskIds    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acGatewayARN' - Undocumented member.
--
-- * 'acDiskIds' - Undocumented member.
addCache
    :: Text -- ^ 'acGatewayARN'
    -> AddCache
addCache pGatewayARN_ =
  AddCache' {_acGatewayARN = pGatewayARN_, _acDiskIds = mempty}


-- | Undocumented member.
acGatewayARN :: Lens' AddCache Text
acGatewayARN = lens _acGatewayARN (\ s a -> s{_acGatewayARN = a})

-- | Undocumented member.
acDiskIds :: Lens' AddCache [Text]
acDiskIds = lens _acDiskIds (\ s a -> s{_acDiskIds = a}) . _Coerce

instance AWSRequest AddCache where
        type Rs AddCache = AddCacheResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 AddCacheResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable AddCache where

instance NFData AddCache where

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
              (catMaybes
                 [Just ("GatewayARN" .= _acGatewayARN),
                  Just ("DiskIds" .= _acDiskIds)])

instance ToPath AddCache where
        toPath = const "/"

instance ToQuery AddCache where
        toQuery = const mempty

-- | /See:/ 'addCacheResponse' smart constructor.
data AddCacheResponse = AddCacheResponse'
  { _acrsGatewayARN     :: !(Maybe Text)
  , _acrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddCacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acrsGatewayARN' - Undocumented member.
--
-- * 'acrsResponseStatus' - -- | The response status code.
addCacheResponse
    :: Int -- ^ 'acrsResponseStatus'
    -> AddCacheResponse
addCacheResponse pResponseStatus_ =
  AddCacheResponse'
    {_acrsGatewayARN = Nothing, _acrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
acrsGatewayARN :: Lens' AddCacheResponse (Maybe Text)
acrsGatewayARN = lens _acrsGatewayARN (\ s a -> s{_acrsGatewayARN = a})

-- | -- | The response status code.
acrsResponseStatus :: Lens' AddCacheResponse Int
acrsResponseStatus = lens _acrsResponseStatus (\ s a -> s{_acrsResponseStatus = a})

instance NFData AddCacheResponse where
