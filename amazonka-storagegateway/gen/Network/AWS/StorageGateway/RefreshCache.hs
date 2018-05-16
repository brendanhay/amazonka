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
-- Module      : Network.AWS.StorageGateway.RefreshCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes the cache for the specified file share. This operation finds objects in the Amazon S3 bucket that were added, removed or replaced since the gateway last listed the bucket's contents and cached the results. This operation is only supported in the file gateway type.
--
--
module Network.AWS.StorageGateway.RefreshCache
    (
    -- * Creating a Request
      refreshCache
    , RefreshCache
    -- * Request Lenses
    , rcFileShareARN

    -- * Destructuring the Response
    , refreshCacheResponse
    , RefreshCacheResponse
    -- * Response Lenses
    , rcrsFileShareARN
    , rcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'refreshCache' smart constructor.
newtype RefreshCache = RefreshCache'
  { _rcFileShareARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RefreshCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcFileShareARN' - Undocumented member.
refreshCache
    :: Text -- ^ 'rcFileShareARN'
    -> RefreshCache
refreshCache pFileShareARN_ = RefreshCache' {_rcFileShareARN = pFileShareARN_}


-- | Undocumented member.
rcFileShareARN :: Lens' RefreshCache Text
rcFileShareARN = lens _rcFileShareARN (\ s a -> s{_rcFileShareARN = a})

instance AWSRequest RefreshCache where
        type Rs RefreshCache = RefreshCacheResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 RefreshCacheResponse' <$>
                   (x .?> "FileShareARN") <*> (pure (fromEnum s)))

instance Hashable RefreshCache where

instance NFData RefreshCache where

instance ToHeaders RefreshCache where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.RefreshCache" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RefreshCache where
        toJSON RefreshCache'{..}
          = object
              (catMaybes
                 [Just ("FileShareARN" .= _rcFileShareARN)])

instance ToPath RefreshCache where
        toPath = const "/"

instance ToQuery RefreshCache where
        toQuery = const mempty

-- | /See:/ 'refreshCacheResponse' smart constructor.
data RefreshCacheResponse = RefreshCacheResponse'
  { _rcrsFileShareARN   :: !(Maybe Text)
  , _rcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RefreshCacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsFileShareARN' - Undocumented member.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
refreshCacheResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> RefreshCacheResponse
refreshCacheResponse pResponseStatus_ =
  RefreshCacheResponse'
    {_rcrsFileShareARN = Nothing, _rcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rcrsFileShareARN :: Lens' RefreshCacheResponse (Maybe Text)
rcrsFileShareARN = lens _rcrsFileShareARN (\ s a -> s{_rcrsFileShareARN = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RefreshCacheResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData RefreshCacheResponse where
