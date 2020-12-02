{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ResetDistributionCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes currently cached content from your Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- After resetting the cache, the next time a content request is made, your distribution pulls, serves, and caches it from the origin.
module Network.AWS.Lightsail.ResetDistributionCache
  ( -- * Creating a Request
    resetDistributionCache,
    ResetDistributionCache,

    -- * Request Lenses
    rdcDistributionName,

    -- * Destructuring the Response
    resetDistributionCacheResponse,
    ResetDistributionCacheResponse,

    -- * Response Lenses
    rdcrsStatus,
    rdcrsOperation,
    rdcrsCreateTime,
    rdcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetDistributionCache' smart constructor.
newtype ResetDistributionCache = ResetDistributionCache'
  { _rdcDistributionName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResetDistributionCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcDistributionName' - The name of the distribution for which to reset cache. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
resetDistributionCache ::
  ResetDistributionCache
resetDistributionCache =
  ResetDistributionCache' {_rdcDistributionName = Nothing}

-- | The name of the distribution for which to reset cache. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
rdcDistributionName :: Lens' ResetDistributionCache (Maybe Text)
rdcDistributionName = lens _rdcDistributionName (\s a -> s {_rdcDistributionName = a})

instance AWSRequest ResetDistributionCache where
  type Rs ResetDistributionCache = ResetDistributionCacheResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          ResetDistributionCacheResponse'
            <$> (x .?> "status")
            <*> (x .?> "operation")
            <*> (x .?> "createTime")
            <*> (pure (fromEnum s))
      )

instance Hashable ResetDistributionCache

instance NFData ResetDistributionCache

instance ToHeaders ResetDistributionCache where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.ResetDistributionCache" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ResetDistributionCache where
  toJSON ResetDistributionCache' {..} =
    object
      (catMaybes [("distributionName" .=) <$> _rdcDistributionName])

instance ToPath ResetDistributionCache where
  toPath = const "/"

instance ToQuery ResetDistributionCache where
  toQuery = const mempty

-- | /See:/ 'resetDistributionCacheResponse' smart constructor.
data ResetDistributionCacheResponse = ResetDistributionCacheResponse'
  { _rdcrsStatus ::
      !(Maybe Text),
    _rdcrsOperation ::
      !(Maybe Operation),
    _rdcrsCreateTime ::
      !(Maybe POSIX),
    _rdcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResetDistributionCacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcrsStatus' - The status of the reset cache request.
--
-- * 'rdcrsOperation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'rdcrsCreateTime' - The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
--
-- * 'rdcrsResponseStatus' - -- | The response status code.
resetDistributionCacheResponse ::
  -- | 'rdcrsResponseStatus'
  Int ->
  ResetDistributionCacheResponse
resetDistributionCacheResponse pResponseStatus_ =
  ResetDistributionCacheResponse'
    { _rdcrsStatus = Nothing,
      _rdcrsOperation = Nothing,
      _rdcrsCreateTime = Nothing,
      _rdcrsResponseStatus = pResponseStatus_
    }

-- | The status of the reset cache request.
rdcrsStatus :: Lens' ResetDistributionCacheResponse (Maybe Text)
rdcrsStatus = lens _rdcrsStatus (\s a -> s {_rdcrsStatus = a})

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
rdcrsOperation :: Lens' ResetDistributionCacheResponse (Maybe Operation)
rdcrsOperation = lens _rdcrsOperation (\s a -> s {_rdcrsOperation = a})

-- | The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
rdcrsCreateTime :: Lens' ResetDistributionCacheResponse (Maybe UTCTime)
rdcrsCreateTime = lens _rdcrsCreateTime (\s a -> s {_rdcrsCreateTime = a}) . mapping _Time

-- | -- | The response status code.
rdcrsResponseStatus :: Lens' ResetDistributionCacheResponse Int
rdcrsResponseStatus = lens _rdcrsResponseStatus (\s a -> s {_rdcrsResponseStatus = a})

instance NFData ResetDistributionCacheResponse
