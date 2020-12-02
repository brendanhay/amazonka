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
-- Module      : Network.AWS.Lightsail.UpdateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- Use this action to update the configuration of your existing distribution
module Network.AWS.Lightsail.UpdateDistribution
  ( -- * Creating a Request
    updateDistribution,
    UpdateDistribution,

    -- * Request Lenses
    udOrigin,
    udCacheBehaviorSettings,
    udIsEnabled,
    udDefaultCacheBehavior,
    udCacheBehaviors,
    udDistributionName,

    -- * Destructuring the Response
    updateDistributionResponse,
    UpdateDistributionResponse,

    -- * Response Lenses
    udrsOperation,
    udrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
  { _udOrigin ::
      !(Maybe InputOrigin),
    _udCacheBehaviorSettings :: !(Maybe CacheSettings),
    _udIsEnabled :: !(Maybe Bool),
    _udDefaultCacheBehavior :: !(Maybe CacheBehavior),
    _udCacheBehaviors :: !(Maybe [CacheBehaviorPerPath]),
    _udDistributionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udOrigin' - An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer. The distribution pulls, caches, and serves content from the origin.
--
-- * 'udCacheBehaviorSettings' - An object that describes the cache behavior settings for the distribution.
--
-- * 'udIsEnabled' - Indicates whether to enable the distribution.
--
-- * 'udDefaultCacheBehavior' - An object that describes the default cache behavior for the distribution.
--
-- * 'udCacheBehaviors' - An array of objects that describe the per-path cache behavior for the distribution.
--
-- * 'udDistributionName' - The name of the distribution to update. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
updateDistribution ::
  -- | 'udDistributionName'
  Text ->
  UpdateDistribution
updateDistribution pDistributionName_ =
  UpdateDistribution'
    { _udOrigin = Nothing,
      _udCacheBehaviorSettings = Nothing,
      _udIsEnabled = Nothing,
      _udDefaultCacheBehavior = Nothing,
      _udCacheBehaviors = Nothing,
      _udDistributionName = pDistributionName_
    }

-- | An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer. The distribution pulls, caches, and serves content from the origin.
udOrigin :: Lens' UpdateDistribution (Maybe InputOrigin)
udOrigin = lens _udOrigin (\s a -> s {_udOrigin = a})

-- | An object that describes the cache behavior settings for the distribution.
udCacheBehaviorSettings :: Lens' UpdateDistribution (Maybe CacheSettings)
udCacheBehaviorSettings = lens _udCacheBehaviorSettings (\s a -> s {_udCacheBehaviorSettings = a})

-- | Indicates whether to enable the distribution.
udIsEnabled :: Lens' UpdateDistribution (Maybe Bool)
udIsEnabled = lens _udIsEnabled (\s a -> s {_udIsEnabled = a})

-- | An object that describes the default cache behavior for the distribution.
udDefaultCacheBehavior :: Lens' UpdateDistribution (Maybe CacheBehavior)
udDefaultCacheBehavior = lens _udDefaultCacheBehavior (\s a -> s {_udDefaultCacheBehavior = a})

-- | An array of objects that describe the per-path cache behavior for the distribution.
udCacheBehaviors :: Lens' UpdateDistribution [CacheBehaviorPerPath]
udCacheBehaviors = lens _udCacheBehaviors (\s a -> s {_udCacheBehaviors = a}) . _Default . _Coerce

-- | The name of the distribution to update. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
udDistributionName :: Lens' UpdateDistribution Text
udDistributionName = lens _udDistributionName (\s a -> s {_udDistributionName = a})

instance AWSRequest UpdateDistribution where
  type Rs UpdateDistribution = UpdateDistributionResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          UpdateDistributionResponse'
            <$> (x .?> "operation") <*> (pure (fromEnum s))
      )

instance Hashable UpdateDistribution

instance NFData UpdateDistribution

instance ToHeaders UpdateDistribution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.UpdateDistribution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateDistribution where
  toJSON UpdateDistribution' {..} =
    object
      ( catMaybes
          [ ("origin" .=) <$> _udOrigin,
            ("cacheBehaviorSettings" .=) <$> _udCacheBehaviorSettings,
            ("isEnabled" .=) <$> _udIsEnabled,
            ("defaultCacheBehavior" .=) <$> _udDefaultCacheBehavior,
            ("cacheBehaviors" .=) <$> _udCacheBehaviors,
            Just ("distributionName" .= _udDistributionName)
          ]
      )

instance ToPath UpdateDistribution where
  toPath = const "/"

instance ToQuery UpdateDistribution where
  toQuery = const mempty

-- | /See:/ 'updateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { _udrsOperation ::
      !(Maybe Operation),
    _udrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsOperation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDistributionResponse ::
  -- | 'udrsResponseStatus'
  Int ->
  UpdateDistributionResponse
updateDistributionResponse pResponseStatus_ =
  UpdateDistributionResponse'
    { _udrsOperation = Nothing,
      _udrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
udrsOperation :: Lens' UpdateDistributionResponse (Maybe Operation)
udrsOperation = lens _udrsOperation (\s a -> s {_udrsOperation = a})

-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDistributionResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\s a -> s {_udrsResponseStatus = a})

instance NFData UpdateDistributionResponse
