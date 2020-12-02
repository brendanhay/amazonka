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
-- Module      : Network.AWS.Lightsail.CreateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- A distribution is a globally distributed network of caching servers that improve the performance of your website or web application hosted on a Lightsail instance. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-content-delivery-network-distributions Content delivery networks in Amazon Lightsail> .
module Network.AWS.Lightsail.CreateDistribution
  ( -- * Creating a Request
    createDistribution,
    CreateDistribution,

    -- * Request Lenses
    cdCacheBehaviorSettings,
    cdCacheBehaviors,
    cdTags,
    cdDistributionName,
    cdOrigin,
    cdDefaultCacheBehavior,
    cdBundleId,

    -- * Destructuring the Response
    createDistributionResponse,
    CreateDistributionResponse,

    -- * Response Lenses
    crersDistribution,
    crersOperation,
    crersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDistribution' smart constructor.
data CreateDistribution = CreateDistribution'
  { _cdCacheBehaviorSettings ::
      !(Maybe CacheSettings),
    _cdCacheBehaviors :: !(Maybe [CacheBehaviorPerPath]),
    _cdTags :: !(Maybe [Tag]),
    _cdDistributionName :: !Text,
    _cdOrigin :: !InputOrigin,
    _cdDefaultCacheBehavior :: !CacheBehavior,
    _cdBundleId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCacheBehaviorSettings' - An object that describes the cache behavior settings for the distribution.
--
-- * 'cdCacheBehaviors' - An array of objects that describe the per-path cache behavior for the distribution.
--
-- * 'cdTags' - The tag keys and optional values to add to the distribution during create. Use the @TagResource@ action to tag a resource after it's created.
--
-- * 'cdDistributionName' - The name for the distribution.
--
-- * 'cdOrigin' - An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer. The distribution pulls, caches, and serves content from the origin.
--
-- * 'cdDefaultCacheBehavior' - An object that describes the default cache behavior for the distribution.
--
-- * 'cdBundleId' - The bundle ID to use for the distribution. A distribution bundle describes the specifications of your distribution, such as the monthly cost and monthly network transfer quota. Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
createDistribution ::
  -- | 'cdDistributionName'
  Text ->
  -- | 'cdOrigin'
  InputOrigin ->
  -- | 'cdDefaultCacheBehavior'
  CacheBehavior ->
  -- | 'cdBundleId'
  Text ->
  CreateDistribution
createDistribution
  pDistributionName_
  pOrigin_
  pDefaultCacheBehavior_
  pBundleId_ =
    CreateDistribution'
      { _cdCacheBehaviorSettings = Nothing,
        _cdCacheBehaviors = Nothing,
        _cdTags = Nothing,
        _cdDistributionName = pDistributionName_,
        _cdOrigin = pOrigin_,
        _cdDefaultCacheBehavior = pDefaultCacheBehavior_,
        _cdBundleId = pBundleId_
      }

-- | An object that describes the cache behavior settings for the distribution.
cdCacheBehaviorSettings :: Lens' CreateDistribution (Maybe CacheSettings)
cdCacheBehaviorSettings = lens _cdCacheBehaviorSettings (\s a -> s {_cdCacheBehaviorSettings = a})

-- | An array of objects that describe the per-path cache behavior for the distribution.
cdCacheBehaviors :: Lens' CreateDistribution [CacheBehaviorPerPath]
cdCacheBehaviors = lens _cdCacheBehaviors (\s a -> s {_cdCacheBehaviors = a}) . _Default . _Coerce

-- | The tag keys and optional values to add to the distribution during create. Use the @TagResource@ action to tag a resource after it's created.
cdTags :: Lens' CreateDistribution [Tag]
cdTags = lens _cdTags (\s a -> s {_cdTags = a}) . _Default . _Coerce

-- | The name for the distribution.
cdDistributionName :: Lens' CreateDistribution Text
cdDistributionName = lens _cdDistributionName (\s a -> s {_cdDistributionName = a})

-- | An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer. The distribution pulls, caches, and serves content from the origin.
cdOrigin :: Lens' CreateDistribution InputOrigin
cdOrigin = lens _cdOrigin (\s a -> s {_cdOrigin = a})

-- | An object that describes the default cache behavior for the distribution.
cdDefaultCacheBehavior :: Lens' CreateDistribution CacheBehavior
cdDefaultCacheBehavior = lens _cdDefaultCacheBehavior (\s a -> s {_cdDefaultCacheBehavior = a})

-- | The bundle ID to use for the distribution. A distribution bundle describes the specifications of your distribution, such as the monthly cost and monthly network transfer quota. Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
cdBundleId :: Lens' CreateDistribution Text
cdBundleId = lens _cdBundleId (\s a -> s {_cdBundleId = a})

instance AWSRequest CreateDistribution where
  type Rs CreateDistribution = CreateDistributionResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateDistributionResponse'
            <$> (x .?> "distribution")
            <*> (x .?> "operation")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateDistribution

instance NFData CreateDistribution

instance ToHeaders CreateDistribution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateDistribution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDistribution where
  toJSON CreateDistribution' {..} =
    object
      ( catMaybes
          [ ("cacheBehaviorSettings" .=) <$> _cdCacheBehaviorSettings,
            ("cacheBehaviors" .=) <$> _cdCacheBehaviors,
            ("tags" .=) <$> _cdTags,
            Just ("distributionName" .= _cdDistributionName),
            Just ("origin" .= _cdOrigin),
            Just ("defaultCacheBehavior" .= _cdDefaultCacheBehavior),
            Just ("bundleId" .= _cdBundleId)
          ]
      )

instance ToPath CreateDistribution where
  toPath = const "/"

instance ToQuery CreateDistribution where
  toQuery = const mempty

-- | /See:/ 'createDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { _crersDistribution ::
      !(Maybe LightsailDistribution),
    _crersOperation :: !(Maybe Operation),
    _crersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersDistribution' - An object that describes the distribution created.
--
-- * 'crersOperation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'crersResponseStatus' - -- | The response status code.
createDistributionResponse ::
  -- | 'crersResponseStatus'
  Int ->
  CreateDistributionResponse
createDistributionResponse pResponseStatus_ =
  CreateDistributionResponse'
    { _crersDistribution = Nothing,
      _crersOperation = Nothing,
      _crersResponseStatus = pResponseStatus_
    }

-- | An object that describes the distribution created.
crersDistribution :: Lens' CreateDistributionResponse (Maybe LightsailDistribution)
crersDistribution = lens _crersDistribution (\s a -> s {_crersDistribution = a})

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
crersOperation :: Lens' CreateDistributionResponse (Maybe Operation)
crersOperation = lens _crersOperation (\s a -> s {_crersOperation = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateDistributionResponse Int
crersResponseStatus = lens _crersResponseStatus (\s a -> s {_crersResponseStatus = a})

instance NFData CreateDistributionResponse
