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
-- Module      : Network.AWS.Route53AutoNaming.DiscoverInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Discovers registered instances for a specified namespace and service. You can use @DiscoverInstances@ to discover instances for any type of namespace. For public and private DNS namespaces, you can also use DNS queries to discover instances.
module Network.AWS.Route53AutoNaming.DiscoverInstances
  ( -- * Creating a Request
    discoverInstances,
    DiscoverInstances,

    -- * Request Lenses
    diQueryParameters,
    diOptionalParameters,
    diHealthStatus,
    diMaxResults,
    diNamespaceName,
    diServiceName,

    -- * Destructuring the Response
    discoverInstancesResponse,
    DiscoverInstancesResponse,

    -- * Response Lenses
    dirsInstances,
    dirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'discoverInstances' smart constructor.
data DiscoverInstances = DiscoverInstances'
  { _diQueryParameters ::
      !(Maybe (Map Text (Text))),
    _diOptionalParameters :: !(Maybe (Map Text (Text))),
    _diHealthStatus :: !(Maybe HealthStatusFilter),
    _diMaxResults :: !(Maybe Nat),
    _diNamespaceName :: !Text,
    _diServiceName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiscoverInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diQueryParameters' - Filters to scope the results based on custom attributes for the instance. For example, @{version=v1, az=1a}@ . Only instances that match all the specified key-value pairs will be returned.
--
-- * 'diOptionalParameters' - Opportunistic filters to scope the results based on custom attributes. If there are instances that match both the filters specified in both the @QueryParameters@ parameter and this parameter, they are returned. Otherwise, these filters are ignored and only instances that match the filters specified in the @QueryParameters@ parameter are returned.
--
-- * 'diHealthStatus' - The health status of the instances that you want to discover.
--
-- * 'diMaxResults' - The maximum number of instances that you want AWS Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- * 'diNamespaceName' - The name of the namespace that you specified when you registered the instance.
--
-- * 'diServiceName' - The name of the service that you specified when you registered the instance.
discoverInstances ::
  -- | 'diNamespaceName'
  Text ->
  -- | 'diServiceName'
  Text ->
  DiscoverInstances
discoverInstances pNamespaceName_ pServiceName_ =
  DiscoverInstances'
    { _diQueryParameters = Nothing,
      _diOptionalParameters = Nothing,
      _diHealthStatus = Nothing,
      _diMaxResults = Nothing,
      _diNamespaceName = pNamespaceName_,
      _diServiceName = pServiceName_
    }

-- | Filters to scope the results based on custom attributes for the instance. For example, @{version=v1, az=1a}@ . Only instances that match all the specified key-value pairs will be returned.
diQueryParameters :: Lens' DiscoverInstances (HashMap Text (Text))
diQueryParameters = lens _diQueryParameters (\s a -> s {_diQueryParameters = a}) . _Default . _Map

-- | Opportunistic filters to scope the results based on custom attributes. If there are instances that match both the filters specified in both the @QueryParameters@ parameter and this parameter, they are returned. Otherwise, these filters are ignored and only instances that match the filters specified in the @QueryParameters@ parameter are returned.
diOptionalParameters :: Lens' DiscoverInstances (HashMap Text (Text))
diOptionalParameters = lens _diOptionalParameters (\s a -> s {_diOptionalParameters = a}) . _Default . _Map

-- | The health status of the instances that you want to discover.
diHealthStatus :: Lens' DiscoverInstances (Maybe HealthStatusFilter)
diHealthStatus = lens _diHealthStatus (\s a -> s {_diHealthStatus = a})

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
diMaxResults :: Lens' DiscoverInstances (Maybe Natural)
diMaxResults = lens _diMaxResults (\s a -> s {_diMaxResults = a}) . mapping _Nat

-- | The name of the namespace that you specified when you registered the instance.
diNamespaceName :: Lens' DiscoverInstances Text
diNamespaceName = lens _diNamespaceName (\s a -> s {_diNamespaceName = a})

-- | The name of the service that you specified when you registered the instance.
diServiceName :: Lens' DiscoverInstances Text
diServiceName = lens _diServiceName (\s a -> s {_diServiceName = a})

instance AWSRequest DiscoverInstances where
  type Rs DiscoverInstances = DiscoverInstancesResponse
  request = postJSON route53AutoNaming
  response =
    receiveJSON
      ( \s h x ->
          DiscoverInstancesResponse'
            <$> (x .?> "Instances" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DiscoverInstances

instance NFData DiscoverInstances

instance ToHeaders DiscoverInstances where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Route53AutoNaming_v20170314.DiscoverInstances" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DiscoverInstances where
  toJSON DiscoverInstances' {..} =
    object
      ( catMaybes
          [ ("QueryParameters" .=) <$> _diQueryParameters,
            ("OptionalParameters" .=) <$> _diOptionalParameters,
            ("HealthStatus" .=) <$> _diHealthStatus,
            ("MaxResults" .=) <$> _diMaxResults,
            Just ("NamespaceName" .= _diNamespaceName),
            Just ("ServiceName" .= _diServiceName)
          ]
      )

instance ToPath DiscoverInstances where
  toPath = const "/"

instance ToQuery DiscoverInstances where
  toQuery = const mempty

-- | /See:/ 'discoverInstancesResponse' smart constructor.
data DiscoverInstancesResponse = DiscoverInstancesResponse'
  { _dirsInstances ::
      !(Maybe [HTTPInstanceSummary]),
    _dirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiscoverInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsInstances' - A complex type that contains one @HttpInstanceSummary@ for each registered instance.
--
-- * 'dirsResponseStatus' - -- | The response status code.
discoverInstancesResponse ::
  -- | 'dirsResponseStatus'
  Int ->
  DiscoverInstancesResponse
discoverInstancesResponse pResponseStatus_ =
  DiscoverInstancesResponse'
    { _dirsInstances = Nothing,
      _dirsResponseStatus = pResponseStatus_
    }

-- | A complex type that contains one @HttpInstanceSummary@ for each registered instance.
dirsInstances :: Lens' DiscoverInstancesResponse [HTTPInstanceSummary]
dirsInstances = lens _dirsInstances (\s a -> s {_dirsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DiscoverInstancesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\s a -> s {_dirsResponseStatus = a})

instance NFData DiscoverInstancesResponse
