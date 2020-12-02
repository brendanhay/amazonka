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
-- Module      : Network.AWS.ECS.PutClusterCapacityProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the available capacity providers and the default capacity provider strategy for a cluster.
--
--
-- You must specify both the available capacity providers and a default capacity provider strategy for the cluster. If the specified cluster has existing capacity providers associated with it, you must specify all existing capacity providers in addition to any new ones you want to add. Any existing capacity providers associated with a cluster that are omitted from a 'PutClusterCapacityProviders' API call will be disassociated with the cluster. You can only disassociate an existing capacity provider from a cluster if it's not being used by any existing tasks.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified, then the cluster's default capacity provider strategy is used. It is recommended to define a default capacity provider strategy for your cluster, however you may specify an empty array (@[]@ ) to bypass defining a default strategy.
module Network.AWS.ECS.PutClusterCapacityProviders
  ( -- * Creating a Request
    putClusterCapacityProviders,
    PutClusterCapacityProviders,

    -- * Request Lenses
    pccpCluster,
    pccpCapacityProviders,
    pccpDefaultCapacityProviderStrategy,

    -- * Destructuring the Response
    putClusterCapacityProvidersResponse,
    PutClusterCapacityProvidersResponse,

    -- * Response Lenses
    pccprsCluster,
    pccprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putClusterCapacityProviders' smart constructor.
data PutClusterCapacityProviders = PutClusterCapacityProviders'
  { _pccpCluster ::
      !Text,
    _pccpCapacityProviders :: ![Text],
    _pccpDefaultCapacityProviderStrategy ::
      ![CapacityProviderStrategyItem]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutClusterCapacityProviders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pccpCluster' - The short name or full Amazon Resource Name (ARN) of the cluster to modify the capacity provider settings for. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'pccpCapacityProviders' - The name of one or more capacity providers to associate with the cluster. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
--
-- * 'pccpDefaultCapacityProviderStrategy' - The capacity provider strategy to use by default for the cluster. When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used. A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
putClusterCapacityProviders ::
  -- | 'pccpCluster'
  Text ->
  PutClusterCapacityProviders
putClusterCapacityProviders pCluster_ =
  PutClusterCapacityProviders'
    { _pccpCluster = pCluster_,
      _pccpCapacityProviders = mempty,
      _pccpDefaultCapacityProviderStrategy = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster to modify the capacity provider settings for. If you do not specify a cluster, the default cluster is assumed.
pccpCluster :: Lens' PutClusterCapacityProviders Text
pccpCluster = lens _pccpCluster (\s a -> s {_pccpCluster = a})

-- | The name of one or more capacity providers to associate with the cluster. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
pccpCapacityProviders :: Lens' PutClusterCapacityProviders [Text]
pccpCapacityProviders = lens _pccpCapacityProviders (\s a -> s {_pccpCapacityProviders = a}) . _Coerce

-- | The capacity provider strategy to use by default for the cluster. When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used. A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
pccpDefaultCapacityProviderStrategy :: Lens' PutClusterCapacityProviders [CapacityProviderStrategyItem]
pccpDefaultCapacityProviderStrategy = lens _pccpDefaultCapacityProviderStrategy (\s a -> s {_pccpDefaultCapacityProviderStrategy = a}) . _Coerce

instance AWSRequest PutClusterCapacityProviders where
  type
    Rs PutClusterCapacityProviders =
      PutClusterCapacityProvidersResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          PutClusterCapacityProvidersResponse'
            <$> (x .?> "cluster") <*> (pure (fromEnum s))
      )

instance Hashable PutClusterCapacityProviders

instance NFData PutClusterCapacityProviders

instance ToHeaders PutClusterCapacityProviders where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.PutClusterCapacityProviders" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutClusterCapacityProviders where
  toJSON PutClusterCapacityProviders' {..} =
    object
      ( catMaybes
          [ Just ("cluster" .= _pccpCluster),
            Just ("capacityProviders" .= _pccpCapacityProviders),
            Just
              ( "defaultCapacityProviderStrategy"
                  .= _pccpDefaultCapacityProviderStrategy
              )
          ]
      )

instance ToPath PutClusterCapacityProviders where
  toPath = const "/"

instance ToQuery PutClusterCapacityProviders where
  toQuery = const mempty

-- | /See:/ 'putClusterCapacityProvidersResponse' smart constructor.
data PutClusterCapacityProvidersResponse = PutClusterCapacityProvidersResponse'
  { _pccprsCluster ::
      !(Maybe Cluster),
    _pccprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutClusterCapacityProvidersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pccprsCluster' - Undocumented member.
--
-- * 'pccprsResponseStatus' - -- | The response status code.
putClusterCapacityProvidersResponse ::
  -- | 'pccprsResponseStatus'
  Int ->
  PutClusterCapacityProvidersResponse
putClusterCapacityProvidersResponse pResponseStatus_ =
  PutClusterCapacityProvidersResponse'
    { _pccprsCluster = Nothing,
      _pccprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
pccprsCluster :: Lens' PutClusterCapacityProvidersResponse (Maybe Cluster)
pccprsCluster = lens _pccprsCluster (\s a -> s {_pccprsCluster = a})

-- | -- | The response status code.
pccprsResponseStatus :: Lens' PutClusterCapacityProvidersResponse Int
pccprsResponseStatus = lens _pccprsResponseStatus (\s a -> s {_pccprsResponseStatus = a})

instance NFData PutClusterCapacityProvidersResponse
