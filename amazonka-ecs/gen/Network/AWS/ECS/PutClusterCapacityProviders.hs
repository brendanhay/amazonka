{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutClusterCapacityProviders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the available capacity providers and the default capacity
-- provider strategy for a cluster.
--
-- You must specify both the available capacity providers and a default
-- capacity provider strategy for the cluster. If the specified cluster has
-- existing capacity providers associated with it, you must specify all
-- existing capacity providers in addition to any new ones you want to add.
-- Any existing capacity providers associated with a cluster that are
-- omitted from a PutClusterCapacityProviders API call will be
-- disassociated with the cluster. You can only disassociate an existing
-- capacity provider from a cluster if it\'s not being used by any existing
-- tasks.
--
-- When creating a service or running a task on a cluster, if no capacity
-- provider or launch type is specified, then the cluster\'s default
-- capacity provider strategy is used. It is recommended to define a
-- default capacity provider strategy for your cluster, however you may
-- specify an empty array (@[]@) to bypass defining a default strategy.
module Network.AWS.ECS.PutClusterCapacityProviders
  ( -- * Creating a Request
    PutClusterCapacityProviders (..),
    newPutClusterCapacityProviders,

    -- * Request Lenses
    putClusterCapacityProviders_cluster,
    putClusterCapacityProviders_capacityProviders,
    putClusterCapacityProviders_defaultCapacityProviderStrategy,

    -- * Destructuring the Response
    PutClusterCapacityProvidersResponse (..),
    newPutClusterCapacityProvidersResponse,

    -- * Response Lenses
    putClusterCapacityProvidersResponse_cluster,
    putClusterCapacityProvidersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutClusterCapacityProviders' smart constructor.
data PutClusterCapacityProviders = PutClusterCapacityProviders'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster to
    -- modify the capacity provider settings for. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Core.Text,
    -- | The name of one or more capacity providers to associate with the
    -- cluster.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created. New capacity providers can be
    -- created with the CreateCapacityProvider API operation.
    --
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
    -- are available to all accounts and only need to be associated with a
    -- cluster to be used.
    capacityProviders :: [Core.Text],
    -- | The capacity provider strategy to use by default for the cluster.
    --
    -- When creating a service or running a task on a cluster, if no capacity
    -- provider or launch type is specified then the default capacity provider
    -- strategy for the cluster is used.
    --
    -- A capacity provider strategy consists of one or more capacity providers
    -- along with the @base@ and @weight@ to assign to them. A capacity
    -- provider must be associated with the cluster to be used in a capacity
    -- provider strategy. The PutClusterCapacityProviders API is used to
    -- associate a capacity provider with a cluster. Only capacity providers
    -- with an @ACTIVE@ or @UPDATING@ status can be used.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created. New capacity providers can be
    -- created with the CreateCapacityProvider API operation.
    --
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
    -- are available to all accounts and only need to be associated with a
    -- cluster to be used.
    defaultCapacityProviderStrategy :: [CapacityProviderStrategyItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutClusterCapacityProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'putClusterCapacityProviders_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to
-- modify the capacity provider settings for. If you do not specify a
-- cluster, the default cluster is assumed.
--
-- 'capacityProviders', 'putClusterCapacityProviders_capacityProviders' - The name of one or more capacity providers to associate with the
-- cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- 'defaultCapacityProviderStrategy', 'putClusterCapacityProviders_defaultCapacityProviderStrategy' - The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity
-- provider or launch type is specified then the default capacity provider
-- strategy for the cluster is used.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
newPutClusterCapacityProviders ::
  -- | 'cluster'
  Core.Text ->
  PutClusterCapacityProviders
newPutClusterCapacityProviders pCluster_ =
  PutClusterCapacityProviders'
    { cluster = pCluster_,
      capacityProviders = Core.mempty,
      defaultCapacityProviderStrategy = Core.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster to
-- modify the capacity provider settings for. If you do not specify a
-- cluster, the default cluster is assumed.
putClusterCapacityProviders_cluster :: Lens.Lens' PutClusterCapacityProviders Core.Text
putClusterCapacityProviders_cluster = Lens.lens (\PutClusterCapacityProviders' {cluster} -> cluster) (\s@PutClusterCapacityProviders' {} a -> s {cluster = a} :: PutClusterCapacityProviders)

-- | The name of one or more capacity providers to associate with the
-- cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
putClusterCapacityProviders_capacityProviders :: Lens.Lens' PutClusterCapacityProviders [Core.Text]
putClusterCapacityProviders_capacityProviders = Lens.lens (\PutClusterCapacityProviders' {capacityProviders} -> capacityProviders) (\s@PutClusterCapacityProviders' {} a -> s {capacityProviders = a} :: PutClusterCapacityProviders) Core.. Lens._Coerce

-- | The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity
-- provider or launch type is specified then the default capacity provider
-- strategy for the cluster is used.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
putClusterCapacityProviders_defaultCapacityProviderStrategy :: Lens.Lens' PutClusterCapacityProviders [CapacityProviderStrategyItem]
putClusterCapacityProviders_defaultCapacityProviderStrategy = Lens.lens (\PutClusterCapacityProviders' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@PutClusterCapacityProviders' {} a -> s {defaultCapacityProviderStrategy = a} :: PutClusterCapacityProviders) Core.. Lens._Coerce

instance Core.AWSRequest PutClusterCapacityProviders where
  type
    AWSResponse PutClusterCapacityProviders =
      PutClusterCapacityProvidersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutClusterCapacityProvidersResponse'
            Core.<$> (x Core..?> "cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutClusterCapacityProviders

instance Core.NFData PutClusterCapacityProviders

instance Core.ToHeaders PutClusterCapacityProviders where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.PutClusterCapacityProviders" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutClusterCapacityProviders where
  toJSON PutClusterCapacityProviders' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just
              ("capacityProviders" Core..= capacityProviders),
            Core.Just
              ( "defaultCapacityProviderStrategy"
                  Core..= defaultCapacityProviderStrategy
              )
          ]
      )

instance Core.ToPath PutClusterCapacityProviders where
  toPath = Core.const "/"

instance Core.ToQuery PutClusterCapacityProviders where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutClusterCapacityProvidersResponse' smart constructor.
data PutClusterCapacityProvidersResponse = PutClusterCapacityProvidersResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutClusterCapacityProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'putClusterCapacityProvidersResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'putClusterCapacityProvidersResponse_httpStatus' - The response's http status code.
newPutClusterCapacityProvidersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutClusterCapacityProvidersResponse
newPutClusterCapacityProvidersResponse pHttpStatus_ =
  PutClusterCapacityProvidersResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putClusterCapacityProvidersResponse_cluster :: Lens.Lens' PutClusterCapacityProvidersResponse (Core.Maybe Cluster)
putClusterCapacityProvidersResponse_cluster = Lens.lens (\PutClusterCapacityProvidersResponse' {cluster} -> cluster) (\s@PutClusterCapacityProvidersResponse' {} a -> s {cluster = a} :: PutClusterCapacityProvidersResponse)

-- | The response's http status code.
putClusterCapacityProvidersResponse_httpStatus :: Lens.Lens' PutClusterCapacityProvidersResponse Core.Int
putClusterCapacityProvidersResponse_httpStatus = Lens.lens (\PutClusterCapacityProvidersResponse' {httpStatus} -> httpStatus) (\s@PutClusterCapacityProvidersResponse' {} a -> s {httpStatus = a} :: PutClusterCapacityProvidersResponse)

instance
  Core.NFData
    PutClusterCapacityProvidersResponse
