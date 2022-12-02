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
-- Module      : Amazonka.ECS.PutClusterCapacityProviders
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Any existing capacity providers that are associated with a cluster that
-- are omitted from a PutClusterCapacityProviders API call will be
-- disassociated with the cluster. You can only disassociate an existing
-- capacity provider from a cluster if it\'s not being used by any existing
-- tasks.
--
-- When creating a service or running a task on a cluster, if no capacity
-- provider or launch type is specified, then the cluster\'s default
-- capacity provider strategy is used. We recommend that you define a
-- default capacity provider strategy for your cluster. However, you must
-- specify an empty array (@[]@) to bypass defining a default strategy.
module Amazonka.ECS.PutClusterCapacityProviders
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutClusterCapacityProviders' smart constructor.
data PutClusterCapacityProviders = PutClusterCapacityProviders'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster to
    -- modify the capacity provider settings for. If you don\'t specify a
    -- cluster, the default cluster is assumed.
    cluster :: Prelude.Text,
    -- | The name of one or more capacity providers to associate with the
    -- cluster.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created. New capacity providers can be
    -- created with the CreateCapacityProvider API operation.
    --
    -- To use a Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
    -- available to all accounts and only need to be associated with a cluster
    -- to be used.
    capacityProviders :: [Prelude.Text],
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
    -- To use a Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
    -- available to all accounts and only need to be associated with a cluster
    -- to be used.
    defaultCapacityProviderStrategy :: [CapacityProviderStrategyItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutClusterCapacityProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'putClusterCapacityProviders_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to
-- modify the capacity provider settings for. If you don\'t specify a
-- cluster, the default cluster is assumed.
--
-- 'capacityProviders', 'putClusterCapacityProviders_capacityProviders' - The name of one or more capacity providers to associate with the
-- cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
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
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
newPutClusterCapacityProviders ::
  -- | 'cluster'
  Prelude.Text ->
  PutClusterCapacityProviders
newPutClusterCapacityProviders pCluster_ =
  PutClusterCapacityProviders'
    { cluster = pCluster_,
      capacityProviders = Prelude.mempty,
      defaultCapacityProviderStrategy =
        Prelude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster to
-- modify the capacity provider settings for. If you don\'t specify a
-- cluster, the default cluster is assumed.
putClusterCapacityProviders_cluster :: Lens.Lens' PutClusterCapacityProviders Prelude.Text
putClusterCapacityProviders_cluster = Lens.lens (\PutClusterCapacityProviders' {cluster} -> cluster) (\s@PutClusterCapacityProviders' {} a -> s {cluster = a} :: PutClusterCapacityProviders)

-- | The name of one or more capacity providers to associate with the
-- cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
putClusterCapacityProviders_capacityProviders :: Lens.Lens' PutClusterCapacityProviders [Prelude.Text]
putClusterCapacityProviders_capacityProviders = Lens.lens (\PutClusterCapacityProviders' {capacityProviders} -> capacityProviders) (\s@PutClusterCapacityProviders' {} a -> s {capacityProviders = a} :: PutClusterCapacityProviders) Prelude.. Lens.coerced

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
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
putClusterCapacityProviders_defaultCapacityProviderStrategy :: Lens.Lens' PutClusterCapacityProviders [CapacityProviderStrategyItem]
putClusterCapacityProviders_defaultCapacityProviderStrategy = Lens.lens (\PutClusterCapacityProviders' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@PutClusterCapacityProviders' {} a -> s {defaultCapacityProviderStrategy = a} :: PutClusterCapacityProviders) Prelude.. Lens.coerced

instance Core.AWSRequest PutClusterCapacityProviders where
  type
    AWSResponse PutClusterCapacityProviders =
      PutClusterCapacityProvidersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutClusterCapacityProvidersResponse'
            Prelude.<$> (x Data..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutClusterCapacityProviders where
  hashWithSalt _salt PutClusterCapacityProviders' {..} =
    _salt `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` capacityProviders
      `Prelude.hashWithSalt` defaultCapacityProviderStrategy

instance Prelude.NFData PutClusterCapacityProviders where
  rnf PutClusterCapacityProviders' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf defaultCapacityProviderStrategy

instance Data.ToHeaders PutClusterCapacityProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.PutClusterCapacityProviders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutClusterCapacityProviders where
  toJSON PutClusterCapacityProviders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cluster" Data..= cluster),
            Prelude.Just
              ("capacityProviders" Data..= capacityProviders),
            Prelude.Just
              ( "defaultCapacityProviderStrategy"
                  Data..= defaultCapacityProviderStrategy
              )
          ]
      )

instance Data.ToPath PutClusterCapacityProviders where
  toPath = Prelude.const "/"

instance Data.ToQuery PutClusterCapacityProviders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutClusterCapacityProvidersResponse' smart constructor.
data PutClusterCapacityProvidersResponse = PutClusterCapacityProvidersResponse'
  { -- | Details about the cluster.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutClusterCapacityProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'putClusterCapacityProvidersResponse_cluster' - Details about the cluster.
--
-- 'httpStatus', 'putClusterCapacityProvidersResponse_httpStatus' - The response's http status code.
newPutClusterCapacityProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutClusterCapacityProvidersResponse
newPutClusterCapacityProvidersResponse pHttpStatus_ =
  PutClusterCapacityProvidersResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the cluster.
putClusterCapacityProvidersResponse_cluster :: Lens.Lens' PutClusterCapacityProvidersResponse (Prelude.Maybe Cluster)
putClusterCapacityProvidersResponse_cluster = Lens.lens (\PutClusterCapacityProvidersResponse' {cluster} -> cluster) (\s@PutClusterCapacityProvidersResponse' {} a -> s {cluster = a} :: PutClusterCapacityProvidersResponse)

-- | The response's http status code.
putClusterCapacityProvidersResponse_httpStatus :: Lens.Lens' PutClusterCapacityProvidersResponse Prelude.Int
putClusterCapacityProvidersResponse_httpStatus = Lens.lens (\PutClusterCapacityProvidersResponse' {httpStatus} -> httpStatus) (\s@PutClusterCapacityProvidersResponse' {} a -> s {httpStatus = a} :: PutClusterCapacityProvidersResponse)

instance
  Prelude.NFData
    PutClusterCapacityProvidersResponse
  where
  rnf PutClusterCapacityProvidersResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
