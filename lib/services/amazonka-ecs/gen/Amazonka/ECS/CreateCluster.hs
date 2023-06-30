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
-- Module      : Amazonka.ECS.CreateCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ECS cluster. By default, your account receives a
-- @default@ cluster when you launch your first container instance.
-- However, you can create your own cluster with a unique name with the
-- @CreateCluster@ action.
--
-- When you call the CreateCluster API operation, Amazon ECS attempts to
-- create the Amazon ECS service-linked role for your account. This is so
-- that it can manage required resources in other Amazon Web Services
-- services on your behalf. However, if the IAM user that makes the call
-- doesn\'t have permissions to create the service-linked role, it isn\'t
-- created. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_capacityProviders,
    createCluster_clusterName,
    createCluster_configuration,
    createCluster_defaultCapacityProviderStrategy,
    createCluster_serviceConnectDefaults,
    createCluster_settings,
    createCluster_tags,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The short name of one or more capacity providers to associate with the
    -- cluster. A capacity provider must be associated with a cluster before it
    -- can be included as part of the default capacity provider strategy of the
    -- cluster or used in a capacity provider strategy when calling the
    -- CreateService or RunTask actions.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must be created but not associated with another
    -- cluster. New Auto Scaling group capacity providers can be created with
    -- the CreateCapacityProvider API operation.
    --
    -- To use a Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
    -- available to all accounts and only need to be associated with a cluster
    -- to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The name of your cluster. If you don\'t specify a name for your cluster,
    -- you create a cluster that\'s named @default@. Up to 255 letters
    -- (uppercase and lowercase), numbers, underscores, and hyphens are
    -- allowed.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The @execute@ command configuration for the cluster.
    configuration :: Prelude.Maybe ClusterConfiguration,
    -- | The capacity provider strategy to set as the default for the cluster.
    -- After a default capacity provider strategy is set for a cluster, when
    -- you call the RunTask or CreateService APIs with no capacity provider
    -- strategy or launch type specified, the default capacity provider
    -- strategy for the cluster is used.
    --
    -- If a default capacity provider strategy isn\'t defined for a cluster
    -- when it was created, it can be defined later with the
    -- PutClusterCapacityProviders API operation.
    defaultCapacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | Use this parameter to set a default Service Connect namespace. After you
    -- set a default Service Connect namespace, any new services with Service
    -- Connect turned on that are created in the cluster are added as client
    -- services in the namespace. This setting only applies to new services
    -- that set the @enabled@ parameter to @true@ in the
    -- @ServiceConnectConfiguration@. You can set the namespace of each service
    -- individually in the @ServiceConnectConfiguration@ to override this
    -- default parameter.
    --
    -- Tasks that run in a namespace can use short names to connect to services
    -- in the namespace. Tasks can connect to services across all of the
    -- clusters in the namespace. Tasks connect through a managed proxy
    -- container that collects logs and metrics for increased visibility. Only
    -- the tasks that Amazon ECS services create are supported with Service
    -- Connect. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    serviceConnectDefaults :: Prelude.Maybe ClusterServiceConnectDefaultsRequest,
    -- | The setting to use when creating a cluster. This parameter is used to
    -- turn on CloudWatch Container Insights for a cluster. If this value is
    -- specified, it overrides the @containerInsights@ value set with
    -- PutAccountSetting or PutAccountSettingDefault.
    settings :: Prelude.Maybe [ClusterSetting],
    -- | The metadata that you apply to the cluster to help you categorize and
    -- organize them. Each tag consists of a key and an optional value. You
    -- define both.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProviders', 'createCluster_capacityProviders' - The short name of one or more capacity providers to associate with the
-- cluster. A capacity provider must be associated with a cluster before it
-- can be included as part of the default capacity provider strategy of the
-- cluster or used in a capacity provider strategy when calling the
-- CreateService or RunTask actions.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must be created but not associated with another
-- cluster. New Auto Scaling group capacity providers can be created with
-- the CreateCapacityProvider API operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'clusterName', 'createCluster_clusterName' - The name of your cluster. If you don\'t specify a name for your cluster,
-- you create a cluster that\'s named @default@. Up to 255 letters
-- (uppercase and lowercase), numbers, underscores, and hyphens are
-- allowed.
--
-- 'configuration', 'createCluster_configuration' - The @execute@ command configuration for the cluster.
--
-- 'defaultCapacityProviderStrategy', 'createCluster_defaultCapacityProviderStrategy' - The capacity provider strategy to set as the default for the cluster.
-- After a default capacity provider strategy is set for a cluster, when
-- you call the RunTask or CreateService APIs with no capacity provider
-- strategy or launch type specified, the default capacity provider
-- strategy for the cluster is used.
--
-- If a default capacity provider strategy isn\'t defined for a cluster
-- when it was created, it can be defined later with the
-- PutClusterCapacityProviders API operation.
--
-- 'serviceConnectDefaults', 'createCluster_serviceConnectDefaults' - Use this parameter to set a default Service Connect namespace. After you
-- set a default Service Connect namespace, any new services with Service
-- Connect turned on that are created in the cluster are added as client
-- services in the namespace. This setting only applies to new services
-- that set the @enabled@ parameter to @true@ in the
-- @ServiceConnectConfiguration@. You can set the namespace of each service
-- individually in the @ServiceConnectConfiguration@ to override this
-- default parameter.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'settings', 'createCluster_settings' - The setting to use when creating a cluster. This parameter is used to
-- turn on CloudWatch Container Insights for a cluster. If this value is
-- specified, it overrides the @containerInsights@ value set with
-- PutAccountSetting or PutAccountSettingDefault.
--
-- 'tags', 'createCluster_tags' - The metadata that you apply to the cluster to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
newCreateCluster ::
  CreateCluster
newCreateCluster =
  CreateCluster'
    { capacityProviders = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      configuration = Prelude.Nothing,
      defaultCapacityProviderStrategy = Prelude.Nothing,
      serviceConnectDefaults = Prelude.Nothing,
      settings = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The short name of one or more capacity providers to associate with the
-- cluster. A capacity provider must be associated with a cluster before it
-- can be included as part of the default capacity provider strategy of the
-- cluster or used in a capacity provider strategy when calling the
-- CreateService or RunTask actions.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must be created but not associated with another
-- cluster. New Auto Scaling group capacity providers can be created with
-- the CreateCapacityProvider API operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
createCluster_capacityProviders :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_capacityProviders = Lens.lens (\CreateCluster' {capacityProviders} -> capacityProviders) (\s@CreateCluster' {} a -> s {capacityProviders = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The name of your cluster. If you don\'t specify a name for your cluster,
-- you create a cluster that\'s named @default@. Up to 255 letters
-- (uppercase and lowercase), numbers, underscores, and hyphens are
-- allowed.
createCluster_clusterName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clusterName = Lens.lens (\CreateCluster' {clusterName} -> clusterName) (\s@CreateCluster' {} a -> s {clusterName = a} :: CreateCluster)

-- | The @execute@ command configuration for the cluster.
createCluster_configuration :: Lens.Lens' CreateCluster (Prelude.Maybe ClusterConfiguration)
createCluster_configuration = Lens.lens (\CreateCluster' {configuration} -> configuration) (\s@CreateCluster' {} a -> s {configuration = a} :: CreateCluster)

-- | The capacity provider strategy to set as the default for the cluster.
-- After a default capacity provider strategy is set for a cluster, when
-- you call the RunTask or CreateService APIs with no capacity provider
-- strategy or launch type specified, the default capacity provider
-- strategy for the cluster is used.
--
-- If a default capacity provider strategy isn\'t defined for a cluster
-- when it was created, it can be defined later with the
-- PutClusterCapacityProviders API operation.
createCluster_defaultCapacityProviderStrategy :: Lens.Lens' CreateCluster (Prelude.Maybe [CapacityProviderStrategyItem])
createCluster_defaultCapacityProviderStrategy = Lens.lens (\CreateCluster' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@CreateCluster' {} a -> s {defaultCapacityProviderStrategy = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter to set a default Service Connect namespace. After you
-- set a default Service Connect namespace, any new services with Service
-- Connect turned on that are created in the cluster are added as client
-- services in the namespace. This setting only applies to new services
-- that set the @enabled@ parameter to @true@ in the
-- @ServiceConnectConfiguration@. You can set the namespace of each service
-- individually in the @ServiceConnectConfiguration@ to override this
-- default parameter.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
createCluster_serviceConnectDefaults :: Lens.Lens' CreateCluster (Prelude.Maybe ClusterServiceConnectDefaultsRequest)
createCluster_serviceConnectDefaults = Lens.lens (\CreateCluster' {serviceConnectDefaults} -> serviceConnectDefaults) (\s@CreateCluster' {} a -> s {serviceConnectDefaults = a} :: CreateCluster)

-- | The setting to use when creating a cluster. This parameter is used to
-- turn on CloudWatch Container Insights for a cluster. If this value is
-- specified, it overrides the @containerInsights@ value set with
-- PutAccountSetting or PutAccountSettingDefault.
createCluster_settings :: Lens.Lens' CreateCluster (Prelude.Maybe [ClusterSetting])
createCluster_settings = Lens.lens (\CreateCluster' {settings} -> settings) (\s@CreateCluster' {} a -> s {settings = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The metadata that you apply to the cluster to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe [Tag])
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Data..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviders
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` defaultCapacityProviderStrategy
      `Prelude.hashWithSalt` serviceConnectDefaults
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf defaultCapacityProviderStrategy
      `Prelude.seq` Prelude.rnf serviceConnectDefaults
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.CreateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capacityProviders" Data..=)
              Prelude.<$> capacityProviders,
            ("clusterName" Data..=) Prelude.<$> clusterName,
            ("configuration" Data..=) Prelude.<$> configuration,
            ("defaultCapacityProviderStrategy" Data..=)
              Prelude.<$> defaultCapacityProviderStrategy,
            ("serviceConnectDefaults" Data..=)
              Prelude.<$> serviceConnectDefaults,
            ("settings" Data..=) Prelude.<$> settings,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The full description of your new cluster.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'createClusterResponse_cluster' - The full description of your new cluster.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new cluster.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
