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
-- Module      : Network.AWS.ECS.CreateCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- create the Amazon ECS service-linked role for your account so that
-- required resources in other AWS services can be managed on your behalf.
-- However, if the IAM user that makes the call does not have permissions
-- to create the service-linked role, it is not created. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_defaultCapacityProviderStrategy,
    createCluster_tags,
    createCluster_capacityProviders,
    createCluster_clusterName,
    createCluster_settings,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The capacity provider strategy to use by default for the cluster.
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
    --
    -- If a default capacity provider strategy is not defined for a cluster
    -- during creation, it can be defined later with the
    -- PutClusterCapacityProviders API operation.
    defaultCapacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The metadata that you apply to the cluster to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define.
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
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Core.Maybe [Tag],
    -- | The short name of one or more capacity providers to associate with the
    -- cluster.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created and not already associated
    -- with another cluster. New capacity providers can be created with the
    -- CreateCapacityProvider API operation.
    --
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
    -- are available to all accounts and only need to be associated with a
    -- cluster to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviders :: Core.Maybe [Core.Text],
    -- | The name of your cluster. If you do not specify a name for your cluster,
    -- you create a cluster named @default@. Up to 255 letters (uppercase and
    -- lowercase), numbers, and hyphens are allowed.
    clusterName :: Core.Maybe Core.Text,
    -- | The setting to use when creating a cluster. This parameter is used to
    -- enable CloudWatch Container Insights for a cluster. If this value is
    -- specified, it will override the @containerInsights@ value set with
    -- PutAccountSetting or PutAccountSettingDefault.
    settings :: Core.Maybe [ClusterSetting]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultCapacityProviderStrategy', 'createCluster_defaultCapacityProviderStrategy' - The capacity provider strategy to use by default for the cluster.
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
--
-- If a default capacity provider strategy is not defined for a cluster
-- during creation, it can be defined later with the
-- PutClusterCapacityProviders API operation.
--
-- 'tags', 'createCluster_tags' - The metadata that you apply to the cluster to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'capacityProviders', 'createCluster_capacityProviders' - The short name of one or more capacity providers to associate with the
-- cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created and not already associated
-- with another cluster. New capacity providers can be created with the
-- CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'clusterName', 'createCluster_clusterName' - The name of your cluster. If you do not specify a name for your cluster,
-- you create a cluster named @default@. Up to 255 letters (uppercase and
-- lowercase), numbers, and hyphens are allowed.
--
-- 'settings', 'createCluster_settings' - The setting to use when creating a cluster. This parameter is used to
-- enable CloudWatch Container Insights for a cluster. If this value is
-- specified, it will override the @containerInsights@ value set with
-- PutAccountSetting or PutAccountSettingDefault.
newCreateCluster ::
  CreateCluster
newCreateCluster =
  CreateCluster'
    { defaultCapacityProviderStrategy =
        Core.Nothing,
      tags = Core.Nothing,
      capacityProviders = Core.Nothing,
      clusterName = Core.Nothing,
      settings = Core.Nothing
    }

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
--
-- If a default capacity provider strategy is not defined for a cluster
-- during creation, it can be defined later with the
-- PutClusterCapacityProviders API operation.
createCluster_defaultCapacityProviderStrategy :: Lens.Lens' CreateCluster (Core.Maybe [CapacityProviderStrategyItem])
createCluster_defaultCapacityProviderStrategy = Lens.lens (\CreateCluster' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@CreateCluster' {} a -> s {defaultCapacityProviderStrategy = a} :: CreateCluster) Core.. Lens.mapping Lens._Coerce

-- | The metadata that you apply to the cluster to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
createCluster_tags :: Lens.Lens' CreateCluster (Core.Maybe [Tag])
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Core.. Lens.mapping Lens._Coerce

-- | The short name of one or more capacity providers to associate with the
-- cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created and not already associated
-- with another cluster. New capacity providers can be created with the
-- CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
createCluster_capacityProviders :: Lens.Lens' CreateCluster (Core.Maybe [Core.Text])
createCluster_capacityProviders = Lens.lens (\CreateCluster' {capacityProviders} -> capacityProviders) (\s@CreateCluster' {} a -> s {capacityProviders = a} :: CreateCluster) Core.. Lens.mapping Lens._Coerce

-- | The name of your cluster. If you do not specify a name for your cluster,
-- you create a cluster named @default@. Up to 255 letters (uppercase and
-- lowercase), numbers, and hyphens are allowed.
createCluster_clusterName :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
createCluster_clusterName = Lens.lens (\CreateCluster' {clusterName} -> clusterName) (\s@CreateCluster' {} a -> s {clusterName = a} :: CreateCluster)

-- | The setting to use when creating a cluster. This parameter is used to
-- enable CloudWatch Container Insights for a cluster. If this value is
-- specified, it will override the @containerInsights@ value set with
-- PutAccountSetting or PutAccountSettingDefault.
createCluster_settings :: Lens.Lens' CreateCluster (Core.Maybe [ClusterSetting])
createCluster_settings = Lens.lens (\CreateCluster' {settings} -> settings) (\s@CreateCluster' {} a -> s {settings = a} :: CreateCluster) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Core.<$> (x Core..?> "cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCluster

instance Core.NFData CreateCluster

instance Core.ToHeaders CreateCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.CreateCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Core.object
      ( Core.catMaybes
          [ ("defaultCapacityProviderStrategy" Core..=)
              Core.<$> defaultCapacityProviderStrategy,
            ("tags" Core..=) Core.<$> tags,
            ("capacityProviders" Core..=)
              Core.<$> capacityProviders,
            ("clusterName" Core..=) Core.<$> clusterName,
            ("settings" Core..=) Core.<$> settings
          ]
      )

instance Core.ToPath CreateCluster where
  toPath = Core.const "/"

instance Core.ToQuery CreateCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The full description of your new cluster.
    cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new cluster.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Core.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Core.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Core.NFData CreateClusterResponse
