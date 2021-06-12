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
-- Module      : Network.AWS.ECS.CreateTaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a task set in the specified cluster and service. This is used
-- when a service uses the @EXTERNAL@ deployment controller type. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.CreateTaskSet
  ( -- * Creating a Request
    CreateTaskSet (..),
    newCreateTaskSet,

    -- * Request Lenses
    createTaskSet_networkConfiguration,
    createTaskSet_capacityProviderStrategy,
    createTaskSet_launchType,
    createTaskSet_platformVersion,
    createTaskSet_loadBalancers,
    createTaskSet_tags,
    createTaskSet_serviceRegistries,
    createTaskSet_externalId,
    createTaskSet_scale,
    createTaskSet_clientToken,
    createTaskSet_service,
    createTaskSet_cluster,
    createTaskSet_taskDefinition,

    -- * Destructuring the Response
    CreateTaskSetResponse (..),
    newCreateTaskSetResponse,

    -- * Response Lenses
    createTaskSetResponse_taskSet,
    createTaskSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTaskSet' smart constructor.
data CreateTaskSet = CreateTaskSet'
  { networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The capacity provider strategy to use for the task set.
    --
    -- A capacity provider strategy consists of one or more capacity providers
    -- along with the @base@ and @weight@ to assign to them. A capacity
    -- provider must be associated with the cluster to be used in a capacity
    -- provider strategy. The PutClusterCapacityProviders API is used to
    -- associate a capacity provider with a cluster. Only capacity providers
    -- with an @ACTIVE@ or @UPDATING@ status can be used.
    --
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
    -- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
    -- specified, the @defaultCapacityProviderStrategy@ for the cluster is
    -- used.
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
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The launch type that new tasks in the task set will use. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
    -- must be omitted.
    launchType :: Core.Maybe LaunchType,
    -- | The platform version that the tasks in the task set should use. A
    -- platform version is specified only for tasks using the Fargate launch
    -- type. If one isn\'t specified, the @LATEST@ platform version is used by
    -- default.
    platformVersion :: Core.Maybe Core.Text,
    -- | A load balancer object representing the load balancer to use with the
    -- task set. The supported load balancer types are either an Application
    -- Load Balancer or a Network Load Balancer.
    loadBalancers :: Core.Maybe [LoadBalancer],
    -- | The metadata that you apply to the task set to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define. When a service is deleted, the tags are deleted as
    -- well.
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
    -- | The details of the service discovery registries to assign to this task
    -- set. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    serviceRegistries :: Core.Maybe [ServiceRegistry],
    -- | An optional non-unique tag that identifies this task set in external
    -- systems. If the task set is associated with a service discovery
    -- registry, the tasks in this task set will have the
    -- @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided
    -- value.
    externalId :: Core.Maybe Core.Text,
    scale :: Core.Maybe Scale,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 32 ASCII characters are allowed.
    clientToken :: Core.Maybe Core.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service to
    -- create the task set in.
    service :: Core.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service to create the task set in.
    cluster :: Core.Text,
    -- | The task definition for the tasks in the task set to use.
    taskDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfiguration', 'createTaskSet_networkConfiguration' - Undocumented member.
--
-- 'capacityProviderStrategy', 'createTaskSet_capacityProviderStrategy' - The capacity provider strategy to use for the task set.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
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
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'launchType', 'createTaskSet_launchType' - The launch type that new tasks in the task set will use. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- 'platformVersion', 'createTaskSet_platformVersion' - The platform version that the tasks in the task set should use. A
-- platform version is specified only for tasks using the Fargate launch
-- type. If one isn\'t specified, the @LATEST@ platform version is used by
-- default.
--
-- 'loadBalancers', 'createTaskSet_loadBalancers' - A load balancer object representing the load balancer to use with the
-- task set. The supported load balancer types are either an Application
-- Load Balancer or a Network Load Balancer.
--
-- 'tags', 'createTaskSet_tags' - The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. When a service is deleted, the tags are deleted as
-- well.
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
-- 'serviceRegistries', 'createTaskSet_serviceRegistries' - The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- 'externalId', 'createTaskSet_externalId' - An optional non-unique tag that identifies this task set in external
-- systems. If the task set is associated with a service discovery
-- registry, the tasks in this task set will have the
-- @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided
-- value.
--
-- 'scale', 'createTaskSet_scale' - Undocumented member.
--
-- 'clientToken', 'createTaskSet_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- 'service', 'createTaskSet_service' - The short name or full Amazon Resource Name (ARN) of the service to
-- create the task set in.
--
-- 'cluster', 'createTaskSet_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to create the task set in.
--
-- 'taskDefinition', 'createTaskSet_taskDefinition' - The task definition for the tasks in the task set to use.
newCreateTaskSet ::
  -- | 'service'
  Core.Text ->
  -- | 'cluster'
  Core.Text ->
  -- | 'taskDefinition'
  Core.Text ->
  CreateTaskSet
newCreateTaskSet pService_ pCluster_ pTaskDefinition_ =
  CreateTaskSet'
    { networkConfiguration = Core.Nothing,
      capacityProviderStrategy = Core.Nothing,
      launchType = Core.Nothing,
      platformVersion = Core.Nothing,
      loadBalancers = Core.Nothing,
      tags = Core.Nothing,
      serviceRegistries = Core.Nothing,
      externalId = Core.Nothing,
      scale = Core.Nothing,
      clientToken = Core.Nothing,
      service = pService_,
      cluster = pCluster_,
      taskDefinition = pTaskDefinition_
    }

-- | Undocumented member.
createTaskSet_networkConfiguration :: Lens.Lens' CreateTaskSet (Core.Maybe NetworkConfiguration)
createTaskSet_networkConfiguration = Lens.lens (\CreateTaskSet' {networkConfiguration} -> networkConfiguration) (\s@CreateTaskSet' {} a -> s {networkConfiguration = a} :: CreateTaskSet)

-- | The capacity provider strategy to use for the task set.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
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
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
createTaskSet_capacityProviderStrategy :: Lens.Lens' CreateTaskSet (Core.Maybe [CapacityProviderStrategyItem])
createTaskSet_capacityProviderStrategy = Lens.lens (\CreateTaskSet' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@CreateTaskSet' {} a -> s {capacityProviderStrategy = a} :: CreateTaskSet) Core.. Lens.mapping Lens._Coerce

-- | The launch type that new tasks in the task set will use. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
createTaskSet_launchType :: Lens.Lens' CreateTaskSet (Core.Maybe LaunchType)
createTaskSet_launchType = Lens.lens (\CreateTaskSet' {launchType} -> launchType) (\s@CreateTaskSet' {} a -> s {launchType = a} :: CreateTaskSet)

-- | The platform version that the tasks in the task set should use. A
-- platform version is specified only for tasks using the Fargate launch
-- type. If one isn\'t specified, the @LATEST@ platform version is used by
-- default.
createTaskSet_platformVersion :: Lens.Lens' CreateTaskSet (Core.Maybe Core.Text)
createTaskSet_platformVersion = Lens.lens (\CreateTaskSet' {platformVersion} -> platformVersion) (\s@CreateTaskSet' {} a -> s {platformVersion = a} :: CreateTaskSet)

-- | A load balancer object representing the load balancer to use with the
-- task set. The supported load balancer types are either an Application
-- Load Balancer or a Network Load Balancer.
createTaskSet_loadBalancers :: Lens.Lens' CreateTaskSet (Core.Maybe [LoadBalancer])
createTaskSet_loadBalancers = Lens.lens (\CreateTaskSet' {loadBalancers} -> loadBalancers) (\s@CreateTaskSet' {} a -> s {loadBalancers = a} :: CreateTaskSet) Core.. Lens.mapping Lens._Coerce

-- | The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. When a service is deleted, the tags are deleted as
-- well.
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
createTaskSet_tags :: Lens.Lens' CreateTaskSet (Core.Maybe [Tag])
createTaskSet_tags = Lens.lens (\CreateTaskSet' {tags} -> tags) (\s@CreateTaskSet' {} a -> s {tags = a} :: CreateTaskSet) Core.. Lens.mapping Lens._Coerce

-- | The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
createTaskSet_serviceRegistries :: Lens.Lens' CreateTaskSet (Core.Maybe [ServiceRegistry])
createTaskSet_serviceRegistries = Lens.lens (\CreateTaskSet' {serviceRegistries} -> serviceRegistries) (\s@CreateTaskSet' {} a -> s {serviceRegistries = a} :: CreateTaskSet) Core.. Lens.mapping Lens._Coerce

-- | An optional non-unique tag that identifies this task set in external
-- systems. If the task set is associated with a service discovery
-- registry, the tasks in this task set will have the
-- @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided
-- value.
createTaskSet_externalId :: Lens.Lens' CreateTaskSet (Core.Maybe Core.Text)
createTaskSet_externalId = Lens.lens (\CreateTaskSet' {externalId} -> externalId) (\s@CreateTaskSet' {} a -> s {externalId = a} :: CreateTaskSet)

-- | Undocumented member.
createTaskSet_scale :: Lens.Lens' CreateTaskSet (Core.Maybe Scale)
createTaskSet_scale = Lens.lens (\CreateTaskSet' {scale} -> scale) (\s@CreateTaskSet' {} a -> s {scale = a} :: CreateTaskSet)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 32 ASCII characters are allowed.
createTaskSet_clientToken :: Lens.Lens' CreateTaskSet (Core.Maybe Core.Text)
createTaskSet_clientToken = Lens.lens (\CreateTaskSet' {clientToken} -> clientToken) (\s@CreateTaskSet' {} a -> s {clientToken = a} :: CreateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the service to
-- create the task set in.
createTaskSet_service :: Lens.Lens' CreateTaskSet Core.Text
createTaskSet_service = Lens.lens (\CreateTaskSet' {service} -> service) (\s@CreateTaskSet' {} a -> s {service = a} :: CreateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to create the task set in.
createTaskSet_cluster :: Lens.Lens' CreateTaskSet Core.Text
createTaskSet_cluster = Lens.lens (\CreateTaskSet' {cluster} -> cluster) (\s@CreateTaskSet' {} a -> s {cluster = a} :: CreateTaskSet)

-- | The task definition for the tasks in the task set to use.
createTaskSet_taskDefinition :: Lens.Lens' CreateTaskSet Core.Text
createTaskSet_taskDefinition = Lens.lens (\CreateTaskSet' {taskDefinition} -> taskDefinition) (\s@CreateTaskSet' {} a -> s {taskDefinition = a} :: CreateTaskSet)

instance Core.AWSRequest CreateTaskSet where
  type
    AWSResponse CreateTaskSet =
      CreateTaskSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskSetResponse'
            Core.<$> (x Core..?> "taskSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTaskSet

instance Core.NFData CreateTaskSet

instance Core.ToHeaders CreateTaskSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.CreateTaskSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTaskSet where
  toJSON CreateTaskSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("networkConfiguration" Core..=)
              Core.<$> networkConfiguration,
            ("capacityProviderStrategy" Core..=)
              Core.<$> capacityProviderStrategy,
            ("launchType" Core..=) Core.<$> launchType,
            ("platformVersion" Core..=) Core.<$> platformVersion,
            ("loadBalancers" Core..=) Core.<$> loadBalancers,
            ("tags" Core..=) Core.<$> tags,
            ("serviceRegistries" Core..=)
              Core.<$> serviceRegistries,
            ("externalId" Core..=) Core.<$> externalId,
            ("scale" Core..=) Core.<$> scale,
            ("clientToken" Core..=) Core.<$> clientToken,
            Core.Just ("service" Core..= service),
            Core.Just ("cluster" Core..= cluster),
            Core.Just ("taskDefinition" Core..= taskDefinition)
          ]
      )

instance Core.ToPath CreateTaskSet where
  toPath = Core.const "/"

instance Core.ToQuery CreateTaskSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTaskSetResponse' smart constructor.
data CreateTaskSetResponse = CreateTaskSetResponse'
  { taskSet :: Core.Maybe TaskSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTaskSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSet', 'createTaskSetResponse_taskSet' - Undocumented member.
--
-- 'httpStatus', 'createTaskSetResponse_httpStatus' - The response's http status code.
newCreateTaskSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTaskSetResponse
newCreateTaskSetResponse pHttpStatus_ =
  CreateTaskSetResponse'
    { taskSet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createTaskSetResponse_taskSet :: Lens.Lens' CreateTaskSetResponse (Core.Maybe TaskSet)
createTaskSetResponse_taskSet = Lens.lens (\CreateTaskSetResponse' {taskSet} -> taskSet) (\s@CreateTaskSetResponse' {} a -> s {taskSet = a} :: CreateTaskSetResponse)

-- | The response's http status code.
createTaskSetResponse_httpStatus :: Lens.Lens' CreateTaskSetResponse Core.Int
createTaskSetResponse_httpStatus = Lens.lens (\CreateTaskSetResponse' {httpStatus} -> httpStatus) (\s@CreateTaskSetResponse' {} a -> s {httpStatus = a} :: CreateTaskSetResponse)

instance Core.NFData CreateTaskSetResponse
