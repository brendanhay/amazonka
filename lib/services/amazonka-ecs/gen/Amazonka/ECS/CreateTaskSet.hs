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
-- Module      : Amazonka.ECS.CreateTaskSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a task set in the specified cluster and service. This is used
-- when a service uses the @EXTERNAL@ deployment controller type. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS deployment types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.CreateTaskSet
  ( -- * Creating a Request
    CreateTaskSet (..),
    newCreateTaskSet,

    -- * Request Lenses
    createTaskSet_capacityProviderStrategy,
    createTaskSet_clientToken,
    createTaskSet_externalId,
    createTaskSet_launchType,
    createTaskSet_loadBalancers,
    createTaskSet_networkConfiguration,
    createTaskSet_platformVersion,
    createTaskSet_scale,
    createTaskSet_serviceRegistries,
    createTaskSet_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTaskSet' smart constructor.
data CreateTaskSet = CreateTaskSet'
  { -- | The capacity provider strategy to use for the task set.
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
    -- To use a Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
    -- available to all accounts and only need to be associated with a cluster
    -- to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The identifier that you provide to ensure the idempotency of the
    -- request. It\'s case sensitive and must be unique. It can be up to 32
    -- ASCII characters are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An optional non-unique tag that identifies this task set in external
    -- systems. If the task set is associated with a service discovery
    -- registry, the tasks in this task set will have the
    -- @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute set to the provided
    -- value.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The launch type that new tasks in the task set uses. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
    -- must be omitted.
    launchType :: Prelude.Maybe LaunchType,
    -- | A load balancer object representing the load balancer to use with the
    -- task set. The supported load balancer types are either an Application
    -- Load Balancer or a Network Load Balancer.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | An object representing the network configuration for a task set.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The platform version that the tasks in the task set uses. A platform
    -- version is specified only for tasks using the Fargate launch type. If
    -- one isn\'t specified, the @LATEST@ platform version is used.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | A floating-point percentage of the desired number of tasks to place and
    -- keep running in the task set.
    scale :: Prelude.Maybe Scale,
    -- | The details of the service discovery registries to assign to this task
    -- set. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The metadata that you apply to the task set to help you categorize and
    -- organize them. Each tag consists of a key and an optional value. You
    -- define both. When a service is deleted, the tags are deleted.
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
    tags :: Prelude.Maybe [Tag],
    -- | The short name or full Amazon Resource Name (ARN) of the service to
    -- create the task set in.
    service :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service to create the task set in.
    cluster :: Prelude.Text,
    -- | The task definition for the tasks in the task set to use.
    taskDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'clientToken', 'createTaskSet_clientToken' - The identifier that you provide to ensure the idempotency of the
-- request. It\'s case sensitive and must be unique. It can be up to 32
-- ASCII characters are allowed.
--
-- 'externalId', 'createTaskSet_externalId' - An optional non-unique tag that identifies this task set in external
-- systems. If the task set is associated with a service discovery
-- registry, the tasks in this task set will have the
-- @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute set to the provided
-- value.
--
-- 'launchType', 'createTaskSet_launchType' - The launch type that new tasks in the task set uses. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- 'loadBalancers', 'createTaskSet_loadBalancers' - A load balancer object representing the load balancer to use with the
-- task set. The supported load balancer types are either an Application
-- Load Balancer or a Network Load Balancer.
--
-- 'networkConfiguration', 'createTaskSet_networkConfiguration' - An object representing the network configuration for a task set.
--
-- 'platformVersion', 'createTaskSet_platformVersion' - The platform version that the tasks in the task set uses. A platform
-- version is specified only for tasks using the Fargate launch type. If
-- one isn\'t specified, the @LATEST@ platform version is used.
--
-- 'scale', 'createTaskSet_scale' - A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
--
-- 'serviceRegistries', 'createTaskSet_serviceRegistries' - The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
--
-- 'tags', 'createTaskSet_tags' - The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define both. When a service is deleted, the tags are deleted.
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
  Prelude.Text ->
  -- | 'cluster'
  Prelude.Text ->
  -- | 'taskDefinition'
  Prelude.Text ->
  CreateTaskSet
newCreateTaskSet pService_ pCluster_ pTaskDefinition_ =
  CreateTaskSet'
    { capacityProviderStrategy =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      externalId = Prelude.Nothing,
      launchType = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      scale = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      tags = Prelude.Nothing,
      service = pService_,
      cluster = pCluster_,
      taskDefinition = pTaskDefinition_
    }

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
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
createTaskSet_capacityProviderStrategy :: Lens.Lens' CreateTaskSet (Prelude.Maybe [CapacityProviderStrategyItem])
createTaskSet_capacityProviderStrategy = Lens.lens (\CreateTaskSet' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@CreateTaskSet' {} a -> s {capacityProviderStrategy = a} :: CreateTaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The identifier that you provide to ensure the idempotency of the
-- request. It\'s case sensitive and must be unique. It can be up to 32
-- ASCII characters are allowed.
createTaskSet_clientToken :: Lens.Lens' CreateTaskSet (Prelude.Maybe Prelude.Text)
createTaskSet_clientToken = Lens.lens (\CreateTaskSet' {clientToken} -> clientToken) (\s@CreateTaskSet' {} a -> s {clientToken = a} :: CreateTaskSet)

-- | An optional non-unique tag that identifies this task set in external
-- systems. If the task set is associated with a service discovery
-- registry, the tasks in this task set will have the
-- @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute set to the provided
-- value.
createTaskSet_externalId :: Lens.Lens' CreateTaskSet (Prelude.Maybe Prelude.Text)
createTaskSet_externalId = Lens.lens (\CreateTaskSet' {externalId} -> externalId) (\s@CreateTaskSet' {} a -> s {externalId = a} :: CreateTaskSet)

-- | The launch type that new tasks in the task set uses. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
createTaskSet_launchType :: Lens.Lens' CreateTaskSet (Prelude.Maybe LaunchType)
createTaskSet_launchType = Lens.lens (\CreateTaskSet' {launchType} -> launchType) (\s@CreateTaskSet' {} a -> s {launchType = a} :: CreateTaskSet)

-- | A load balancer object representing the load balancer to use with the
-- task set. The supported load balancer types are either an Application
-- Load Balancer or a Network Load Balancer.
createTaskSet_loadBalancers :: Lens.Lens' CreateTaskSet (Prelude.Maybe [LoadBalancer])
createTaskSet_loadBalancers = Lens.lens (\CreateTaskSet' {loadBalancers} -> loadBalancers) (\s@CreateTaskSet' {} a -> s {loadBalancers = a} :: CreateTaskSet) Prelude.. Lens.mapping Lens.coerced

-- | An object representing the network configuration for a task set.
createTaskSet_networkConfiguration :: Lens.Lens' CreateTaskSet (Prelude.Maybe NetworkConfiguration)
createTaskSet_networkConfiguration = Lens.lens (\CreateTaskSet' {networkConfiguration} -> networkConfiguration) (\s@CreateTaskSet' {} a -> s {networkConfiguration = a} :: CreateTaskSet)

-- | The platform version that the tasks in the task set uses. A platform
-- version is specified only for tasks using the Fargate launch type. If
-- one isn\'t specified, the @LATEST@ platform version is used.
createTaskSet_platformVersion :: Lens.Lens' CreateTaskSet (Prelude.Maybe Prelude.Text)
createTaskSet_platformVersion = Lens.lens (\CreateTaskSet' {platformVersion} -> platformVersion) (\s@CreateTaskSet' {} a -> s {platformVersion = a} :: CreateTaskSet)

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
createTaskSet_scale :: Lens.Lens' CreateTaskSet (Prelude.Maybe Scale)
createTaskSet_scale = Lens.lens (\CreateTaskSet' {scale} -> scale) (\s@CreateTaskSet' {} a -> s {scale = a} :: CreateTaskSet)

-- | The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
createTaskSet_serviceRegistries :: Lens.Lens' CreateTaskSet (Prelude.Maybe [ServiceRegistry])
createTaskSet_serviceRegistries = Lens.lens (\CreateTaskSet' {serviceRegistries} -> serviceRegistries) (\s@CreateTaskSet' {} a -> s {serviceRegistries = a} :: CreateTaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define both. When a service is deleted, the tags are deleted.
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
createTaskSet_tags :: Lens.Lens' CreateTaskSet (Prelude.Maybe [Tag])
createTaskSet_tags = Lens.lens (\CreateTaskSet' {tags} -> tags) (\s@CreateTaskSet' {} a -> s {tags = a} :: CreateTaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the service to
-- create the task set in.
createTaskSet_service :: Lens.Lens' CreateTaskSet Prelude.Text
createTaskSet_service = Lens.lens (\CreateTaskSet' {service} -> service) (\s@CreateTaskSet' {} a -> s {service = a} :: CreateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service to create the task set in.
createTaskSet_cluster :: Lens.Lens' CreateTaskSet Prelude.Text
createTaskSet_cluster = Lens.lens (\CreateTaskSet' {cluster} -> cluster) (\s@CreateTaskSet' {} a -> s {cluster = a} :: CreateTaskSet)

-- | The task definition for the tasks in the task set to use.
createTaskSet_taskDefinition :: Lens.Lens' CreateTaskSet Prelude.Text
createTaskSet_taskDefinition = Lens.lens (\CreateTaskSet' {taskDefinition} -> taskDefinition) (\s@CreateTaskSet' {} a -> s {taskDefinition = a} :: CreateTaskSet)

instance Core.AWSRequest CreateTaskSet where
  type
    AWSResponse CreateTaskSet =
      CreateTaskSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskSetResponse'
            Prelude.<$> (x Data..?> "taskSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTaskSet where
  hashWithSalt _salt CreateTaskSet' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` taskDefinition

instance Prelude.NFData CreateTaskSet where
  rnf CreateTaskSet' {..} =
    Prelude.rnf capacityProviderStrategy `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf externalId `Prelude.seq`
          Prelude.rnf launchType `Prelude.seq`
            Prelude.rnf loadBalancers `Prelude.seq`
              Prelude.rnf networkConfiguration `Prelude.seq`
                Prelude.rnf platformVersion `Prelude.seq`
                  Prelude.rnf scale `Prelude.seq`
                    Prelude.rnf serviceRegistries `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf service `Prelude.seq`
                          Prelude.rnf cluster `Prelude.seq`
                            Prelude.rnf taskDefinition

instance Data.ToHeaders CreateTaskSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.CreateTaskSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTaskSet where
  toJSON CreateTaskSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capacityProviderStrategy" Data..=)
              Prelude.<$> capacityProviderStrategy,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("externalId" Data..=) Prelude.<$> externalId,
            ("launchType" Data..=) Prelude.<$> launchType,
            ("loadBalancers" Data..=) Prelude.<$> loadBalancers,
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("platformVersion" Data..=)
              Prelude.<$> platformVersion,
            ("scale" Data..=) Prelude.<$> scale,
            ("serviceRegistries" Data..=)
              Prelude.<$> serviceRegistries,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("service" Data..= service),
            Prelude.Just ("cluster" Data..= cluster),
            Prelude.Just
              ("taskDefinition" Data..= taskDefinition)
          ]
      )

instance Data.ToPath CreateTaskSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTaskSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTaskSetResponse' smart constructor.
data CreateTaskSetResponse = CreateTaskSetResponse'
  { -- | Information about a set of Amazon ECS tasks in either an CodeDeploy or
    -- an @EXTERNAL@ deployment. A task set includes details such as the
    -- desired number of tasks, how many tasks are running, and whether the
    -- task set serves production traffic.
    taskSet :: Prelude.Maybe TaskSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTaskSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSet', 'createTaskSetResponse_taskSet' - Information about a set of Amazon ECS tasks in either an CodeDeploy or
-- an @EXTERNAL@ deployment. A task set includes details such as the
-- desired number of tasks, how many tasks are running, and whether the
-- task set serves production traffic.
--
-- 'httpStatus', 'createTaskSetResponse_httpStatus' - The response's http status code.
newCreateTaskSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTaskSetResponse
newCreateTaskSetResponse pHttpStatus_ =
  CreateTaskSetResponse'
    { taskSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a set of Amazon ECS tasks in either an CodeDeploy or
-- an @EXTERNAL@ deployment. A task set includes details such as the
-- desired number of tasks, how many tasks are running, and whether the
-- task set serves production traffic.
createTaskSetResponse_taskSet :: Lens.Lens' CreateTaskSetResponse (Prelude.Maybe TaskSet)
createTaskSetResponse_taskSet = Lens.lens (\CreateTaskSetResponse' {taskSet} -> taskSet) (\s@CreateTaskSetResponse' {} a -> s {taskSet = a} :: CreateTaskSetResponse)

-- | The response's http status code.
createTaskSetResponse_httpStatus :: Lens.Lens' CreateTaskSetResponse Prelude.Int
createTaskSetResponse_httpStatus = Lens.lens (\CreateTaskSetResponse' {httpStatus} -> httpStatus) (\s@CreateTaskSetResponse' {} a -> s {httpStatus = a} :: CreateTaskSetResponse)

instance Prelude.NFData CreateTaskSetResponse where
  rnf CreateTaskSetResponse' {..} =
    Prelude.rnf taskSet `Prelude.seq`
      Prelude.rnf httpStatus
