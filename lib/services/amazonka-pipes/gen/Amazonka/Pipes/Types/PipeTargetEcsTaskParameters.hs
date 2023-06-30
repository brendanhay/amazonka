{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pipes.Types.PipeTargetEcsTaskParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetEcsTaskParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.CapacityProviderStrategyItem
import Amazonka.Pipes.Types.EcsTaskOverride
import Amazonka.Pipes.Types.LaunchType
import Amazonka.Pipes.Types.NetworkConfiguration
import Amazonka.Pipes.Types.PlacementConstraint
import Amazonka.Pipes.Types.PlacementStrategy
import Amazonka.Pipes.Types.PropagateTags
import Amazonka.Pipes.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an Amazon ECS task as a target.
--
-- /See:/ 'newPipeTargetEcsTaskParameters' smart constructor.
data PipeTargetEcsTaskParameters = PipeTargetEcsTaskParameters'
  { -- | The capacity provider strategy to use for the task.
    --
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
    -- must be omitted. If no @capacityProviderStrategy@ or launchType is
    -- specified, the @defaultCapacityProviderStrategy@ for the cluster is
    -- used.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | Specifies whether to enable Amazon ECS managed tags for the task. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the Amazon Elastic Container Service Developer Guide.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | Whether or not to enable the execute command functionality for the
    -- containers in this task. If true, this enables execute command
    -- functionality on all containers in the task.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | Specifies an Amazon ECS task group for the task. The maximum length is
    -- 255 characters.
    group' :: Prelude.Maybe Prelude.Text,
    -- | Specifies the launch type on which your task is running. The launch type
    -- that you specify here must match one of the launch type
    -- (compatibilities) of the target task. The @FARGATE@ value is supported
    -- only in the Regions where Fargate with Amazon ECS is supported. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html Fargate on Amazon ECS>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | Use this structure if the Amazon ECS task uses the @awsvpc@ network
    -- mode. This structure specifies the VPC subnets and security groups
    -- associated with the task, and whether a public IP address is to be used.
    -- This structure is required if @LaunchType@ is @FARGATE@ because the
    -- @awsvpc@ mode is required for Fargate tasks.
    --
    -- If you specify @NetworkConfiguration@ when the target ECS task does not
    -- use the @awsvpc@ network mode, the task fails.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The overrides that are associated with a task.
    overrides :: Prelude.Maybe EcsTaskOverride,
    -- | An array of placement constraint objects to use for the task. You can
    -- specify up to 10 constraints per task (including constraints in the task
    -- definition and those specified at runtime).
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The placement strategy objects to use for the task. You can specify a
    -- maximum of five strategy rules per task.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
    -- | Specifies the platform version for the task. Specify only the numeric
    -- portion of the platform version, such as @1.1.0@.
    --
    -- This structure is used only if @LaunchType@ is @FARGATE@. For more
    -- information about valid platform versions, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to propagate the tags from the task definition to the
    -- task. If no value is specified, the tags are not propagated. Tags can
    -- only be propagated to the task during task creation. To add tags to a
    -- task after task creation, use the @TagResource@ API action.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The reference ID to use for the task.
    referenceId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The metadata that you apply to the task to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define. To learn more, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html#ECS-RunTask-request-tags RunTask>
    -- in the Amazon ECS API Reference.
    tags :: Prelude.Maybe [Tag],
    -- | The number of tasks to create based on @TaskDefinition@. The default is
    -- 1.
    taskCount :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the task definition to use if the event target is an Amazon
    -- ECS task.
    taskDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetEcsTaskParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProviderStrategy', 'pipeTargetEcsTaskParameters_capacityProviderStrategy' - The capacity provider strategy to use for the task.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or launchType is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- 'enableECSManagedTags', 'pipeTargetEcsTaskParameters_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the Amazon Elastic Container Service Developer Guide.
--
-- 'enableExecuteCommand', 'pipeTargetEcsTaskParameters_enableExecuteCommand' - Whether or not to enable the execute command functionality for the
-- containers in this task. If true, this enables execute command
-- functionality on all containers in the task.
--
-- 'group'', 'pipeTargetEcsTaskParameters_group' - Specifies an Amazon ECS task group for the task. The maximum length is
-- 255 characters.
--
-- 'launchType', 'pipeTargetEcsTaskParameters_launchType' - Specifies the launch type on which your task is running. The launch type
-- that you specify here must match one of the launch type
-- (compatibilities) of the target task. The @FARGATE@ value is supported
-- only in the Regions where Fargate with Amazon ECS is supported. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html Fargate on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'networkConfiguration', 'pipeTargetEcsTaskParameters_networkConfiguration' - Use this structure if the Amazon ECS task uses the @awsvpc@ network
-- mode. This structure specifies the VPC subnets and security groups
-- associated with the task, and whether a public IP address is to be used.
-- This structure is required if @LaunchType@ is @FARGATE@ because the
-- @awsvpc@ mode is required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not
-- use the @awsvpc@ network mode, the task fails.
--
-- 'overrides', 'pipeTargetEcsTaskParameters_overrides' - The overrides that are associated with a task.
--
-- 'placementConstraints', 'pipeTargetEcsTaskParameters_placementConstraints' - An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
--
-- 'placementStrategy', 'pipeTargetEcsTaskParameters_placementStrategy' - The placement strategy objects to use for the task. You can specify a
-- maximum of five strategy rules per task.
--
-- 'platformVersion', 'pipeTargetEcsTaskParameters_platformVersion' - Specifies the platform version for the task. Specify only the numeric
-- portion of the platform version, such as @1.1.0@.
--
-- This structure is used only if @LaunchType@ is @FARGATE@. For more
-- information about valid platform versions, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'propagateTags', 'pipeTargetEcsTaskParameters_propagateTags' - Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the @TagResource@ API action.
--
-- 'referenceId', 'pipeTargetEcsTaskParameters_referenceId' - The reference ID to use for the task.
--
-- 'tags', 'pipeTargetEcsTaskParameters_tags' - The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. To learn more, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html#ECS-RunTask-request-tags RunTask>
-- in the Amazon ECS API Reference.
--
-- 'taskCount', 'pipeTargetEcsTaskParameters_taskCount' - The number of tasks to create based on @TaskDefinition@. The default is
-- 1.
--
-- 'taskDefinitionArn', 'pipeTargetEcsTaskParameters_taskDefinitionArn' - The ARN of the task definition to use if the event target is an Amazon
-- ECS task.
newPipeTargetEcsTaskParameters ::
  -- | 'taskDefinitionArn'
  Prelude.Text ->
  PipeTargetEcsTaskParameters
newPipeTargetEcsTaskParameters pTaskDefinitionArn_ =
  PipeTargetEcsTaskParameters'
    { capacityProviderStrategy =
        Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      group' = Prelude.Nothing,
      launchType = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      overrides = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      placementStrategy = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      referenceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      taskCount = Prelude.Nothing,
      taskDefinitionArn = pTaskDefinitionArn_
    }

-- | The capacity provider strategy to use for the task.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or launchType is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
pipeTargetEcsTaskParameters_capacityProviderStrategy :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe [CapacityProviderStrategyItem])
pipeTargetEcsTaskParameters_capacityProviderStrategy = Lens.lens (\PipeTargetEcsTaskParameters' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@PipeTargetEcsTaskParameters' {} a -> s {capacityProviderStrategy = a} :: PipeTargetEcsTaskParameters) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the Amazon Elastic Container Service Developer Guide.
pipeTargetEcsTaskParameters_enableECSManagedTags :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe Prelude.Bool)
pipeTargetEcsTaskParameters_enableECSManagedTags = Lens.lens (\PipeTargetEcsTaskParameters' {enableECSManagedTags} -> enableECSManagedTags) (\s@PipeTargetEcsTaskParameters' {} a -> s {enableECSManagedTags = a} :: PipeTargetEcsTaskParameters)

-- | Whether or not to enable the execute command functionality for the
-- containers in this task. If true, this enables execute command
-- functionality on all containers in the task.
pipeTargetEcsTaskParameters_enableExecuteCommand :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe Prelude.Bool)
pipeTargetEcsTaskParameters_enableExecuteCommand = Lens.lens (\PipeTargetEcsTaskParameters' {enableExecuteCommand} -> enableExecuteCommand) (\s@PipeTargetEcsTaskParameters' {} a -> s {enableExecuteCommand = a} :: PipeTargetEcsTaskParameters)

-- | Specifies an Amazon ECS task group for the task. The maximum length is
-- 255 characters.
pipeTargetEcsTaskParameters_group :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe Prelude.Text)
pipeTargetEcsTaskParameters_group = Lens.lens (\PipeTargetEcsTaskParameters' {group'} -> group') (\s@PipeTargetEcsTaskParameters' {} a -> s {group' = a} :: PipeTargetEcsTaskParameters)

-- | Specifies the launch type on which your task is running. The launch type
-- that you specify here must match one of the launch type
-- (compatibilities) of the target task. The @FARGATE@ value is supported
-- only in the Regions where Fargate with Amazon ECS is supported. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html Fargate on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
pipeTargetEcsTaskParameters_launchType :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe LaunchType)
pipeTargetEcsTaskParameters_launchType = Lens.lens (\PipeTargetEcsTaskParameters' {launchType} -> launchType) (\s@PipeTargetEcsTaskParameters' {} a -> s {launchType = a} :: PipeTargetEcsTaskParameters)

-- | Use this structure if the Amazon ECS task uses the @awsvpc@ network
-- mode. This structure specifies the VPC subnets and security groups
-- associated with the task, and whether a public IP address is to be used.
-- This structure is required if @LaunchType@ is @FARGATE@ because the
-- @awsvpc@ mode is required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not
-- use the @awsvpc@ network mode, the task fails.
pipeTargetEcsTaskParameters_networkConfiguration :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe NetworkConfiguration)
pipeTargetEcsTaskParameters_networkConfiguration = Lens.lens (\PipeTargetEcsTaskParameters' {networkConfiguration} -> networkConfiguration) (\s@PipeTargetEcsTaskParameters' {} a -> s {networkConfiguration = a} :: PipeTargetEcsTaskParameters)

-- | The overrides that are associated with a task.
pipeTargetEcsTaskParameters_overrides :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe EcsTaskOverride)
pipeTargetEcsTaskParameters_overrides = Lens.lens (\PipeTargetEcsTaskParameters' {overrides} -> overrides) (\s@PipeTargetEcsTaskParameters' {} a -> s {overrides = a} :: PipeTargetEcsTaskParameters)

-- | An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
pipeTargetEcsTaskParameters_placementConstraints :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe [PlacementConstraint])
pipeTargetEcsTaskParameters_placementConstraints = Lens.lens (\PipeTargetEcsTaskParameters' {placementConstraints} -> placementConstraints) (\s@PipeTargetEcsTaskParameters' {} a -> s {placementConstraints = a} :: PipeTargetEcsTaskParameters) Prelude.. Lens.mapping Lens.coerced

-- | The placement strategy objects to use for the task. You can specify a
-- maximum of five strategy rules per task.
pipeTargetEcsTaskParameters_placementStrategy :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe [PlacementStrategy])
pipeTargetEcsTaskParameters_placementStrategy = Lens.lens (\PipeTargetEcsTaskParameters' {placementStrategy} -> placementStrategy) (\s@PipeTargetEcsTaskParameters' {} a -> s {placementStrategy = a} :: PipeTargetEcsTaskParameters) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the platform version for the task. Specify only the numeric
-- portion of the platform version, such as @1.1.0@.
--
-- This structure is used only if @LaunchType@ is @FARGATE@. For more
-- information about valid platform versions, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
pipeTargetEcsTaskParameters_platformVersion :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe Prelude.Text)
pipeTargetEcsTaskParameters_platformVersion = Lens.lens (\PipeTargetEcsTaskParameters' {platformVersion} -> platformVersion) (\s@PipeTargetEcsTaskParameters' {} a -> s {platformVersion = a} :: PipeTargetEcsTaskParameters)

-- | Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the @TagResource@ API action.
pipeTargetEcsTaskParameters_propagateTags :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe PropagateTags)
pipeTargetEcsTaskParameters_propagateTags = Lens.lens (\PipeTargetEcsTaskParameters' {propagateTags} -> propagateTags) (\s@PipeTargetEcsTaskParameters' {} a -> s {propagateTags = a} :: PipeTargetEcsTaskParameters)

-- | The reference ID to use for the task.
pipeTargetEcsTaskParameters_referenceId :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe Prelude.Text)
pipeTargetEcsTaskParameters_referenceId = Lens.lens (\PipeTargetEcsTaskParameters' {referenceId} -> referenceId) (\s@PipeTargetEcsTaskParameters' {} a -> s {referenceId = a} :: PipeTargetEcsTaskParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. To learn more, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html#ECS-RunTask-request-tags RunTask>
-- in the Amazon ECS API Reference.
pipeTargetEcsTaskParameters_tags :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe [Tag])
pipeTargetEcsTaskParameters_tags = Lens.lens (\PipeTargetEcsTaskParameters' {tags} -> tags) (\s@PipeTargetEcsTaskParameters' {} a -> s {tags = a} :: PipeTargetEcsTaskParameters) Prelude.. Lens.mapping Lens.coerced

-- | The number of tasks to create based on @TaskDefinition@. The default is
-- 1.
pipeTargetEcsTaskParameters_taskCount :: Lens.Lens' PipeTargetEcsTaskParameters (Prelude.Maybe Prelude.Natural)
pipeTargetEcsTaskParameters_taskCount = Lens.lens (\PipeTargetEcsTaskParameters' {taskCount} -> taskCount) (\s@PipeTargetEcsTaskParameters' {} a -> s {taskCount = a} :: PipeTargetEcsTaskParameters)

-- | The ARN of the task definition to use if the event target is an Amazon
-- ECS task.
pipeTargetEcsTaskParameters_taskDefinitionArn :: Lens.Lens' PipeTargetEcsTaskParameters Prelude.Text
pipeTargetEcsTaskParameters_taskDefinitionArn = Lens.lens (\PipeTargetEcsTaskParameters' {taskDefinitionArn} -> taskDefinitionArn) (\s@PipeTargetEcsTaskParameters' {} a -> s {taskDefinitionArn = a} :: PipeTargetEcsTaskParameters)

instance Data.FromJSON PipeTargetEcsTaskParameters where
  parseJSON =
    Data.withObject
      "PipeTargetEcsTaskParameters"
      ( \x ->
          PipeTargetEcsTaskParameters'
            Prelude.<$> ( x
                            Data..:? "CapacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EnableECSManagedTags")
            Prelude.<*> (x Data..:? "EnableExecuteCommand")
            Prelude.<*> (x Data..:? "Group")
            Prelude.<*> (x Data..:? "LaunchType")
            Prelude.<*> (x Data..:? "NetworkConfiguration")
            Prelude.<*> (x Data..:? "Overrides")
            Prelude.<*> ( x
                            Data..:? "PlacementConstraints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PlacementStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PlatformVersion")
            Prelude.<*> (x Data..:? "PropagateTags")
            Prelude.<*> (x Data..:? "ReferenceId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TaskCount")
            Prelude.<*> (x Data..: "TaskDefinitionArn")
      )

instance Prelude.Hashable PipeTargetEcsTaskParameters where
  hashWithSalt _salt PipeTargetEcsTaskParameters' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskCount
      `Prelude.hashWithSalt` taskDefinitionArn

instance Prelude.NFData PipeTargetEcsTaskParameters where
  rnf PipeTargetEcsTaskParameters' {..} =
    Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf enableECSManagedTags
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf group'
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskCount
      `Prelude.seq` Prelude.rnf taskDefinitionArn

instance Data.ToJSON PipeTargetEcsTaskParameters where
  toJSON PipeTargetEcsTaskParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CapacityProviderStrategy" Data..=)
              Prelude.<$> capacityProviderStrategy,
            ("EnableECSManagedTags" Data..=)
              Prelude.<$> enableECSManagedTags,
            ("EnableExecuteCommand" Data..=)
              Prelude.<$> enableExecuteCommand,
            ("Group" Data..=) Prelude.<$> group',
            ("LaunchType" Data..=) Prelude.<$> launchType,
            ("NetworkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("Overrides" Data..=) Prelude.<$> overrides,
            ("PlacementConstraints" Data..=)
              Prelude.<$> placementConstraints,
            ("PlacementStrategy" Data..=)
              Prelude.<$> placementStrategy,
            ("PlatformVersion" Data..=)
              Prelude.<$> platformVersion,
            ("PropagateTags" Data..=) Prelude.<$> propagateTags,
            ("ReferenceId" Data..=) Prelude.<$> referenceId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TaskCount" Data..=) Prelude.<$> taskCount,
            Prelude.Just
              ("TaskDefinitionArn" Data..= taskDefinitionArn)
          ]
      )
