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
-- Module      : Amazonka.Scheduler.Types.EcsParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.EcsParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.CapacityProviderStrategyItem
import Amazonka.Scheduler.Types.LaunchType
import Amazonka.Scheduler.Types.NetworkConfiguration
import Amazonka.Scheduler.Types.PlacementConstraint
import Amazonka.Scheduler.Types.PlacementStrategy
import Amazonka.Scheduler.Types.PropagateTags

-- | The templated target type for the Amazon ECS
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
-- API operation.
--
-- /See:/ 'newEcsParameters' smart constructor.
data EcsParameters = EcsParameters'
  { -- | The capacity provider strategy to use for the task.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | Specifies whether to enable Amazon ECS managed tags for the task. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon ECS Developer Guide/.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | Whether or not to enable the execute command functionality for the
    -- containers in this task. If true, this enables execute command
    -- functionality on all containers in the task.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | Specifies an ECS task group for the task. The maximum length is 255
    -- characters.
    group' :: Prelude.Maybe Prelude.Text,
    -- | Specifies the launch type on which your task is running. The launch type
    -- that you specify here must match one of the launch type
    -- (compatibilities) of the target task. The @FARGATE@ value is supported
    -- only in the Regions where Fargate with Amazon ECS is supported. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS_Fargate.html AWS Fargate on Amazon ECS>
    -- in the /Amazon ECS Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | This structure specifies the network configuration for an ECS task.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | An array of placement constraint objects to use for the task. You can
    -- specify up to 10 constraints per task (including constraints in the task
    -- definition and those specified at runtime).
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The task placement strategy for a task or service.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
    -- | Specifies the platform version for the task. Specify only the numeric
    -- portion of the platform version, such as @1.1.0@.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to propagate the tags from the task definition to the
    -- task. If no value is specified, the tags are not propagated. Tags can
    -- only be propagated to the task during task creation. To add tags to a
    -- task after task creation, use Amazon ECS\'s
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_TagResource.html TagResource>
    -- API action.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The reference ID to use for the task.
    referenceId :: Prelude.Maybe Prelude.Text,
    -- | The metadata that you apply to the task to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
    -- in the /Amazon ECS API Reference/.
    tags :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | The number of tasks to create based on @TaskDefinition@. The default is
    -- @1@.
    taskCount :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the task definition to use if the
    -- event target is an Amazon ECS task.
    taskDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProviderStrategy', 'ecsParameters_capacityProviderStrategy' - The capacity provider strategy to use for the task.
--
-- 'enableECSManagedTags', 'ecsParameters_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon ECS Developer Guide/.
--
-- 'enableExecuteCommand', 'ecsParameters_enableExecuteCommand' - Whether or not to enable the execute command functionality for the
-- containers in this task. If true, this enables execute command
-- functionality on all containers in the task.
--
-- 'group'', 'ecsParameters_group' - Specifies an ECS task group for the task. The maximum length is 255
-- characters.
--
-- 'launchType', 'ecsParameters_launchType' - Specifies the launch type on which your task is running. The launch type
-- that you specify here must match one of the launch type
-- (compatibilities) of the target task. The @FARGATE@ value is supported
-- only in the Regions where Fargate with Amazon ECS is supported. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS_Fargate.html AWS Fargate on Amazon ECS>
-- in the /Amazon ECS Developer Guide/.
--
-- 'networkConfiguration', 'ecsParameters_networkConfiguration' - This structure specifies the network configuration for an ECS task.
--
-- 'placementConstraints', 'ecsParameters_placementConstraints' - An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
--
-- 'placementStrategy', 'ecsParameters_placementStrategy' - The task placement strategy for a task or service.
--
-- 'platformVersion', 'ecsParameters_platformVersion' - Specifies the platform version for the task. Specify only the numeric
-- portion of the platform version, such as @1.1.0@.
--
-- 'propagateTags', 'ecsParameters_propagateTags' - Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use Amazon ECS\'s
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_TagResource.html TagResource>
-- API action.
--
-- 'referenceId', 'ecsParameters_referenceId' - The reference ID to use for the task.
--
-- 'tags', 'ecsParameters_tags' - The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
-- in the /Amazon ECS API Reference/.
--
-- 'taskCount', 'ecsParameters_taskCount' - The number of tasks to create based on @TaskDefinition@. The default is
-- @1@.
--
-- 'taskDefinitionArn', 'ecsParameters_taskDefinitionArn' - The Amazon Resource Name (ARN) of the task definition to use if the
-- event target is an Amazon ECS task.
newEcsParameters ::
  -- | 'taskDefinitionArn'
  Prelude.Text ->
  EcsParameters
newEcsParameters pTaskDefinitionArn_ =
  EcsParameters'
    { capacityProviderStrategy =
        Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      group' = Prelude.Nothing,
      launchType = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
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
ecsParameters_capacityProviderStrategy :: Lens.Lens' EcsParameters (Prelude.Maybe [CapacityProviderStrategyItem])
ecsParameters_capacityProviderStrategy = Lens.lens (\EcsParameters' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@EcsParameters' {} a -> s {capacityProviderStrategy = a} :: EcsParameters) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon ECS Developer Guide/.
ecsParameters_enableECSManagedTags :: Lens.Lens' EcsParameters (Prelude.Maybe Prelude.Bool)
ecsParameters_enableECSManagedTags = Lens.lens (\EcsParameters' {enableECSManagedTags} -> enableECSManagedTags) (\s@EcsParameters' {} a -> s {enableECSManagedTags = a} :: EcsParameters)

-- | Whether or not to enable the execute command functionality for the
-- containers in this task. If true, this enables execute command
-- functionality on all containers in the task.
ecsParameters_enableExecuteCommand :: Lens.Lens' EcsParameters (Prelude.Maybe Prelude.Bool)
ecsParameters_enableExecuteCommand = Lens.lens (\EcsParameters' {enableExecuteCommand} -> enableExecuteCommand) (\s@EcsParameters' {} a -> s {enableExecuteCommand = a} :: EcsParameters)

-- | Specifies an ECS task group for the task. The maximum length is 255
-- characters.
ecsParameters_group :: Lens.Lens' EcsParameters (Prelude.Maybe Prelude.Text)
ecsParameters_group = Lens.lens (\EcsParameters' {group'} -> group') (\s@EcsParameters' {} a -> s {group' = a} :: EcsParameters)

-- | Specifies the launch type on which your task is running. The launch type
-- that you specify here must match one of the launch type
-- (compatibilities) of the target task. The @FARGATE@ value is supported
-- only in the Regions where Fargate with Amazon ECS is supported. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS_Fargate.html AWS Fargate on Amazon ECS>
-- in the /Amazon ECS Developer Guide/.
ecsParameters_launchType :: Lens.Lens' EcsParameters (Prelude.Maybe LaunchType)
ecsParameters_launchType = Lens.lens (\EcsParameters' {launchType} -> launchType) (\s@EcsParameters' {} a -> s {launchType = a} :: EcsParameters)

-- | This structure specifies the network configuration for an ECS task.
ecsParameters_networkConfiguration :: Lens.Lens' EcsParameters (Prelude.Maybe NetworkConfiguration)
ecsParameters_networkConfiguration = Lens.lens (\EcsParameters' {networkConfiguration} -> networkConfiguration) (\s@EcsParameters' {} a -> s {networkConfiguration = a} :: EcsParameters)

-- | An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
ecsParameters_placementConstraints :: Lens.Lens' EcsParameters (Prelude.Maybe [PlacementConstraint])
ecsParameters_placementConstraints = Lens.lens (\EcsParameters' {placementConstraints} -> placementConstraints) (\s@EcsParameters' {} a -> s {placementConstraints = a} :: EcsParameters) Prelude.. Lens.mapping Lens.coerced

-- | The task placement strategy for a task or service.
ecsParameters_placementStrategy :: Lens.Lens' EcsParameters (Prelude.Maybe [PlacementStrategy])
ecsParameters_placementStrategy = Lens.lens (\EcsParameters' {placementStrategy} -> placementStrategy) (\s@EcsParameters' {} a -> s {placementStrategy = a} :: EcsParameters) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the platform version for the task. Specify only the numeric
-- portion of the platform version, such as @1.1.0@.
ecsParameters_platformVersion :: Lens.Lens' EcsParameters (Prelude.Maybe Prelude.Text)
ecsParameters_platformVersion = Lens.lens (\EcsParameters' {platformVersion} -> platformVersion) (\s@EcsParameters' {} a -> s {platformVersion = a} :: EcsParameters)

-- | Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use Amazon ECS\'s
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_TagResource.html TagResource>
-- API action.
ecsParameters_propagateTags :: Lens.Lens' EcsParameters (Prelude.Maybe PropagateTags)
ecsParameters_propagateTags = Lens.lens (\EcsParameters' {propagateTags} -> propagateTags) (\s@EcsParameters' {} a -> s {propagateTags = a} :: EcsParameters)

-- | The reference ID to use for the task.
ecsParameters_referenceId :: Lens.Lens' EcsParameters (Prelude.Maybe Prelude.Text)
ecsParameters_referenceId = Lens.lens (\EcsParameters' {referenceId} -> referenceId) (\s@EcsParameters' {} a -> s {referenceId = a} :: EcsParameters)

-- | The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
-- in the /Amazon ECS API Reference/.
ecsParameters_tags :: Lens.Lens' EcsParameters (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
ecsParameters_tags = Lens.lens (\EcsParameters' {tags} -> tags) (\s@EcsParameters' {} a -> s {tags = a} :: EcsParameters) Prelude.. Lens.mapping Lens.coerced

-- | The number of tasks to create based on @TaskDefinition@. The default is
-- @1@.
ecsParameters_taskCount :: Lens.Lens' EcsParameters (Prelude.Maybe Prelude.Natural)
ecsParameters_taskCount = Lens.lens (\EcsParameters' {taskCount} -> taskCount) (\s@EcsParameters' {} a -> s {taskCount = a} :: EcsParameters)

-- | The Amazon Resource Name (ARN) of the task definition to use if the
-- event target is an Amazon ECS task.
ecsParameters_taskDefinitionArn :: Lens.Lens' EcsParameters Prelude.Text
ecsParameters_taskDefinitionArn = Lens.lens (\EcsParameters' {taskDefinitionArn} -> taskDefinitionArn) (\s@EcsParameters' {} a -> s {taskDefinitionArn = a} :: EcsParameters)

instance Data.FromJSON EcsParameters where
  parseJSON =
    Data.withObject
      "EcsParameters"
      ( \x ->
          EcsParameters'
            Prelude.<$> ( x
                            Data..:? "CapacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EnableECSManagedTags")
            Prelude.<*> (x Data..:? "EnableExecuteCommand")
            Prelude.<*> (x Data..:? "Group")
            Prelude.<*> (x Data..:? "LaunchType")
            Prelude.<*> (x Data..:? "NetworkConfiguration")
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

instance Prelude.Hashable EcsParameters where
  hashWithSalt _salt EcsParameters' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskCount
      `Prelude.hashWithSalt` taskDefinitionArn

instance Prelude.NFData EcsParameters where
  rnf EcsParameters' {..} =
    Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf enableECSManagedTags
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf group'
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskCount
      `Prelude.seq` Prelude.rnf taskDefinitionArn

instance Data.ToJSON EcsParameters where
  toJSON EcsParameters' {..} =
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
