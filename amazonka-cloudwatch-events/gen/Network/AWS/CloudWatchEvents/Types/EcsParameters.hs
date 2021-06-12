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
-- Module      : Network.AWS.CloudWatchEvents.Types.EcsParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EcsParameters where

import Network.AWS.CloudWatchEvents.Types.LaunchType
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The custom parameters to be used when the target is an Amazon ECS task.
--
-- /See:/ 'newEcsParameters' smart constructor.
data EcsParameters = EcsParameters'
  { -- | Use this structure if the ECS task uses the @awsvpc@ network mode. This
    -- structure specifies the VPC subnets and security groups associated with
    -- the task, and whether a public IP address is to be used. This structure
    -- is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is
    -- required for Fargate tasks.
    --
    -- If you specify @NetworkConfiguration@ when the target ECS task does not
    -- use the @awsvpc@ network mode, the task fails.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | Specifies the platform version for the task. Specify only the numeric
    -- portion of the platform version, such as @1.1.0@.
    --
    -- This structure is used only if @LaunchType@ is @FARGATE@. For more
    -- information about valid platform versions, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Core.Maybe Core.Text,
    -- | Specifies the launch type on which your task is running. The launch type
    -- that you specify here must match one of the launch type
    -- (compatibilities) of the target task. The @FARGATE@ value is supported
    -- only in the Regions where AWS Fargate with Amazon ECS is supported. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Core.Maybe LaunchType,
    -- | Specifies an ECS task group for the task. The maximum length is 255
    -- characters.
    group' :: Core.Maybe Core.Text,
    -- | The number of tasks to create based on @TaskDefinition@. The default is
    -- 1.
    taskCount :: Core.Maybe Core.Natural,
    -- | The ARN of the task definition to use if the event target is an Amazon
    -- ECS task.
    taskDefinitionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EcsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfiguration', 'ecsParameters_networkConfiguration' - Use this structure if the ECS task uses the @awsvpc@ network mode. This
-- structure specifies the VPC subnets and security groups associated with
-- the task, and whether a public IP address is to be used. This structure
-- is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is
-- required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not
-- use the @awsvpc@ network mode, the task fails.
--
-- 'platformVersion', 'ecsParameters_platformVersion' - Specifies the platform version for the task. Specify only the numeric
-- portion of the platform version, such as @1.1.0@.
--
-- This structure is used only if @LaunchType@ is @FARGATE@. For more
-- information about valid platform versions, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'launchType', 'ecsParameters_launchType' - Specifies the launch type on which your task is running. The launch type
-- that you specify here must match one of the launch type
-- (compatibilities) of the target task. The @FARGATE@ value is supported
-- only in the Regions where AWS Fargate with Amazon ECS is supported. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'group'', 'ecsParameters_group' - Specifies an ECS task group for the task. The maximum length is 255
-- characters.
--
-- 'taskCount', 'ecsParameters_taskCount' - The number of tasks to create based on @TaskDefinition@. The default is
-- 1.
--
-- 'taskDefinitionArn', 'ecsParameters_taskDefinitionArn' - The ARN of the task definition to use if the event target is an Amazon
-- ECS task.
newEcsParameters ::
  -- | 'taskDefinitionArn'
  Core.Text ->
  EcsParameters
newEcsParameters pTaskDefinitionArn_ =
  EcsParameters'
    { networkConfiguration = Core.Nothing,
      platformVersion = Core.Nothing,
      launchType = Core.Nothing,
      group' = Core.Nothing,
      taskCount = Core.Nothing,
      taskDefinitionArn = pTaskDefinitionArn_
    }

-- | Use this structure if the ECS task uses the @awsvpc@ network mode. This
-- structure specifies the VPC subnets and security groups associated with
-- the task, and whether a public IP address is to be used. This structure
-- is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is
-- required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not
-- use the @awsvpc@ network mode, the task fails.
ecsParameters_networkConfiguration :: Lens.Lens' EcsParameters (Core.Maybe NetworkConfiguration)
ecsParameters_networkConfiguration = Lens.lens (\EcsParameters' {networkConfiguration} -> networkConfiguration) (\s@EcsParameters' {} a -> s {networkConfiguration = a} :: EcsParameters)

-- | Specifies the platform version for the task. Specify only the numeric
-- portion of the platform version, such as @1.1.0@.
--
-- This structure is used only if @LaunchType@ is @FARGATE@. For more
-- information about valid platform versions, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
ecsParameters_platformVersion :: Lens.Lens' EcsParameters (Core.Maybe Core.Text)
ecsParameters_platformVersion = Lens.lens (\EcsParameters' {platformVersion} -> platformVersion) (\s@EcsParameters' {} a -> s {platformVersion = a} :: EcsParameters)

-- | Specifies the launch type on which your task is running. The launch type
-- that you specify here must match one of the launch type
-- (compatibilities) of the target task. The @FARGATE@ value is supported
-- only in the Regions where AWS Fargate with Amazon ECS is supported. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
ecsParameters_launchType :: Lens.Lens' EcsParameters (Core.Maybe LaunchType)
ecsParameters_launchType = Lens.lens (\EcsParameters' {launchType} -> launchType) (\s@EcsParameters' {} a -> s {launchType = a} :: EcsParameters)

-- | Specifies an ECS task group for the task. The maximum length is 255
-- characters.
ecsParameters_group :: Lens.Lens' EcsParameters (Core.Maybe Core.Text)
ecsParameters_group = Lens.lens (\EcsParameters' {group'} -> group') (\s@EcsParameters' {} a -> s {group' = a} :: EcsParameters)

-- | The number of tasks to create based on @TaskDefinition@. The default is
-- 1.
ecsParameters_taskCount :: Lens.Lens' EcsParameters (Core.Maybe Core.Natural)
ecsParameters_taskCount = Lens.lens (\EcsParameters' {taskCount} -> taskCount) (\s@EcsParameters' {} a -> s {taskCount = a} :: EcsParameters)

-- | The ARN of the task definition to use if the event target is an Amazon
-- ECS task.
ecsParameters_taskDefinitionArn :: Lens.Lens' EcsParameters Core.Text
ecsParameters_taskDefinitionArn = Lens.lens (\EcsParameters' {taskDefinitionArn} -> taskDefinitionArn) (\s@EcsParameters' {} a -> s {taskDefinitionArn = a} :: EcsParameters)

instance Core.FromJSON EcsParameters where
  parseJSON =
    Core.withObject
      "EcsParameters"
      ( \x ->
          EcsParameters'
            Core.<$> (x Core..:? "NetworkConfiguration")
            Core.<*> (x Core..:? "PlatformVersion")
            Core.<*> (x Core..:? "LaunchType")
            Core.<*> (x Core..:? "Group")
            Core.<*> (x Core..:? "TaskCount")
            Core.<*> (x Core..: "TaskDefinitionArn")
      )

instance Core.Hashable EcsParameters

instance Core.NFData EcsParameters

instance Core.ToJSON EcsParameters where
  toJSON EcsParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NetworkConfiguration" Core..=)
              Core.<$> networkConfiguration,
            ("PlatformVersion" Core..=) Core.<$> platformVersion,
            ("LaunchType" Core..=) Core.<$> launchType,
            ("Group" Core..=) Core.<$> group',
            ("TaskCount" Core..=) Core.<$> taskCount,
            Core.Just
              ("TaskDefinitionArn" Core..= taskDefinitionArn)
          ]
      )
