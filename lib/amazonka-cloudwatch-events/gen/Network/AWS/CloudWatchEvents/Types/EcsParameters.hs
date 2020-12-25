{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EcsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EcsParameters
  ( EcsParameters (..),

    -- * Smart constructor
    mkEcsParameters,

    -- * Lenses
    epTaskDefinitionArn,
    epGroup,
    epLaunchType,
    epNetworkConfiguration,
    epPlatformVersion,
    epTaskCount,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.Arn as Types
import qualified Network.AWS.CloudWatchEvents.Types.LaunchType as Types
import qualified Network.AWS.CloudWatchEvents.Types.NetworkConfiguration as Types
import qualified Network.AWS.CloudWatchEvents.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The custom parameters to be used when the target is an Amazon ECS task.
--
-- /See:/ 'mkEcsParameters' smart constructor.
data EcsParameters = EcsParameters'
  { -- | The ARN of the task definition to use if the event target is an Amazon ECS task.
    taskDefinitionArn :: Types.Arn,
    -- | Specifies an ECS task group for the task. The maximum length is 255 characters.
    group :: Core.Maybe Types.String,
    -- | Specifies the launch type on which your task is running. The launch type that you specify here must match one of the launch type (compatibilities) of the target task. The @FARGATE@ value is supported only in the Regions where AWS Fargate with Amazon ECS is supported. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
    launchType :: Core.Maybe Types.LaunchType,
    -- | Use this structure if the ECS task uses the @awsvpc@ network mode. This structure specifies the VPC subnets and security groups associated with the task, and whether a public IP address is to be used. This structure is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is required for Fargate tasks.
    --
    -- If you specify @NetworkConfiguration@ when the target ECS task does not use the @awsvpc@ network mode, the task fails.
    networkConfiguration :: Core.Maybe Types.NetworkConfiguration,
    -- | Specifies the platform version for the task. Specify only the numeric portion of the platform version, such as @1.1.0@ .
    --
    -- This structure is used only if @LaunchType@ is @FARGATE@ . For more information about valid platform versions, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Core.Maybe Types.String,
    -- | The number of tasks to create based on @TaskDefinition@ . The default is 1.
    taskCount :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EcsParameters' value with any optional fields omitted.
mkEcsParameters ::
  -- | 'taskDefinitionArn'
  Types.Arn ->
  EcsParameters
mkEcsParameters taskDefinitionArn =
  EcsParameters'
    { taskDefinitionArn,
      group = Core.Nothing,
      launchType = Core.Nothing,
      networkConfiguration = Core.Nothing,
      platformVersion = Core.Nothing,
      taskCount = Core.Nothing
    }

-- | The ARN of the task definition to use if the event target is an Amazon ECS task.
--
-- /Note:/ Consider using 'taskDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTaskDefinitionArn :: Lens.Lens' EcsParameters Types.Arn
epTaskDefinitionArn = Lens.field @"taskDefinitionArn"
{-# DEPRECATED epTaskDefinitionArn "Use generic-lens or generic-optics with 'taskDefinitionArn' instead." #-}

-- | Specifies an ECS task group for the task. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epGroup :: Lens.Lens' EcsParameters (Core.Maybe Types.String)
epGroup = Lens.field @"group"
{-# DEPRECATED epGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Specifies the launch type on which your task is running. The launch type that you specify here must match one of the launch type (compatibilities) of the target task. The @FARGATE@ value is supported only in the Regions where AWS Fargate with Amazon ECS is supported. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLaunchType :: Lens.Lens' EcsParameters (Core.Maybe Types.LaunchType)
epLaunchType = Lens.field @"launchType"
{-# DEPRECATED epLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | Use this structure if the ECS task uses the @awsvpc@ network mode. This structure specifies the VPC subnets and security groups associated with the task, and whether a public IP address is to be used. This structure is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not use the @awsvpc@ network mode, the task fails.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epNetworkConfiguration :: Lens.Lens' EcsParameters (Core.Maybe Types.NetworkConfiguration)
epNetworkConfiguration = Lens.field @"networkConfiguration"
{-# DEPRECATED epNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | Specifies the platform version for the task. Specify only the numeric portion of the platform version, such as @1.1.0@ .
--
-- This structure is used only if @LaunchType@ is @FARGATE@ . For more information about valid platform versions, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPlatformVersion :: Lens.Lens' EcsParameters (Core.Maybe Types.String)
epPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED epPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The number of tasks to create based on @TaskDefinition@ . The default is 1.
--
-- /Note:/ Consider using 'taskCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTaskCount :: Lens.Lens' EcsParameters (Core.Maybe Core.Natural)
epTaskCount = Lens.field @"taskCount"
{-# DEPRECATED epTaskCount "Use generic-lens or generic-optics with 'taskCount' instead." #-}

instance Core.FromJSON EcsParameters where
  toJSON EcsParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TaskDefinitionArn" Core..= taskDefinitionArn),
            ("Group" Core..=) Core.<$> group,
            ("LaunchType" Core..=) Core.<$> launchType,
            ("NetworkConfiguration" Core..=) Core.<$> networkConfiguration,
            ("PlatformVersion" Core..=) Core.<$> platformVersion,
            ("TaskCount" Core..=) Core.<$> taskCount
          ]
      )

instance Core.FromJSON EcsParameters where
  parseJSON =
    Core.withObject "EcsParameters" Core.$
      \x ->
        EcsParameters'
          Core.<$> (x Core..: "TaskDefinitionArn")
          Core.<*> (x Core..:? "Group")
          Core.<*> (x Core..:? "LaunchType")
          Core.<*> (x Core..:? "NetworkConfiguration")
          Core.<*> (x Core..:? "PlatformVersion")
          Core.<*> (x Core..:? "TaskCount")
