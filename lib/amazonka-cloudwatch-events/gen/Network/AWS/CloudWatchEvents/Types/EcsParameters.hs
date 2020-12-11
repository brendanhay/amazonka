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
    epGroup,
    epPlatformVersion,
    epLaunchType,
    epTaskCount,
    epNetworkConfiguration,
    epTaskDefinitionARN,
  )
where

import Network.AWS.CloudWatchEvents.Types.LaunchType
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The custom parameters to be used when the target is an Amazon ECS task.
--
-- /See:/ 'mkEcsParameters' smart constructor.
data EcsParameters = EcsParameters'
  { group :: Lude.Maybe Lude.Text,
    platformVersion :: Lude.Maybe Lude.Text,
    launchType :: Lude.Maybe LaunchType,
    taskCount :: Lude.Maybe Lude.Natural,
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    taskDefinitionARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EcsParameters' with the minimum fields required to make a request.
--
-- * 'group' - Specifies an ECS task group for the task. The maximum length is 255 characters.
-- * 'launchType' - Specifies the launch type on which your task is running. The launch type that you specify here must match one of the launch type (compatibilities) of the target task. The @FARGATE@ value is supported only in the Regions where AWS Fargate with Amazon ECS is supported. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'networkConfiguration' - Use this structure if the ECS task uses the @awsvpc@ network mode. This structure specifies the VPC subnets and security groups associated with the task, and whether a public IP address is to be used. This structure is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not use the @awsvpc@ network mode, the task fails.
-- * 'platformVersion' - Specifies the platform version for the task. Specify only the numeric portion of the platform version, such as @1.1.0@ .
--
-- This structure is used only if @LaunchType@ is @FARGATE@ . For more information about valid platform versions, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'taskCount' - The number of tasks to create based on @TaskDefinition@ . The default is 1.
-- * 'taskDefinitionARN' - The ARN of the task definition to use if the event target is an Amazon ECS task.
mkEcsParameters ::
  -- | 'taskDefinitionARN'
  Lude.Text ->
  EcsParameters
mkEcsParameters pTaskDefinitionARN_ =
  EcsParameters'
    { group = Lude.Nothing,
      platformVersion = Lude.Nothing,
      launchType = Lude.Nothing,
      taskCount = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      taskDefinitionARN = pTaskDefinitionARN_
    }

-- | Specifies an ECS task group for the task. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epGroup :: Lens.Lens' EcsParameters (Lude.Maybe Lude.Text)
epGroup = Lens.lens (group :: EcsParameters -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: EcsParameters)
{-# DEPRECATED epGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Specifies the platform version for the task. Specify only the numeric portion of the platform version, such as @1.1.0@ .
--
-- This structure is used only if @LaunchType@ is @FARGATE@ . For more information about valid platform versions, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPlatformVersion :: Lens.Lens' EcsParameters (Lude.Maybe Lude.Text)
epPlatformVersion = Lens.lens (platformVersion :: EcsParameters -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: EcsParameters)
{-# DEPRECATED epPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | Specifies the launch type on which your task is running. The launch type that you specify here must match one of the launch type (compatibilities) of the target task. The @FARGATE@ value is supported only in the Regions where AWS Fargate with Amazon ECS is supported. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLaunchType :: Lens.Lens' EcsParameters (Lude.Maybe LaunchType)
epLaunchType = Lens.lens (launchType :: EcsParameters -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: EcsParameters)
{-# DEPRECATED epLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The number of tasks to create based on @TaskDefinition@ . The default is 1.
--
-- /Note:/ Consider using 'taskCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTaskCount :: Lens.Lens' EcsParameters (Lude.Maybe Lude.Natural)
epTaskCount = Lens.lens (taskCount :: EcsParameters -> Lude.Maybe Lude.Natural) (\s a -> s {taskCount = a} :: EcsParameters)
{-# DEPRECATED epTaskCount "Use generic-lens or generic-optics with 'taskCount' instead." #-}

-- | Use this structure if the ECS task uses the @awsvpc@ network mode. This structure specifies the VPC subnets and security groups associated with the task, and whether a public IP address is to be used. This structure is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is required for Fargate tasks.
--
-- If you specify @NetworkConfiguration@ when the target ECS task does not use the @awsvpc@ network mode, the task fails.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epNetworkConfiguration :: Lens.Lens' EcsParameters (Lude.Maybe NetworkConfiguration)
epNetworkConfiguration = Lens.lens (networkConfiguration :: EcsParameters -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: EcsParameters)
{-# DEPRECATED epNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The ARN of the task definition to use if the event target is an Amazon ECS task.
--
-- /Note:/ Consider using 'taskDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTaskDefinitionARN :: Lens.Lens' EcsParameters Lude.Text
epTaskDefinitionARN = Lens.lens (taskDefinitionARN :: EcsParameters -> Lude.Text) (\s a -> s {taskDefinitionARN = a} :: EcsParameters)
{-# DEPRECATED epTaskDefinitionARN "Use generic-lens or generic-optics with 'taskDefinitionARN' instead." #-}

instance Lude.FromJSON EcsParameters where
  parseJSON =
    Lude.withObject
      "EcsParameters"
      ( \x ->
          EcsParameters'
            Lude.<$> (x Lude..:? "Group")
            Lude.<*> (x Lude..:? "PlatformVersion")
            Lude.<*> (x Lude..:? "LaunchType")
            Lude.<*> (x Lude..:? "TaskCount")
            Lude.<*> (x Lude..:? "NetworkConfiguration")
            Lude.<*> (x Lude..: "TaskDefinitionArn")
      )

instance Lude.ToJSON EcsParameters where
  toJSON EcsParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            ("PlatformVersion" Lude..=) Lude.<$> platformVersion,
            ("LaunchType" Lude..=) Lude.<$> launchType,
            ("TaskCount" Lude..=) Lude.<$> taskCount,
            ("NetworkConfiguration" Lude..=) Lude.<$> networkConfiguration,
            Lude.Just ("TaskDefinitionArn" Lude..= taskDefinitionARN)
          ]
      )
