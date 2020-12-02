{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EcsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EcsParameters where

import Network.AWS.CloudWatchEvents.Types.LaunchType
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The custom parameters to be used when the target is an Amazon ECS task.
--
--
--
-- /See:/ 'ecsParameters' smart constructor.
data EcsParameters = EcsParameters'
  { _epGroup :: !(Maybe Text),
    _epPlatformVersion :: !(Maybe Text),
    _epLaunchType :: !(Maybe LaunchType),
    _epTaskCount :: !(Maybe Nat),
    _epNetworkConfiguration :: !(Maybe NetworkConfiguration),
    _epTaskDefinitionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EcsParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epGroup' - Specifies an ECS task group for the task. The maximum length is 255 characters.
--
-- * 'epPlatformVersion' - Specifies the platform version for the task. Specify only the numeric portion of the platform version, such as @1.1.0@ . This structure is used only if @LaunchType@ is @FARGATE@ . For more information about valid platform versions, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'epLaunchType' - Specifies the launch type on which your task is running. The launch type that you specify here must match one of the launch type (compatibilities) of the target task. The @FARGATE@ value is supported only in the Regions where AWS Fargate with Amazon ECS is supported. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'epTaskCount' - The number of tasks to create based on @TaskDefinition@ . The default is 1.
--
-- * 'epNetworkConfiguration' - Use this structure if the ECS task uses the @awsvpc@ network mode. This structure specifies the VPC subnets and security groups associated with the task, and whether a public IP address is to be used. This structure is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is required for Fargate tasks. If you specify @NetworkConfiguration@ when the target ECS task does not use the @awsvpc@ network mode, the task fails.
--
-- * 'epTaskDefinitionARN' - The ARN of the task definition to use if the event target is an Amazon ECS task.
ecsParameters ::
  -- | 'epTaskDefinitionARN'
  Text ->
  EcsParameters
ecsParameters pTaskDefinitionARN_ =
  EcsParameters'
    { _epGroup = Nothing,
      _epPlatformVersion = Nothing,
      _epLaunchType = Nothing,
      _epTaskCount = Nothing,
      _epNetworkConfiguration = Nothing,
      _epTaskDefinitionARN = pTaskDefinitionARN_
    }

-- | Specifies an ECS task group for the task. The maximum length is 255 characters.
epGroup :: Lens' EcsParameters (Maybe Text)
epGroup = lens _epGroup (\s a -> s {_epGroup = a})

-- | Specifies the platform version for the task. Specify only the numeric portion of the platform version, such as @1.1.0@ . This structure is used only if @LaunchType@ is @FARGATE@ . For more information about valid platform versions, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
epPlatformVersion :: Lens' EcsParameters (Maybe Text)
epPlatformVersion = lens _epPlatformVersion (\s a -> s {_epPlatformVersion = a})

-- | Specifies the launch type on which your task is running. The launch type that you specify here must match one of the launch type (compatibilities) of the target task. The @FARGATE@ value is supported only in the Regions where AWS Fargate with Amazon ECS is supported. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/AWS-Fargate.html AWS Fargate on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
epLaunchType :: Lens' EcsParameters (Maybe LaunchType)
epLaunchType = lens _epLaunchType (\s a -> s {_epLaunchType = a})

-- | The number of tasks to create based on @TaskDefinition@ . The default is 1.
epTaskCount :: Lens' EcsParameters (Maybe Natural)
epTaskCount = lens _epTaskCount (\s a -> s {_epTaskCount = a}) . mapping _Nat

-- | Use this structure if the ECS task uses the @awsvpc@ network mode. This structure specifies the VPC subnets and security groups associated with the task, and whether a public IP address is to be used. This structure is required if @LaunchType@ is @FARGATE@ because the @awsvpc@ mode is required for Fargate tasks. If you specify @NetworkConfiguration@ when the target ECS task does not use the @awsvpc@ network mode, the task fails.
epNetworkConfiguration :: Lens' EcsParameters (Maybe NetworkConfiguration)
epNetworkConfiguration = lens _epNetworkConfiguration (\s a -> s {_epNetworkConfiguration = a})

-- | The ARN of the task definition to use if the event target is an Amazon ECS task.
epTaskDefinitionARN :: Lens' EcsParameters Text
epTaskDefinitionARN = lens _epTaskDefinitionARN (\s a -> s {_epTaskDefinitionARN = a})

instance FromJSON EcsParameters where
  parseJSON =
    withObject
      "EcsParameters"
      ( \x ->
          EcsParameters'
            <$> (x .:? "Group")
            <*> (x .:? "PlatformVersion")
            <*> (x .:? "LaunchType")
            <*> (x .:? "TaskCount")
            <*> (x .:? "NetworkConfiguration")
            <*> (x .: "TaskDefinitionArn")
      )

instance Hashable EcsParameters

instance NFData EcsParameters

instance ToJSON EcsParameters where
  toJSON EcsParameters' {..} =
    object
      ( catMaybes
          [ ("Group" .=) <$> _epGroup,
            ("PlatformVersion" .=) <$> _epPlatformVersion,
            ("LaunchType" .=) <$> _epLaunchType,
            ("TaskCount" .=) <$> _epTaskCount,
            ("NetworkConfiguration" .=) <$> _epNetworkConfiguration,
            Just ("TaskDefinitionArn" .= _epTaskDefinitionARN)
          ]
      )
