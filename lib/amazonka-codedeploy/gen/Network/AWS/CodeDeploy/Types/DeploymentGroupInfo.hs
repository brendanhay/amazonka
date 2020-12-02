{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentGroupInfo where

import Network.AWS.CodeDeploy.Types.AlarmConfiguration
import Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
import Network.AWS.CodeDeploy.Types.AutoScalingGroup
import Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.DeploymentStyle
import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import Network.AWS.CodeDeploy.Types.ECSService
import Network.AWS.CodeDeploy.Types.LastDeploymentInfo
import Network.AWS.CodeDeploy.Types.LoadBalancerInfo
import Network.AWS.CodeDeploy.Types.OnPremisesTagSet
import Network.AWS.CodeDeploy.Types.RevisionLocation
import Network.AWS.CodeDeploy.Types.TagFilter
import Network.AWS.CodeDeploy.Types.TriggerConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment group.
--
--
--
-- /See:/ 'deploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { _dgiServiceRoleARN ::
      !(Maybe Text),
    _dgiEc2TagSet :: !(Maybe EC2TagSet),
    _dgiDeploymentConfigName :: !(Maybe Text),
    _dgiLastAttemptedDeployment ::
      !(Maybe LastDeploymentInfo),
    _dgiOnPremisesTagSet :: !(Maybe OnPremisesTagSet),
    _dgiComputePlatform :: !(Maybe ComputePlatform),
    _dgiTargetRevision :: !(Maybe RevisionLocation),
    _dgiEc2TagFilters :: !(Maybe [EC2TagFilter]),
    _dgiEcsServices :: !(Maybe [ECSService]),
    _dgiBlueGreenDeploymentConfiguration ::
      !(Maybe BlueGreenDeploymentConfiguration),
    _dgiLoadBalancerInfo :: !(Maybe LoadBalancerInfo),
    _dgiOnPremisesInstanceTagFilters ::
      !(Maybe [TagFilter]),
    _dgiLastSuccessfulDeployment ::
      !(Maybe LastDeploymentInfo),
    _dgiApplicationName :: !(Maybe Text),
    _dgiAlarmConfiguration ::
      !(Maybe AlarmConfiguration),
    _dgiTriggerConfigurations ::
      !(Maybe [TriggerConfig]),
    _dgiDeploymentGroupId :: !(Maybe Text),
    _dgiAutoScalingGroups ::
      !(Maybe [AutoScalingGroup]),
    _dgiDeploymentStyle :: !(Maybe DeploymentStyle),
    _dgiAutoRollbackConfiguration ::
      !(Maybe AutoRollbackConfiguration),
    _dgiDeploymentGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentGroupInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgiServiceRoleARN' - A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
--
-- * 'dgiEc2TagSet' - Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- * 'dgiDeploymentConfigName' - The deployment configuration name.
--
-- * 'dgiLastAttemptedDeployment' - Information about the most recent attempted deployment to the deployment group.
--
-- * 'dgiOnPremisesTagSet' - Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
--
-- * 'dgiComputePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- * 'dgiTargetRevision' - Information about the deployment group's target revision, including type and location.
--
-- * 'dgiEc2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
--
-- * 'dgiEcsServices' - The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
--
-- * 'dgiBlueGreenDeploymentConfiguration' - Information about blue/green deployment options for a deployment group.
--
-- * 'dgiLoadBalancerInfo' - Information about the load balancer to use in a deployment.
--
-- * 'dgiOnPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
--
-- * 'dgiLastSuccessfulDeployment' - Information about the most recent successful deployment to the deployment group.
--
-- * 'dgiApplicationName' - The application name.
--
-- * 'dgiAlarmConfiguration' - A list of alarms associated with the deployment group.
--
-- * 'dgiTriggerConfigurations' - Information about triggers associated with the deployment group.
--
-- * 'dgiDeploymentGroupId' - The deployment group ID.
--
-- * 'dgiAutoScalingGroups' - A list of associated Auto Scaling groups.
--
-- * 'dgiDeploymentStyle' - Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- * 'dgiAutoRollbackConfiguration' - Information about the automatic rollback configuration associated with the deployment group.
--
-- * 'dgiDeploymentGroupName' - The deployment group name.
deploymentGroupInfo ::
  DeploymentGroupInfo
deploymentGroupInfo =
  DeploymentGroupInfo'
    { _dgiServiceRoleARN = Nothing,
      _dgiEc2TagSet = Nothing,
      _dgiDeploymentConfigName = Nothing,
      _dgiLastAttemptedDeployment = Nothing,
      _dgiOnPremisesTagSet = Nothing,
      _dgiComputePlatform = Nothing,
      _dgiTargetRevision = Nothing,
      _dgiEc2TagFilters = Nothing,
      _dgiEcsServices = Nothing,
      _dgiBlueGreenDeploymentConfiguration = Nothing,
      _dgiLoadBalancerInfo = Nothing,
      _dgiOnPremisesInstanceTagFilters = Nothing,
      _dgiLastSuccessfulDeployment = Nothing,
      _dgiApplicationName = Nothing,
      _dgiAlarmConfiguration = Nothing,
      _dgiTriggerConfigurations = Nothing,
      _dgiDeploymentGroupId = Nothing,
      _dgiAutoScalingGroups = Nothing,
      _dgiDeploymentStyle = Nothing,
      _dgiAutoRollbackConfiguration = Nothing,
      _dgiDeploymentGroupName = Nothing
    }

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
dgiServiceRoleARN :: Lens' DeploymentGroupInfo (Maybe Text)
dgiServiceRoleARN = lens _dgiServiceRoleARN (\s a -> s {_dgiServiceRoleARN = a})

-- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
dgiEc2TagSet :: Lens' DeploymentGroupInfo (Maybe EC2TagSet)
dgiEc2TagSet = lens _dgiEc2TagSet (\s a -> s {_dgiEc2TagSet = a})

-- | The deployment configuration name.
dgiDeploymentConfigName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentConfigName = lens _dgiDeploymentConfigName (\s a -> s {_dgiDeploymentConfigName = a})

-- | Information about the most recent attempted deployment to the deployment group.
dgiLastAttemptedDeployment :: Lens' DeploymentGroupInfo (Maybe LastDeploymentInfo)
dgiLastAttemptedDeployment = lens _dgiLastAttemptedDeployment (\s a -> s {_dgiLastAttemptedDeployment = a})

-- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
dgiOnPremisesTagSet :: Lens' DeploymentGroupInfo (Maybe OnPremisesTagSet)
dgiOnPremisesTagSet = lens _dgiOnPremisesTagSet (\s a -> s {_dgiOnPremisesTagSet = a})

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
dgiComputePlatform :: Lens' DeploymentGroupInfo (Maybe ComputePlatform)
dgiComputePlatform = lens _dgiComputePlatform (\s a -> s {_dgiComputePlatform = a})

-- | Information about the deployment group's target revision, including type and location.
dgiTargetRevision :: Lens' DeploymentGroupInfo (Maybe RevisionLocation)
dgiTargetRevision = lens _dgiTargetRevision (\s a -> s {_dgiTargetRevision = a})

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
dgiEc2TagFilters :: Lens' DeploymentGroupInfo [EC2TagFilter]
dgiEc2TagFilters = lens _dgiEc2TagFilters (\s a -> s {_dgiEc2TagFilters = a}) . _Default . _Coerce

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
dgiEcsServices :: Lens' DeploymentGroupInfo [ECSService]
dgiEcsServices = lens _dgiEcsServices (\s a -> s {_dgiEcsServices = a}) . _Default . _Coerce

-- | Information about blue/green deployment options for a deployment group.
dgiBlueGreenDeploymentConfiguration :: Lens' DeploymentGroupInfo (Maybe BlueGreenDeploymentConfiguration)
dgiBlueGreenDeploymentConfiguration = lens _dgiBlueGreenDeploymentConfiguration (\s a -> s {_dgiBlueGreenDeploymentConfiguration = a})

-- | Information about the load balancer to use in a deployment.
dgiLoadBalancerInfo :: Lens' DeploymentGroupInfo (Maybe LoadBalancerInfo)
dgiLoadBalancerInfo = lens _dgiLoadBalancerInfo (\s a -> s {_dgiLoadBalancerInfo = a})

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
dgiOnPremisesInstanceTagFilters :: Lens' DeploymentGroupInfo [TagFilter]
dgiOnPremisesInstanceTagFilters = lens _dgiOnPremisesInstanceTagFilters (\s a -> s {_dgiOnPremisesInstanceTagFilters = a}) . _Default . _Coerce

-- | Information about the most recent successful deployment to the deployment group.
dgiLastSuccessfulDeployment :: Lens' DeploymentGroupInfo (Maybe LastDeploymentInfo)
dgiLastSuccessfulDeployment = lens _dgiLastSuccessfulDeployment (\s a -> s {_dgiLastSuccessfulDeployment = a})

-- | The application name.
dgiApplicationName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiApplicationName = lens _dgiApplicationName (\s a -> s {_dgiApplicationName = a})

-- | A list of alarms associated with the deployment group.
dgiAlarmConfiguration :: Lens' DeploymentGroupInfo (Maybe AlarmConfiguration)
dgiAlarmConfiguration = lens _dgiAlarmConfiguration (\s a -> s {_dgiAlarmConfiguration = a})

-- | Information about triggers associated with the deployment group.
dgiTriggerConfigurations :: Lens' DeploymentGroupInfo [TriggerConfig]
dgiTriggerConfigurations = lens _dgiTriggerConfigurations (\s a -> s {_dgiTriggerConfigurations = a}) . _Default . _Coerce

-- | The deployment group ID.
dgiDeploymentGroupId :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupId = lens _dgiDeploymentGroupId (\s a -> s {_dgiDeploymentGroupId = a})

-- | A list of associated Auto Scaling groups.
dgiAutoScalingGroups :: Lens' DeploymentGroupInfo [AutoScalingGroup]
dgiAutoScalingGroups = lens _dgiAutoScalingGroups (\s a -> s {_dgiAutoScalingGroups = a}) . _Default . _Coerce

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
dgiDeploymentStyle :: Lens' DeploymentGroupInfo (Maybe DeploymentStyle)
dgiDeploymentStyle = lens _dgiDeploymentStyle (\s a -> s {_dgiDeploymentStyle = a})

-- | Information about the automatic rollback configuration associated with the deployment group.
dgiAutoRollbackConfiguration :: Lens' DeploymentGroupInfo (Maybe AutoRollbackConfiguration)
dgiAutoRollbackConfiguration = lens _dgiAutoRollbackConfiguration (\s a -> s {_dgiAutoRollbackConfiguration = a})

-- | The deployment group name.
dgiDeploymentGroupName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupName = lens _dgiDeploymentGroupName (\s a -> s {_dgiDeploymentGroupName = a})

instance FromJSON DeploymentGroupInfo where
  parseJSON =
    withObject
      "DeploymentGroupInfo"
      ( \x ->
          DeploymentGroupInfo'
            <$> (x .:? "serviceRoleArn")
            <*> (x .:? "ec2TagSet")
            <*> (x .:? "deploymentConfigName")
            <*> (x .:? "lastAttemptedDeployment")
            <*> (x .:? "onPremisesTagSet")
            <*> (x .:? "computePlatform")
            <*> (x .:? "targetRevision")
            <*> (x .:? "ec2TagFilters" .!= mempty)
            <*> (x .:? "ecsServices" .!= mempty)
            <*> (x .:? "blueGreenDeploymentConfiguration")
            <*> (x .:? "loadBalancerInfo")
            <*> (x .:? "onPremisesInstanceTagFilters" .!= mempty)
            <*> (x .:? "lastSuccessfulDeployment")
            <*> (x .:? "applicationName")
            <*> (x .:? "alarmConfiguration")
            <*> (x .:? "triggerConfigurations" .!= mempty)
            <*> (x .:? "deploymentGroupId")
            <*> (x .:? "autoScalingGroups" .!= mempty)
            <*> (x .:? "deploymentStyle")
            <*> (x .:? "autoRollbackConfiguration")
            <*> (x .:? "deploymentGroupName")
      )

instance Hashable DeploymentGroupInfo

instance NFData DeploymentGroupInfo
