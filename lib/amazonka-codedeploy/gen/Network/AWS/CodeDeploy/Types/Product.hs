{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.Product where

import Network.AWS.CodeDeploy.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an alarm.
--
--
--
-- /See:/ 'alarm' smart constructor.
newtype Alarm = Alarm'
  { _aName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aName' - The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
alarm
    :: Alarm
alarm = Alarm' {_aName = Nothing}


-- | The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
aName :: Lens' Alarm (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

instance FromJSON Alarm where
        parseJSON
          = withObject "Alarm"
              (\ x -> Alarm' <$> (x .:? "name"))

instance Hashable Alarm where

instance NFData Alarm where

instance ToJSON Alarm where
        toJSON Alarm'{..}
          = object (catMaybes [("name" .=) <$> _aName])

-- | Information about alarms associated with the deployment group.
--
--
--
-- /See:/ 'alarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { _acIgnorePollAlarmFailure :: !(Maybe Bool)
  , _acEnabled                :: !(Maybe Bool)
  , _acAlarms                 :: !(Maybe [Alarm])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlarmConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acIgnorePollAlarmFailure' - Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.     * true: The deployment will proceed even if alarm status information can't be retrieved from Amazon CloudWatch.     * false: The deployment will stop if alarm status information can't be retrieved from Amazon CloudWatch.
--
-- * 'acEnabled' - Indicates whether the alarm configuration is enabled.
--
-- * 'acAlarms' - A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
alarmConfiguration
    :: AlarmConfiguration
alarmConfiguration =
  AlarmConfiguration'
    { _acIgnorePollAlarmFailure = Nothing
    , _acEnabled = Nothing
    , _acAlarms = Nothing
    }


-- | Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.     * true: The deployment will proceed even if alarm status information can't be retrieved from Amazon CloudWatch.     * false: The deployment will stop if alarm status information can't be retrieved from Amazon CloudWatch.
acIgnorePollAlarmFailure :: Lens' AlarmConfiguration (Maybe Bool)
acIgnorePollAlarmFailure = lens _acIgnorePollAlarmFailure (\ s a -> s{_acIgnorePollAlarmFailure = a})

-- | Indicates whether the alarm configuration is enabled.
acEnabled :: Lens' AlarmConfiguration (Maybe Bool)
acEnabled = lens _acEnabled (\ s a -> s{_acEnabled = a})

-- | A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
acAlarms :: Lens' AlarmConfiguration [Alarm]
acAlarms = lens _acAlarms (\ s a -> s{_acAlarms = a}) . _Default . _Coerce

instance FromJSON AlarmConfiguration where
        parseJSON
          = withObject "AlarmConfiguration"
              (\ x ->
                 AlarmConfiguration' <$>
                   (x .:? "ignorePollAlarmFailure") <*>
                     (x .:? "enabled")
                     <*> (x .:? "alarms" .!= mempty))

instance Hashable AlarmConfiguration where

instance NFData AlarmConfiguration where

instance ToJSON AlarmConfiguration where
        toJSON AlarmConfiguration'{..}
          = object
              (catMaybes
                 [("ignorePollAlarmFailure" .=) <$>
                    _acIgnorePollAlarmFailure,
                  ("enabled" .=) <$> _acEnabled,
                  ("alarms" .=) <$> _acAlarms])

-- | Information about an application.
--
--
--
-- /See:/ 'applicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { _aiLinkedToGitHub    :: !(Maybe Bool)
  , _aiComputePlatform   :: !(Maybe ComputePlatform)
  , _aiApplicationId     :: !(Maybe Text)
  , _aiApplicationName   :: !(Maybe Text)
  , _aiGitHubAccountName :: !(Maybe Text)
  , _aiCreateTime        :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiLinkedToGitHub' - True if the user has authenticated with GitHub for the specified application; otherwise, false.
--
-- * 'aiComputePlatform' - The destination platform type for deployment of the application (@Lambda@ or @Server@ ).
--
-- * 'aiApplicationId' - The application ID.
--
-- * 'aiApplicationName' - The application name.
--
-- * 'aiGitHubAccountName' - The name for a connection to a GitHub account.
--
-- * 'aiCreateTime' - The time at which the application was created.
applicationInfo
    :: ApplicationInfo
applicationInfo =
  ApplicationInfo'
    { _aiLinkedToGitHub = Nothing
    , _aiComputePlatform = Nothing
    , _aiApplicationId = Nothing
    , _aiApplicationName = Nothing
    , _aiGitHubAccountName = Nothing
    , _aiCreateTime = Nothing
    }


-- | True if the user has authenticated with GitHub for the specified application; otherwise, false.
aiLinkedToGitHub :: Lens' ApplicationInfo (Maybe Bool)
aiLinkedToGitHub = lens _aiLinkedToGitHub (\ s a -> s{_aiLinkedToGitHub = a})

-- | The destination platform type for deployment of the application (@Lambda@ or @Server@ ).
aiComputePlatform :: Lens' ApplicationInfo (Maybe ComputePlatform)
aiComputePlatform = lens _aiComputePlatform (\ s a -> s{_aiComputePlatform = a})

-- | The application ID.
aiApplicationId :: Lens' ApplicationInfo (Maybe Text)
aiApplicationId = lens _aiApplicationId (\ s a -> s{_aiApplicationId = a})

-- | The application name.
aiApplicationName :: Lens' ApplicationInfo (Maybe Text)
aiApplicationName = lens _aiApplicationName (\ s a -> s{_aiApplicationName = a})

-- | The name for a connection to a GitHub account.
aiGitHubAccountName :: Lens' ApplicationInfo (Maybe Text)
aiGitHubAccountName = lens _aiGitHubAccountName (\ s a -> s{_aiGitHubAccountName = a})

-- | The time at which the application was created.
aiCreateTime :: Lens' ApplicationInfo (Maybe UTCTime)
aiCreateTime = lens _aiCreateTime (\ s a -> s{_aiCreateTime = a}) . mapping _Time

instance FromJSON ApplicationInfo where
        parseJSON
          = withObject "ApplicationInfo"
              (\ x ->
                 ApplicationInfo' <$>
                   (x .:? "linkedToGitHub") <*>
                     (x .:? "computePlatform")
                     <*> (x .:? "applicationId")
                     <*> (x .:? "applicationName")
                     <*> (x .:? "gitHubAccountName")
                     <*> (x .:? "createTime"))

instance Hashable ApplicationInfo where

instance NFData ApplicationInfo where

-- | Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment doesn't complete successfully.
--
--
--
-- /See:/ 'autoRollbackConfiguration' smart constructor.
data AutoRollbackConfiguration = AutoRollbackConfiguration'
  { _arcEnabled :: !(Maybe Bool)
  , _arcEvents  :: !(Maybe [AutoRollbackEvent])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoRollbackConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arcEnabled' - Indicates whether a defined automatic rollback configuration is currently enabled.
--
-- * 'arcEvents' - The event type or types that trigger a rollback.
autoRollbackConfiguration
    :: AutoRollbackConfiguration
autoRollbackConfiguration =
  AutoRollbackConfiguration' {_arcEnabled = Nothing, _arcEvents = Nothing}


-- | Indicates whether a defined automatic rollback configuration is currently enabled.
arcEnabled :: Lens' AutoRollbackConfiguration (Maybe Bool)
arcEnabled = lens _arcEnabled (\ s a -> s{_arcEnabled = a})

-- | The event type or types that trigger a rollback.
arcEvents :: Lens' AutoRollbackConfiguration [AutoRollbackEvent]
arcEvents = lens _arcEvents (\ s a -> s{_arcEvents = a}) . _Default . _Coerce

instance FromJSON AutoRollbackConfiguration where
        parseJSON
          = withObject "AutoRollbackConfiguration"
              (\ x ->
                 AutoRollbackConfiguration' <$>
                   (x .:? "enabled") <*> (x .:? "events" .!= mempty))

instance Hashable AutoRollbackConfiguration where

instance NFData AutoRollbackConfiguration where

instance ToJSON AutoRollbackConfiguration where
        toJSON AutoRollbackConfiguration'{..}
          = object
              (catMaybes
                 [("enabled" .=) <$> _arcEnabled,
                  ("events" .=) <$> _arcEvents])

-- | Information about an Auto Scaling group.
--
--
--
-- /See:/ 'autoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { _asgHook :: !(Maybe Text)
  , _asgName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgHook' - An Auto Scaling lifecycle event hook name.
--
-- * 'asgName' - The Auto Scaling group name.
autoScalingGroup
    :: AutoScalingGroup
autoScalingGroup = AutoScalingGroup' {_asgHook = Nothing, _asgName = Nothing}


-- | An Auto Scaling lifecycle event hook name.
asgHook :: Lens' AutoScalingGroup (Maybe Text)
asgHook = lens _asgHook (\ s a -> s{_asgHook = a})

-- | The Auto Scaling group name.
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\ s a -> s{_asgName = a})

instance FromJSON AutoScalingGroup where
        parseJSON
          = withObject "AutoScalingGroup"
              (\ x ->
                 AutoScalingGroup' <$>
                   (x .:? "hook") <*> (x .:? "name"))

instance Hashable AutoScalingGroup where

instance NFData AutoScalingGroup where

-- | Information about blue/green deployment options for a deployment group.
--
--
--
-- /See:/ 'blueGreenDeploymentConfiguration' smart constructor.
data BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration'
  { _bgdcDeploymentReadyOption :: !(Maybe DeploymentReadyOption)
  , _bgdcGreenFleetProvisioningOption :: !(Maybe GreenFleetProvisioningOption)
  , _bgdcTerminateBlueInstancesOnDeploymentSuccess :: !(Maybe BlueInstanceTerminationOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlueGreenDeploymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdcDeploymentReadyOption' - Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
--
-- * 'bgdcGreenFleetProvisioningOption' - Information about how instances are provisioned for a replacement environment in a blue/green deployment.
--
-- * 'bgdcTerminateBlueInstancesOnDeploymentSuccess' - Information about whether to terminate instances in the original fleet during a blue/green deployment.
blueGreenDeploymentConfiguration
    :: BlueGreenDeploymentConfiguration
blueGreenDeploymentConfiguration =
  BlueGreenDeploymentConfiguration'
    { _bgdcDeploymentReadyOption = Nothing
    , _bgdcGreenFleetProvisioningOption = Nothing
    , _bgdcTerminateBlueInstancesOnDeploymentSuccess = Nothing
    }


-- | Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
bgdcDeploymentReadyOption :: Lens' BlueGreenDeploymentConfiguration (Maybe DeploymentReadyOption)
bgdcDeploymentReadyOption = lens _bgdcDeploymentReadyOption (\ s a -> s{_bgdcDeploymentReadyOption = a})

-- | Information about how instances are provisioned for a replacement environment in a blue/green deployment.
bgdcGreenFleetProvisioningOption :: Lens' BlueGreenDeploymentConfiguration (Maybe GreenFleetProvisioningOption)
bgdcGreenFleetProvisioningOption = lens _bgdcGreenFleetProvisioningOption (\ s a -> s{_bgdcGreenFleetProvisioningOption = a})

-- | Information about whether to terminate instances in the original fleet during a blue/green deployment.
bgdcTerminateBlueInstancesOnDeploymentSuccess :: Lens' BlueGreenDeploymentConfiguration (Maybe BlueInstanceTerminationOption)
bgdcTerminateBlueInstancesOnDeploymentSuccess = lens _bgdcTerminateBlueInstancesOnDeploymentSuccess (\ s a -> s{_bgdcTerminateBlueInstancesOnDeploymentSuccess = a})

instance FromJSON BlueGreenDeploymentConfiguration
         where
        parseJSON
          = withObject "BlueGreenDeploymentConfiguration"
              (\ x ->
                 BlueGreenDeploymentConfiguration' <$>
                   (x .:? "deploymentReadyOption") <*>
                     (x .:? "greenFleetProvisioningOption")
                     <*>
                     (x .:? "terminateBlueInstancesOnDeploymentSuccess"))

instance Hashable BlueGreenDeploymentConfiguration
         where

instance NFData BlueGreenDeploymentConfiguration
         where

instance ToJSON BlueGreenDeploymentConfiguration
         where
        toJSON BlueGreenDeploymentConfiguration'{..}
          = object
              (catMaybes
                 [("deploymentReadyOption" .=) <$>
                    _bgdcDeploymentReadyOption,
                  ("greenFleetProvisioningOption" .=) <$>
                    _bgdcGreenFleetProvisioningOption,
                  ("terminateBlueInstancesOnDeploymentSuccess" .=) <$>
                    _bgdcTerminateBlueInstancesOnDeploymentSuccess])

-- | Information about whether instances in the original environment are terminated when a blue/green deployment is successful.
--
--
--
-- /See:/ 'blueInstanceTerminationOption' smart constructor.
data BlueInstanceTerminationOption = BlueInstanceTerminationOption'
  { _bitoAction                       :: !(Maybe InstanceAction)
  , _bitoTerminationWaitTimeInMinutes :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlueInstanceTerminationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bitoAction' - The action to take on instances in the original environment after a successful blue/green deployment.     * TERMINATE: Instances are terminated after a specified wait time.     * KEEP_ALIVE: Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
--
-- * 'bitoTerminationWaitTimeInMinutes' - The number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment.
blueInstanceTerminationOption
    :: BlueInstanceTerminationOption
blueInstanceTerminationOption =
  BlueInstanceTerminationOption'
    {_bitoAction = Nothing, _bitoTerminationWaitTimeInMinutes = Nothing}


-- | The action to take on instances in the original environment after a successful blue/green deployment.     * TERMINATE: Instances are terminated after a specified wait time.     * KEEP_ALIVE: Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
bitoAction :: Lens' BlueInstanceTerminationOption (Maybe InstanceAction)
bitoAction = lens _bitoAction (\ s a -> s{_bitoAction = a})

-- | The number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment.
bitoTerminationWaitTimeInMinutes :: Lens' BlueInstanceTerminationOption (Maybe Int)
bitoTerminationWaitTimeInMinutes = lens _bitoTerminationWaitTimeInMinutes (\ s a -> s{_bitoTerminationWaitTimeInMinutes = a})

instance FromJSON BlueInstanceTerminationOption where
        parseJSON
          = withObject "BlueInstanceTerminationOption"
              (\ x ->
                 BlueInstanceTerminationOption' <$>
                   (x .:? "action") <*>
                     (x .:? "terminationWaitTimeInMinutes"))

instance Hashable BlueInstanceTerminationOption where

instance NFData BlueInstanceTerminationOption where

instance ToJSON BlueInstanceTerminationOption where
        toJSON BlueInstanceTerminationOption'{..}
          = object
              (catMaybes
                 [("action" .=) <$> _bitoAction,
                  ("terminationWaitTimeInMinutes" .=) <$>
                    _bitoTerminationWaitTimeInMinutes])

-- | Information about a deployment configuration.
--
--
--
-- /See:/ 'deploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { _dciDeploymentConfigName :: !(Maybe Text)
  , _dciComputePlatform      :: !(Maybe ComputePlatform)
  , _dciMinimumHealthyHosts  :: !(Maybe MinimumHealthyHosts)
  , _dciTrafficRoutingConfig :: !(Maybe TrafficRoutingConfig)
  , _dciDeploymentConfigId   :: !(Maybe Text)
  , _dciCreateTime           :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentConfigInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dciDeploymentConfigName' - The deployment configuration name.
--
-- * 'dciComputePlatform' - The destination platform type for the deployment (@Lambda@ or @Server@ ).
--
-- * 'dciMinimumHealthyHosts' - Information about the number or percentage of minimum healthy instance.
--
-- * 'dciTrafficRoutingConfig' - The configuration specifying how the deployment traffic will be routed. Only deployments with a Lambda compute platform can specify this.
--
-- * 'dciDeploymentConfigId' - The deployment configuration ID.
--
-- * 'dciCreateTime' - The time at which the deployment configuration was created.
deploymentConfigInfo
    :: DeploymentConfigInfo
deploymentConfigInfo =
  DeploymentConfigInfo'
    { _dciDeploymentConfigName = Nothing
    , _dciComputePlatform = Nothing
    , _dciMinimumHealthyHosts = Nothing
    , _dciTrafficRoutingConfig = Nothing
    , _dciDeploymentConfigId = Nothing
    , _dciCreateTime = Nothing
    }


-- | The deployment configuration name.
dciDeploymentConfigName :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigName = lens _dciDeploymentConfigName (\ s a -> s{_dciDeploymentConfigName = a})

-- | The destination platform type for the deployment (@Lambda@ or @Server@ ).
dciComputePlatform :: Lens' DeploymentConfigInfo (Maybe ComputePlatform)
dciComputePlatform = lens _dciComputePlatform (\ s a -> s{_dciComputePlatform = a})

-- | Information about the number or percentage of minimum healthy instance.
dciMinimumHealthyHosts :: Lens' DeploymentConfigInfo (Maybe MinimumHealthyHosts)
dciMinimumHealthyHosts = lens _dciMinimumHealthyHosts (\ s a -> s{_dciMinimumHealthyHosts = a})

-- | The configuration specifying how the deployment traffic will be routed. Only deployments with a Lambda compute platform can specify this.
dciTrafficRoutingConfig :: Lens' DeploymentConfigInfo (Maybe TrafficRoutingConfig)
dciTrafficRoutingConfig = lens _dciTrafficRoutingConfig (\ s a -> s{_dciTrafficRoutingConfig = a})

-- | The deployment configuration ID.
dciDeploymentConfigId :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigId = lens _dciDeploymentConfigId (\ s a -> s{_dciDeploymentConfigId = a})

-- | The time at which the deployment configuration was created.
dciCreateTime :: Lens' DeploymentConfigInfo (Maybe UTCTime)
dciCreateTime = lens _dciCreateTime (\ s a -> s{_dciCreateTime = a}) . mapping _Time

instance FromJSON DeploymentConfigInfo where
        parseJSON
          = withObject "DeploymentConfigInfo"
              (\ x ->
                 DeploymentConfigInfo' <$>
                   (x .:? "deploymentConfigName") <*>
                     (x .:? "computePlatform")
                     <*> (x .:? "minimumHealthyHosts")
                     <*> (x .:? "trafficRoutingConfig")
                     <*> (x .:? "deploymentConfigId")
                     <*> (x .:? "createTime"))

instance Hashable DeploymentConfigInfo where

instance NFData DeploymentConfigInfo where

-- | Information about a deployment group.
--
--
--
-- /See:/ 'deploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { _dgiServiceRoleARN :: !(Maybe Text)
  , _dgiEc2TagSet :: !(Maybe EC2TagSet)
  , _dgiDeploymentConfigName :: !(Maybe Text)
  , _dgiLastAttemptedDeployment :: !(Maybe LastDeploymentInfo)
  , _dgiOnPremisesTagSet :: !(Maybe OnPremisesTagSet)
  , _dgiComputePlatform :: !(Maybe ComputePlatform)
  , _dgiTargetRevision :: !(Maybe RevisionLocation)
  , _dgiEc2TagFilters :: !(Maybe [EC2TagFilter])
  , _dgiBlueGreenDeploymentConfiguration :: !(Maybe BlueGreenDeploymentConfiguration)
  , _dgiLoadBalancerInfo :: !(Maybe LoadBalancerInfo)
  , _dgiOnPremisesInstanceTagFilters :: !(Maybe [TagFilter])
  , _dgiLastSuccessfulDeployment :: !(Maybe LastDeploymentInfo)
  , _dgiApplicationName :: !(Maybe Text)
  , _dgiAlarmConfiguration :: !(Maybe AlarmConfiguration)
  , _dgiTriggerConfigurations :: !(Maybe [TriggerConfig])
  , _dgiDeploymentGroupId :: !(Maybe Text)
  , _dgiAutoScalingGroups :: !(Maybe [AutoScalingGroup])
  , _dgiDeploymentStyle :: !(Maybe DeploymentStyle)
  , _dgiAutoRollbackConfiguration :: !(Maybe AutoRollbackConfiguration)
  , _dgiDeploymentGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentGroupInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgiServiceRoleARN' - A service role ARN.
--
-- * 'dgiEc2TagSet' - Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- * 'dgiDeploymentConfigName' - The deployment configuration name.
--
-- * 'dgiLastAttemptedDeployment' - Information about the most recent attempted deployment to the deployment group.
--
-- * 'dgiOnPremisesTagSet' - Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
--
-- * 'dgiComputePlatform' - The destination platform type for the deployment group (@Lambda@ or @Server@ ).
--
-- * 'dgiTargetRevision' - Information about the deployment group's target revision, including type and location.
--
-- * 'dgiEc2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
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
deploymentGroupInfo
    :: DeploymentGroupInfo
deploymentGroupInfo =
  DeploymentGroupInfo'
    { _dgiServiceRoleARN = Nothing
    , _dgiEc2TagSet = Nothing
    , _dgiDeploymentConfigName = Nothing
    , _dgiLastAttemptedDeployment = Nothing
    , _dgiOnPremisesTagSet = Nothing
    , _dgiComputePlatform = Nothing
    , _dgiTargetRevision = Nothing
    , _dgiEc2TagFilters = Nothing
    , _dgiBlueGreenDeploymentConfiguration = Nothing
    , _dgiLoadBalancerInfo = Nothing
    , _dgiOnPremisesInstanceTagFilters = Nothing
    , _dgiLastSuccessfulDeployment = Nothing
    , _dgiApplicationName = Nothing
    , _dgiAlarmConfiguration = Nothing
    , _dgiTriggerConfigurations = Nothing
    , _dgiDeploymentGroupId = Nothing
    , _dgiAutoScalingGroups = Nothing
    , _dgiDeploymentStyle = Nothing
    , _dgiAutoRollbackConfiguration = Nothing
    , _dgiDeploymentGroupName = Nothing
    }


-- | A service role ARN.
dgiServiceRoleARN :: Lens' DeploymentGroupInfo (Maybe Text)
dgiServiceRoleARN = lens _dgiServiceRoleARN (\ s a -> s{_dgiServiceRoleARN = a})

-- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as ec2TagFilters.
dgiEc2TagSet :: Lens' DeploymentGroupInfo (Maybe EC2TagSet)
dgiEc2TagSet = lens _dgiEc2TagSet (\ s a -> s{_dgiEc2TagSet = a})

-- | The deployment configuration name.
dgiDeploymentConfigName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentConfigName = lens _dgiDeploymentConfigName (\ s a -> s{_dgiDeploymentConfigName = a})

-- | Information about the most recent attempted deployment to the deployment group.
dgiLastAttemptedDeployment :: Lens' DeploymentGroupInfo (Maybe LastDeploymentInfo)
dgiLastAttemptedDeployment = lens _dgiLastAttemptedDeployment (\ s a -> s{_dgiLastAttemptedDeployment = a})

-- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
dgiOnPremisesTagSet :: Lens' DeploymentGroupInfo (Maybe OnPremisesTagSet)
dgiOnPremisesTagSet = lens _dgiOnPremisesTagSet (\ s a -> s{_dgiOnPremisesTagSet = a})

-- | The destination platform type for the deployment group (@Lambda@ or @Server@ ).
dgiComputePlatform :: Lens' DeploymentGroupInfo (Maybe ComputePlatform)
dgiComputePlatform = lens _dgiComputePlatform (\ s a -> s{_dgiComputePlatform = a})

-- | Information about the deployment group's target revision, including type and location.
dgiTargetRevision :: Lens' DeploymentGroupInfo (Maybe RevisionLocation)
dgiTargetRevision = lens _dgiTargetRevision (\ s a -> s{_dgiTargetRevision = a})

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
dgiEc2TagFilters :: Lens' DeploymentGroupInfo [EC2TagFilter]
dgiEc2TagFilters = lens _dgiEc2TagFilters (\ s a -> s{_dgiEc2TagFilters = a}) . _Default . _Coerce

-- | Information about blue/green deployment options for a deployment group.
dgiBlueGreenDeploymentConfiguration :: Lens' DeploymentGroupInfo (Maybe BlueGreenDeploymentConfiguration)
dgiBlueGreenDeploymentConfiguration = lens _dgiBlueGreenDeploymentConfiguration (\ s a -> s{_dgiBlueGreenDeploymentConfiguration = a})

-- | Information about the load balancer to use in a deployment.
dgiLoadBalancerInfo :: Lens' DeploymentGroupInfo (Maybe LoadBalancerInfo)
dgiLoadBalancerInfo = lens _dgiLoadBalancerInfo (\ s a -> s{_dgiLoadBalancerInfo = a})

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
dgiOnPremisesInstanceTagFilters :: Lens' DeploymentGroupInfo [TagFilter]
dgiOnPremisesInstanceTagFilters = lens _dgiOnPremisesInstanceTagFilters (\ s a -> s{_dgiOnPremisesInstanceTagFilters = a}) . _Default . _Coerce

-- | Information about the most recent successful deployment to the deployment group.
dgiLastSuccessfulDeployment :: Lens' DeploymentGroupInfo (Maybe LastDeploymentInfo)
dgiLastSuccessfulDeployment = lens _dgiLastSuccessfulDeployment (\ s a -> s{_dgiLastSuccessfulDeployment = a})

-- | The application name.
dgiApplicationName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiApplicationName = lens _dgiApplicationName (\ s a -> s{_dgiApplicationName = a})

-- | A list of alarms associated with the deployment group.
dgiAlarmConfiguration :: Lens' DeploymentGroupInfo (Maybe AlarmConfiguration)
dgiAlarmConfiguration = lens _dgiAlarmConfiguration (\ s a -> s{_dgiAlarmConfiguration = a})

-- | Information about triggers associated with the deployment group.
dgiTriggerConfigurations :: Lens' DeploymentGroupInfo [TriggerConfig]
dgiTriggerConfigurations = lens _dgiTriggerConfigurations (\ s a -> s{_dgiTriggerConfigurations = a}) . _Default . _Coerce

-- | The deployment group ID.
dgiDeploymentGroupId :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupId = lens _dgiDeploymentGroupId (\ s a -> s{_dgiDeploymentGroupId = a})

-- | A list of associated Auto Scaling groups.
dgiAutoScalingGroups :: Lens' DeploymentGroupInfo [AutoScalingGroup]
dgiAutoScalingGroups = lens _dgiAutoScalingGroups (\ s a -> s{_dgiAutoScalingGroups = a}) . _Default . _Coerce

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
dgiDeploymentStyle :: Lens' DeploymentGroupInfo (Maybe DeploymentStyle)
dgiDeploymentStyle = lens _dgiDeploymentStyle (\ s a -> s{_dgiDeploymentStyle = a})

-- | Information about the automatic rollback configuration associated with the deployment group.
dgiAutoRollbackConfiguration :: Lens' DeploymentGroupInfo (Maybe AutoRollbackConfiguration)
dgiAutoRollbackConfiguration = lens _dgiAutoRollbackConfiguration (\ s a -> s{_dgiAutoRollbackConfiguration = a})

-- | The deployment group name.
dgiDeploymentGroupName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupName = lens _dgiDeploymentGroupName (\ s a -> s{_dgiDeploymentGroupName = a})

instance FromJSON DeploymentGroupInfo where
        parseJSON
          = withObject "DeploymentGroupInfo"
              (\ x ->
                 DeploymentGroupInfo' <$>
                   (x .:? "serviceRoleArn") <*> (x .:? "ec2TagSet") <*>
                     (x .:? "deploymentConfigName")
                     <*> (x .:? "lastAttemptedDeployment")
                     <*> (x .:? "onPremisesTagSet")
                     <*> (x .:? "computePlatform")
                     <*> (x .:? "targetRevision")
                     <*> (x .:? "ec2TagFilters" .!= mempty)
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
                     <*> (x .:? "deploymentGroupName"))

instance Hashable DeploymentGroupInfo where

instance NFData DeploymentGroupInfo where

-- | Information about a deployment.
--
--
--
-- /See:/ 'deploymentInfo' smart constructor.
data DeploymentInfo = DeploymentInfo'
  { _diCreator :: !(Maybe DeploymentCreator)
  , _diStatus :: !(Maybe DeploymentStatus)
  , _diDeploymentId :: !(Maybe Text)
  , _diDeploymentConfigName :: !(Maybe Text)
  , _diComputePlatform :: !(Maybe ComputePlatform)
  , _diPreviousRevision :: !(Maybe RevisionLocation)
  , _diInstanceTerminationWaitTimeStarted :: !(Maybe Bool)
  , _diDeploymentStatusMessages :: !(Maybe [Text])
  , _diStartTime :: !(Maybe POSIX)
  , _diCompleteTime :: !(Maybe POSIX)
  , _diBlueGreenDeploymentConfiguration :: !(Maybe BlueGreenDeploymentConfiguration)
  , _diErrorInformation :: !(Maybe ErrorInformation)
  , _diLoadBalancerInfo :: !(Maybe LoadBalancerInfo)
  , _diAdditionalDeploymentStatusInfo :: !(Maybe Text)
  , _diDeploymentOverview :: !(Maybe DeploymentOverview)
  , _diFileExistsBehavior :: !(Maybe FileExistsBehavior)
  , _diApplicationName :: !(Maybe Text)
  , _diRollbackInfo :: !(Maybe RollbackInfo)
  , _diTargetInstances :: !(Maybe TargetInstances)
  , _diRevision :: !(Maybe RevisionLocation)
  , _diDescription :: !(Maybe Text)
  , _diDeploymentStyle :: !(Maybe DeploymentStyle)
  , _diCreateTime :: !(Maybe POSIX)
  , _diAutoRollbackConfiguration :: !(Maybe AutoRollbackConfiguration)
  , _diUpdateOutdatedInstancesOnly :: !(Maybe Bool)
  , _diDeploymentGroupName :: !(Maybe Text)
  , _diIgnoreApplicationStopFailures :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diCreator' - The means by which the deployment was created:     * user: A user created the deployment.     * autoscaling: Auto Scaling created the deployment.     * codeDeployRollback: A rollback process created the deployment.
--
-- * 'diStatus' - The current state of the deployment as a whole.
--
-- * 'diDeploymentId' - The deployment ID.
--
-- * 'diDeploymentConfigName' - The deployment configuration name.
--
-- * 'diComputePlatform' - The destination platform type for the deployment (@Lambda@ or @Server@ ).
--
-- * 'diPreviousRevision' - Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
--
-- * 'diInstanceTerminationWaitTimeStarted' - Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified; otherwise, 'true' as soon as the termination wait period starts.
--
-- * 'diDeploymentStatusMessages' - Messages that contain information about the status of a deployment.
--
-- * 'diStartTime' - A timestamp indicating when the deployment was deployed to the deployment group. In some cases, the reported value of the start time may be later than the complete time. This is due to differences in the clock settings of back-end servers that participate in the deployment process.
--
-- * 'diCompleteTime' - A timestamp indicating when the deployment was complete.
--
-- * 'diBlueGreenDeploymentConfiguration' - Information about blue/green deployment options for this deployment.
--
-- * 'diErrorInformation' - Information about any error associated with this deployment.
--
-- * 'diLoadBalancerInfo' - Information about the load balancer used in the deployment.
--
-- * 'diAdditionalDeploymentStatusInfo' - Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
--
-- * 'diDeploymentOverview' - A summary of the deployment status of the instances in the deployment.
--
-- * 'diFileExistsBehavior' - Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.     * DISALLOW: The deployment fails. This is also the default behavior if no option is specified.     * OVERWRITE: The version of the file from the application revision currently being deployed replaces the version already on the instance.     * RETAIN: The version of the file already on the instance is kept and used as part of the new deployment.
--
-- * 'diApplicationName' - The application name.
--
-- * 'diRollbackInfo' - Information about a deployment rollback.
--
-- * 'diTargetInstances' - Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- * 'diRevision' - Information about the location of stored application artifacts and the service from which to retrieve them.
--
-- * 'diDescription' - A comment about the deployment.
--
-- * 'diDeploymentStyle' - Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- * 'diCreateTime' - A timestamp indicating when the deployment was created.
--
-- * 'diAutoRollbackConfiguration' - Information about the automatic rollback configuration associated with the deployment.
--
-- * 'diUpdateOutdatedInstancesOnly' - Indicates whether only instances that are not running the latest application revision are to be deployed to.
--
-- * 'diDeploymentGroupName' - The deployment group name.
--
-- * 'diIgnoreApplicationStopFailures' - If true, then if the deployment causes the ApplicationStop deployment lifecycle event to an instance to fail, the deployment to that instance will not be considered to have failed at that point and will continue on to the BeforeInstall deployment lifecycle event. If false or not specified, then if the deployment causes the ApplicationStop deployment lifecycle event to an instance to fail, the deployment to that instance will stop, and the deployment to that instance will be considered to have failed.
deploymentInfo
    :: DeploymentInfo
deploymentInfo =
  DeploymentInfo'
    { _diCreator = Nothing
    , _diStatus = Nothing
    , _diDeploymentId = Nothing
    , _diDeploymentConfigName = Nothing
    , _diComputePlatform = Nothing
    , _diPreviousRevision = Nothing
    , _diInstanceTerminationWaitTimeStarted = Nothing
    , _diDeploymentStatusMessages = Nothing
    , _diStartTime = Nothing
    , _diCompleteTime = Nothing
    , _diBlueGreenDeploymentConfiguration = Nothing
    , _diErrorInformation = Nothing
    , _diLoadBalancerInfo = Nothing
    , _diAdditionalDeploymentStatusInfo = Nothing
    , _diDeploymentOverview = Nothing
    , _diFileExistsBehavior = Nothing
    , _diApplicationName = Nothing
    , _diRollbackInfo = Nothing
    , _diTargetInstances = Nothing
    , _diRevision = Nothing
    , _diDescription = Nothing
    , _diDeploymentStyle = Nothing
    , _diCreateTime = Nothing
    , _diAutoRollbackConfiguration = Nothing
    , _diUpdateOutdatedInstancesOnly = Nothing
    , _diDeploymentGroupName = Nothing
    , _diIgnoreApplicationStopFailures = Nothing
    }


-- | The means by which the deployment was created:     * user: A user created the deployment.     * autoscaling: Auto Scaling created the deployment.     * codeDeployRollback: A rollback process created the deployment.
diCreator :: Lens' DeploymentInfo (Maybe DeploymentCreator)
diCreator = lens _diCreator (\ s a -> s{_diCreator = a})

-- | The current state of the deployment as a whole.
diStatus :: Lens' DeploymentInfo (Maybe DeploymentStatus)
diStatus = lens _diStatus (\ s a -> s{_diStatus = a})

-- | The deployment ID.
diDeploymentId :: Lens' DeploymentInfo (Maybe Text)
diDeploymentId = lens _diDeploymentId (\ s a -> s{_diDeploymentId = a})

-- | The deployment configuration name.
diDeploymentConfigName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentConfigName = lens _diDeploymentConfigName (\ s a -> s{_diDeploymentConfigName = a})

-- | The destination platform type for the deployment (@Lambda@ or @Server@ ).
diComputePlatform :: Lens' DeploymentInfo (Maybe ComputePlatform)
diComputePlatform = lens _diComputePlatform (\ s a -> s{_diComputePlatform = a})

-- | Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
diPreviousRevision :: Lens' DeploymentInfo (Maybe RevisionLocation)
diPreviousRevision = lens _diPreviousRevision (\ s a -> s{_diPreviousRevision = a})

-- | Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified; otherwise, 'true' as soon as the termination wait period starts.
diInstanceTerminationWaitTimeStarted :: Lens' DeploymentInfo (Maybe Bool)
diInstanceTerminationWaitTimeStarted = lens _diInstanceTerminationWaitTimeStarted (\ s a -> s{_diInstanceTerminationWaitTimeStarted = a})

-- | Messages that contain information about the status of a deployment.
diDeploymentStatusMessages :: Lens' DeploymentInfo [Text]
diDeploymentStatusMessages = lens _diDeploymentStatusMessages (\ s a -> s{_diDeploymentStatusMessages = a}) . _Default . _Coerce

-- | A timestamp indicating when the deployment was deployed to the deployment group. In some cases, the reported value of the start time may be later than the complete time. This is due to differences in the clock settings of back-end servers that participate in the deployment process.
diStartTime :: Lens' DeploymentInfo (Maybe UTCTime)
diStartTime = lens _diStartTime (\ s a -> s{_diStartTime = a}) . mapping _Time

-- | A timestamp indicating when the deployment was complete.
diCompleteTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCompleteTime = lens _diCompleteTime (\ s a -> s{_diCompleteTime = a}) . mapping _Time

-- | Information about blue/green deployment options for this deployment.
diBlueGreenDeploymentConfiguration :: Lens' DeploymentInfo (Maybe BlueGreenDeploymentConfiguration)
diBlueGreenDeploymentConfiguration = lens _diBlueGreenDeploymentConfiguration (\ s a -> s{_diBlueGreenDeploymentConfiguration = a})

-- | Information about any error associated with this deployment.
diErrorInformation :: Lens' DeploymentInfo (Maybe ErrorInformation)
diErrorInformation = lens _diErrorInformation (\ s a -> s{_diErrorInformation = a})

-- | Information about the load balancer used in the deployment.
diLoadBalancerInfo :: Lens' DeploymentInfo (Maybe LoadBalancerInfo)
diLoadBalancerInfo = lens _diLoadBalancerInfo (\ s a -> s{_diLoadBalancerInfo = a})

-- | Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
diAdditionalDeploymentStatusInfo :: Lens' DeploymentInfo (Maybe Text)
diAdditionalDeploymentStatusInfo = lens _diAdditionalDeploymentStatusInfo (\ s a -> s{_diAdditionalDeploymentStatusInfo = a})

-- | A summary of the deployment status of the instances in the deployment.
diDeploymentOverview :: Lens' DeploymentInfo (Maybe DeploymentOverview)
diDeploymentOverview = lens _diDeploymentOverview (\ s a -> s{_diDeploymentOverview = a})

-- | Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.     * DISALLOW: The deployment fails. This is also the default behavior if no option is specified.     * OVERWRITE: The version of the file from the application revision currently being deployed replaces the version already on the instance.     * RETAIN: The version of the file already on the instance is kept and used as part of the new deployment.
diFileExistsBehavior :: Lens' DeploymentInfo (Maybe FileExistsBehavior)
diFileExistsBehavior = lens _diFileExistsBehavior (\ s a -> s{_diFileExistsBehavior = a})

-- | The application name.
diApplicationName :: Lens' DeploymentInfo (Maybe Text)
diApplicationName = lens _diApplicationName (\ s a -> s{_diApplicationName = a})

-- | Information about a deployment rollback.
diRollbackInfo :: Lens' DeploymentInfo (Maybe RollbackInfo)
diRollbackInfo = lens _diRollbackInfo (\ s a -> s{_diRollbackInfo = a})

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
diTargetInstances :: Lens' DeploymentInfo (Maybe TargetInstances)
diTargetInstances = lens _diTargetInstances (\ s a -> s{_diTargetInstances = a})

-- | Information about the location of stored application artifacts and the service from which to retrieve them.
diRevision :: Lens' DeploymentInfo (Maybe RevisionLocation)
diRevision = lens _diRevision (\ s a -> s{_diRevision = a})

-- | A comment about the deployment.
diDescription :: Lens' DeploymentInfo (Maybe Text)
diDescription = lens _diDescription (\ s a -> s{_diDescription = a})

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
diDeploymentStyle :: Lens' DeploymentInfo (Maybe DeploymentStyle)
diDeploymentStyle = lens _diDeploymentStyle (\ s a -> s{_diDeploymentStyle = a})

-- | A timestamp indicating when the deployment was created.
diCreateTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCreateTime = lens _diCreateTime (\ s a -> s{_diCreateTime = a}) . mapping _Time

-- | Information about the automatic rollback configuration associated with the deployment.
diAutoRollbackConfiguration :: Lens' DeploymentInfo (Maybe AutoRollbackConfiguration)
diAutoRollbackConfiguration = lens _diAutoRollbackConfiguration (\ s a -> s{_diAutoRollbackConfiguration = a})

-- | Indicates whether only instances that are not running the latest application revision are to be deployed to.
diUpdateOutdatedInstancesOnly :: Lens' DeploymentInfo (Maybe Bool)
diUpdateOutdatedInstancesOnly = lens _diUpdateOutdatedInstancesOnly (\ s a -> s{_diUpdateOutdatedInstancesOnly = a})

-- | The deployment group name.
diDeploymentGroupName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentGroupName = lens _diDeploymentGroupName (\ s a -> s{_diDeploymentGroupName = a})

-- | If true, then if the deployment causes the ApplicationStop deployment lifecycle event to an instance to fail, the deployment to that instance will not be considered to have failed at that point and will continue on to the BeforeInstall deployment lifecycle event. If false or not specified, then if the deployment causes the ApplicationStop deployment lifecycle event to an instance to fail, the deployment to that instance will stop, and the deployment to that instance will be considered to have failed.
diIgnoreApplicationStopFailures :: Lens' DeploymentInfo (Maybe Bool)
diIgnoreApplicationStopFailures = lens _diIgnoreApplicationStopFailures (\ s a -> s{_diIgnoreApplicationStopFailures = a})

instance FromJSON DeploymentInfo where
        parseJSON
          = withObject "DeploymentInfo"
              (\ x ->
                 DeploymentInfo' <$>
                   (x .:? "creator") <*> (x .:? "status") <*>
                     (x .:? "deploymentId")
                     <*> (x .:? "deploymentConfigName")
                     <*> (x .:? "computePlatform")
                     <*> (x .:? "previousRevision")
                     <*> (x .:? "instanceTerminationWaitTimeStarted")
                     <*> (x .:? "deploymentStatusMessages" .!= mempty)
                     <*> (x .:? "startTime")
                     <*> (x .:? "completeTime")
                     <*> (x .:? "blueGreenDeploymentConfiguration")
                     <*> (x .:? "errorInformation")
                     <*> (x .:? "loadBalancerInfo")
                     <*> (x .:? "additionalDeploymentStatusInfo")
                     <*> (x .:? "deploymentOverview")
                     <*> (x .:? "fileExistsBehavior")
                     <*> (x .:? "applicationName")
                     <*> (x .:? "rollbackInfo")
                     <*> (x .:? "targetInstances")
                     <*> (x .:? "revision")
                     <*> (x .:? "description")
                     <*> (x .:? "deploymentStyle")
                     <*> (x .:? "createTime")
                     <*> (x .:? "autoRollbackConfiguration")
                     <*> (x .:? "updateOutdatedInstancesOnly")
                     <*> (x .:? "deploymentGroupName")
                     <*> (x .:? "ignoreApplicationStopFailures"))

instance Hashable DeploymentInfo where

instance NFData DeploymentInfo where

-- | Information about the deployment status of the instances in the deployment.
--
--
--
-- /See:/ 'deploymentOverview' smart constructor.
data DeploymentOverview = DeploymentOverview'
  { _doPending    :: !(Maybe Integer)
  , _doSkipped    :: !(Maybe Integer)
  , _doInProgress :: !(Maybe Integer)
  , _doSucceeded  :: !(Maybe Integer)
  , _doReady      :: !(Maybe Integer)
  , _doFailed     :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentOverview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doPending' - The number of instances in the deployment in a pending state.
--
-- * 'doSkipped' - The number of instances in the deployment in a skipped state.
--
-- * 'doInProgress' - The number of instances in which the deployment is in progress.
--
-- * 'doSucceeded' - The number of instances in the deployment to which revisions have been successfully deployed.
--
-- * 'doReady' - The number of instances in a replacement environment ready to receive traffic in a blue/green deployment.
--
-- * 'doFailed' - The number of instances in the deployment in a failed state.
deploymentOverview
    :: DeploymentOverview
deploymentOverview =
  DeploymentOverview'
    { _doPending = Nothing
    , _doSkipped = Nothing
    , _doInProgress = Nothing
    , _doSucceeded = Nothing
    , _doReady = Nothing
    , _doFailed = Nothing
    }


-- | The number of instances in the deployment in a pending state.
doPending :: Lens' DeploymentOverview (Maybe Integer)
doPending = lens _doPending (\ s a -> s{_doPending = a})

-- | The number of instances in the deployment in a skipped state.
doSkipped :: Lens' DeploymentOverview (Maybe Integer)
doSkipped = lens _doSkipped (\ s a -> s{_doSkipped = a})

-- | The number of instances in which the deployment is in progress.
doInProgress :: Lens' DeploymentOverview (Maybe Integer)
doInProgress = lens _doInProgress (\ s a -> s{_doInProgress = a})

-- | The number of instances in the deployment to which revisions have been successfully deployed.
doSucceeded :: Lens' DeploymentOverview (Maybe Integer)
doSucceeded = lens _doSucceeded (\ s a -> s{_doSucceeded = a})

-- | The number of instances in a replacement environment ready to receive traffic in a blue/green deployment.
doReady :: Lens' DeploymentOverview (Maybe Integer)
doReady = lens _doReady (\ s a -> s{_doReady = a})

-- | The number of instances in the deployment in a failed state.
doFailed :: Lens' DeploymentOverview (Maybe Integer)
doFailed = lens _doFailed (\ s a -> s{_doFailed = a})

instance FromJSON DeploymentOverview where
        parseJSON
          = withObject "DeploymentOverview"
              (\ x ->
                 DeploymentOverview' <$>
                   (x .:? "Pending") <*> (x .:? "Skipped") <*>
                     (x .:? "InProgress")
                     <*> (x .:? "Succeeded")
                     <*> (x .:? "Ready")
                     <*> (x .:? "Failed"))

instance Hashable DeploymentOverview where

instance NFData DeploymentOverview where

-- | Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.
--
--
--
-- /See:/ 'deploymentReadyOption' smart constructor.
data DeploymentReadyOption = DeploymentReadyOption'
  { _droActionOnTimeout   :: !(Maybe DeploymentReadyAction)
  , _droWaitTimeInMinutes :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentReadyOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'droActionOnTimeout' - Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
--
-- * 'droWaitTimeInMinutes' - The number of minutes to wait before the status of a blue/green deployment changed to Stopped if rerouting is not started manually. Applies only to the STOP_DEPLOYMENT option for actionOnTimeout
deploymentReadyOption
    :: DeploymentReadyOption
deploymentReadyOption =
  DeploymentReadyOption'
    {_droActionOnTimeout = Nothing, _droWaitTimeInMinutes = Nothing}


-- | Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
droActionOnTimeout :: Lens' DeploymentReadyOption (Maybe DeploymentReadyAction)
droActionOnTimeout = lens _droActionOnTimeout (\ s a -> s{_droActionOnTimeout = a})

-- | The number of minutes to wait before the status of a blue/green deployment changed to Stopped if rerouting is not started manually. Applies only to the STOP_DEPLOYMENT option for actionOnTimeout
droWaitTimeInMinutes :: Lens' DeploymentReadyOption (Maybe Int)
droWaitTimeInMinutes = lens _droWaitTimeInMinutes (\ s a -> s{_droWaitTimeInMinutes = a})

instance FromJSON DeploymentReadyOption where
        parseJSON
          = withObject "DeploymentReadyOption"
              (\ x ->
                 DeploymentReadyOption' <$>
                   (x .:? "actionOnTimeout") <*>
                     (x .:? "waitTimeInMinutes"))

instance Hashable DeploymentReadyOption where

instance NFData DeploymentReadyOption where

instance ToJSON DeploymentReadyOption where
        toJSON DeploymentReadyOption'{..}
          = object
              (catMaybes
                 [("actionOnTimeout" .=) <$> _droActionOnTimeout,
                  ("waitTimeInMinutes" .=) <$> _droWaitTimeInMinutes])

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
--
--
-- /See:/ 'deploymentStyle' smart constructor.
data DeploymentStyle = DeploymentStyle'
  { _dsDeploymentOption :: !(Maybe DeploymentOption)
  , _dsDeploymentType   :: !(Maybe DeploymentType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentStyle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsDeploymentOption' - Indicates whether to route deployment traffic behind a load balancer.
--
-- * 'dsDeploymentType' - Indicates whether to run an in-place deployment or a blue/green deployment.
deploymentStyle
    :: DeploymentStyle
deploymentStyle =
  DeploymentStyle' {_dsDeploymentOption = Nothing, _dsDeploymentType = Nothing}


-- | Indicates whether to route deployment traffic behind a load balancer.
dsDeploymentOption :: Lens' DeploymentStyle (Maybe DeploymentOption)
dsDeploymentOption = lens _dsDeploymentOption (\ s a -> s{_dsDeploymentOption = a})

-- | Indicates whether to run an in-place deployment or a blue/green deployment.
dsDeploymentType :: Lens' DeploymentStyle (Maybe DeploymentType)
dsDeploymentType = lens _dsDeploymentType (\ s a -> s{_dsDeploymentType = a})

instance FromJSON DeploymentStyle where
        parseJSON
          = withObject "DeploymentStyle"
              (\ x ->
                 DeploymentStyle' <$>
                   (x .:? "deploymentOption") <*>
                     (x .:? "deploymentType"))

instance Hashable DeploymentStyle where

instance NFData DeploymentStyle where

instance ToJSON DeploymentStyle where
        toJSON DeploymentStyle'{..}
          = object
              (catMaybes
                 [("deploymentOption" .=) <$> _dsDeploymentOption,
                  ("deploymentType" .=) <$> _dsDeploymentType])

-- | Diagnostic information about executable scripts that are part of a deployment.
--
--
--
-- /See:/ 'diagnostics' smart constructor.
data Diagnostics = Diagnostics'
  { _dLogTail    :: !(Maybe Text)
  , _dErrorCode  :: !(Maybe LifecycleErrorCode)
  , _dScriptName :: !(Maybe Text)
  , _dMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Diagnostics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLogTail' - The last portion of the diagnostic log. If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
--
-- * 'dErrorCode' - The associated error code:     * Success: The specified script ran.     * ScriptMissing: The specified script was not found in the specified location.     * ScriptNotExecutable: The specified script is not a recognized executable file type.     * ScriptTimedOut: The specified script did not finish running in the specified time period.     * ScriptFailed: The specified script failed to run as expected.     * UnknownError: The specified script did not run for an unknown reason.
--
-- * 'dScriptName' - The name of the script.
--
-- * 'dMessage' - The message associated with the error.
diagnostics
    :: Diagnostics
diagnostics =
  Diagnostics'
    { _dLogTail = Nothing
    , _dErrorCode = Nothing
    , _dScriptName = Nothing
    , _dMessage = Nothing
    }


-- | The last portion of the diagnostic log. If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
dLogTail :: Lens' Diagnostics (Maybe Text)
dLogTail = lens _dLogTail (\ s a -> s{_dLogTail = a})

-- | The associated error code:     * Success: The specified script ran.     * ScriptMissing: The specified script was not found in the specified location.     * ScriptNotExecutable: The specified script is not a recognized executable file type.     * ScriptTimedOut: The specified script did not finish running in the specified time period.     * ScriptFailed: The specified script failed to run as expected.     * UnknownError: The specified script did not run for an unknown reason.
dErrorCode :: Lens' Diagnostics (Maybe LifecycleErrorCode)
dErrorCode = lens _dErrorCode (\ s a -> s{_dErrorCode = a})

-- | The name of the script.
dScriptName :: Lens' Diagnostics (Maybe Text)
dScriptName = lens _dScriptName (\ s a -> s{_dScriptName = a})

-- | The message associated with the error.
dMessage :: Lens' Diagnostics (Maybe Text)
dMessage = lens _dMessage (\ s a -> s{_dMessage = a})

instance FromJSON Diagnostics where
        parseJSON
          = withObject "Diagnostics"
              (\ x ->
                 Diagnostics' <$>
                   (x .:? "logTail") <*> (x .:? "errorCode") <*>
                     (x .:? "scriptName")
                     <*> (x .:? "message"))

instance Hashable Diagnostics where

instance NFData Diagnostics where

-- | Information about an EC2 tag filter.
--
--
--
-- /See:/ 'ec2TagFilter' smart constructor.
data EC2TagFilter = EC2TagFilter'
  { _etfValue :: !(Maybe Text)
  , _etfKey   :: !(Maybe Text)
  , _etfType  :: !(Maybe EC2TagFilterType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etfValue' - The tag filter value.
--
-- * 'etfKey' - The tag filter key.
--
-- * 'etfType' - The tag filter type:     * KEY_ONLY: Key only.     * VALUE_ONLY: Value only.     * KEY_AND_VALUE: Key and value.
ec2TagFilter
    :: EC2TagFilter
ec2TagFilter =
  EC2TagFilter' {_etfValue = Nothing, _etfKey = Nothing, _etfType = Nothing}


-- | The tag filter value.
etfValue :: Lens' EC2TagFilter (Maybe Text)
etfValue = lens _etfValue (\ s a -> s{_etfValue = a})

-- | The tag filter key.
etfKey :: Lens' EC2TagFilter (Maybe Text)
etfKey = lens _etfKey (\ s a -> s{_etfKey = a})

-- | The tag filter type:     * KEY_ONLY: Key only.     * VALUE_ONLY: Value only.     * KEY_AND_VALUE: Key and value.
etfType :: Lens' EC2TagFilter (Maybe EC2TagFilterType)
etfType = lens _etfType (\ s a -> s{_etfType = a})

instance FromJSON EC2TagFilter where
        parseJSON
          = withObject "EC2TagFilter"
              (\ x ->
                 EC2TagFilter' <$>
                   (x .:? "Value") <*> (x .:? "Key") <*> (x .:? "Type"))

instance Hashable EC2TagFilter where

instance NFData EC2TagFilter where

instance ToJSON EC2TagFilter where
        toJSON EC2TagFilter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _etfValue, ("Key" .=) <$> _etfKey,
                  ("Type" .=) <$> _etfType])

-- | Information about groups of EC2 instance tags.
--
--
--
-- /See:/ 'ec2TagSet' smart constructor.
newtype EC2TagSet = EC2TagSet'
  { _etsEc2TagSetList :: Maybe [[EC2TagFilter]]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2TagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etsEc2TagSetList' - A list containing other lists of EC2 instance tag groups. In order for an instance to be included in the deployment group, it must be identified by all the tag groups in the list.
ec2TagSet
    :: EC2TagSet
ec2TagSet = EC2TagSet' {_etsEc2TagSetList = Nothing}


-- | A list containing other lists of EC2 instance tag groups. In order for an instance to be included in the deployment group, it must be identified by all the tag groups in the list.
etsEc2TagSetList :: Lens' EC2TagSet [[EC2TagFilter]]
etsEc2TagSetList = lens _etsEc2TagSetList (\ s a -> s{_etsEc2TagSetList = a}) . _Default . _Coerce

instance FromJSON EC2TagSet where
        parseJSON
          = withObject "EC2TagSet"
              (\ x ->
                 EC2TagSet' <$> (x .:? "ec2TagSetList" .!= mempty))

instance Hashable EC2TagSet where

instance NFData EC2TagSet where

instance ToJSON EC2TagSet where
        toJSON EC2TagSet'{..}
          = object
              (catMaybes
                 [("ec2TagSetList" .=) <$> _etsEc2TagSetList])

-- | Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.
--
--
--
-- /See:/ 'eLBInfo' smart constructor.
newtype ELBInfo = ELBInfo'
  { _elbiName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ELBInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elbiName' - For blue/green deployments, the name of the load balancer that will be used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment completes.
eLBInfo
    :: ELBInfo
eLBInfo = ELBInfo' {_elbiName = Nothing}


-- | For blue/green deployments, the name of the load balancer that will be used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment completes.
elbiName :: Lens' ELBInfo (Maybe Text)
elbiName = lens _elbiName (\ s a -> s{_elbiName = a})

instance FromJSON ELBInfo where
        parseJSON
          = withObject "ELBInfo"
              (\ x -> ELBInfo' <$> (x .:? "name"))

instance Hashable ELBInfo where

instance NFData ELBInfo where

instance ToJSON ELBInfo where
        toJSON ELBInfo'{..}
          = object (catMaybes [("name" .=) <$> _elbiName])

-- | Information about a deployment error.
--
--
--
-- /See:/ 'errorInformation' smart constructor.
data ErrorInformation = ErrorInformation'
  { _eiCode    :: !(Maybe DeployErrorCode)
  , _eiMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiCode' - For information about additional error codes, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <http://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> . The error code:     * APPLICATION_MISSING: The application was missing. This error code will most likely be raised if the application is deleted after the deployment is created but before it is started.     * DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This error code will most likely be raised if the deployment group is deleted after the deployment is created but before it is started.     * HEALTH_CONSTRAINTS: The deployment failed on too many instances to be successfully deployed within the instance health constraints specified.     * HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully deployed within the instance health constraints specified.     * IAM_ROLE_MISSING: The service role cannot be accessed.     * IAM_ROLE_PERMISSIONS: The service role does not have the correct permissions.     * INTERNAL_ERROR: There was an internal error.     * NO_EC2_SUBSCRIPTION: The calling account is not subscribed to the Amazon EC2 service.     * NO_INSTANCES: No instance were specified, or no instance can be found.     * OVER_MAX_INSTANCES: The maximum number of instance was exceeded.     * THROTTLED: The operation was throttled because the calling account exceeded the throttling limits of one or more AWS services.     * TIMEOUT: The deployment has timed out.     * REVISION_MISSING: The revision ID was missing. This error code will most likely be raised if the revision is deleted after the deployment is created but before it is started.
--
-- * 'eiMessage' - An accompanying error message.
errorInformation
    :: ErrorInformation
errorInformation = ErrorInformation' {_eiCode = Nothing, _eiMessage = Nothing}


-- | For information about additional error codes, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for AWS CodeDeploy> in the <http://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> . The error code:     * APPLICATION_MISSING: The application was missing. This error code will most likely be raised if the application is deleted after the deployment is created but before it is started.     * DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This error code will most likely be raised if the deployment group is deleted after the deployment is created but before it is started.     * HEALTH_CONSTRAINTS: The deployment failed on too many instances to be successfully deployed within the instance health constraints specified.     * HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully deployed within the instance health constraints specified.     * IAM_ROLE_MISSING: The service role cannot be accessed.     * IAM_ROLE_PERMISSIONS: The service role does not have the correct permissions.     * INTERNAL_ERROR: There was an internal error.     * NO_EC2_SUBSCRIPTION: The calling account is not subscribed to the Amazon EC2 service.     * NO_INSTANCES: No instance were specified, or no instance can be found.     * OVER_MAX_INSTANCES: The maximum number of instance was exceeded.     * THROTTLED: The operation was throttled because the calling account exceeded the throttling limits of one or more AWS services.     * TIMEOUT: The deployment has timed out.     * REVISION_MISSING: The revision ID was missing. This error code will most likely be raised if the revision is deleted after the deployment is created but before it is started.
eiCode :: Lens' ErrorInformation (Maybe DeployErrorCode)
eiCode = lens _eiCode (\ s a -> s{_eiCode = a})

-- | An accompanying error message.
eiMessage :: Lens' ErrorInformation (Maybe Text)
eiMessage = lens _eiMessage (\ s a -> s{_eiMessage = a})

instance FromJSON ErrorInformation where
        parseJSON
          = withObject "ErrorInformation"
              (\ x ->
                 ErrorInformation' <$>
                   (x .:? "code") <*> (x .:? "message"))

instance Hashable ErrorInformation where

instance NFData ErrorInformation where

-- | Information about an application revision.
--
--
--
-- /See:/ 'genericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { _griRegisterTime     :: !(Maybe POSIX)
  , _griFirstUsedTime    :: !(Maybe POSIX)
  , _griDeploymentGroups :: !(Maybe [Text])
  , _griLastUsedTime     :: !(Maybe POSIX)
  , _griDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenericRevisionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'griRegisterTime' - When the revision was registered with AWS CodeDeploy.
--
-- * 'griFirstUsedTime' - When the revision was first used by AWS CodeDeploy.
--
-- * 'griDeploymentGroups' - The deployment groups for which this is the current target revision.
--
-- * 'griLastUsedTime' - When the revision was last used by AWS CodeDeploy.
--
-- * 'griDescription' - A comment about the revision.
genericRevisionInfo
    :: GenericRevisionInfo
genericRevisionInfo =
  GenericRevisionInfo'
    { _griRegisterTime = Nothing
    , _griFirstUsedTime = Nothing
    , _griDeploymentGroups = Nothing
    , _griLastUsedTime = Nothing
    , _griDescription = Nothing
    }


-- | When the revision was registered with AWS CodeDeploy.
griRegisterTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griRegisterTime = lens _griRegisterTime (\ s a -> s{_griRegisterTime = a}) . mapping _Time

-- | When the revision was first used by AWS CodeDeploy.
griFirstUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griFirstUsedTime = lens _griFirstUsedTime (\ s a -> s{_griFirstUsedTime = a}) . mapping _Time

-- | The deployment groups for which this is the current target revision.
griDeploymentGroups :: Lens' GenericRevisionInfo [Text]
griDeploymentGroups = lens _griDeploymentGroups (\ s a -> s{_griDeploymentGroups = a}) . _Default . _Coerce

-- | When the revision was last used by AWS CodeDeploy.
griLastUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griLastUsedTime = lens _griLastUsedTime (\ s a -> s{_griLastUsedTime = a}) . mapping _Time

-- | A comment about the revision.
griDescription :: Lens' GenericRevisionInfo (Maybe Text)
griDescription = lens _griDescription (\ s a -> s{_griDescription = a})

instance FromJSON GenericRevisionInfo where
        parseJSON
          = withObject "GenericRevisionInfo"
              (\ x ->
                 GenericRevisionInfo' <$>
                   (x .:? "registerTime") <*> (x .:? "firstUsedTime")
                     <*> (x .:? "deploymentGroups" .!= mempty)
                     <*> (x .:? "lastUsedTime")
                     <*> (x .:? "description"))

instance Hashable GenericRevisionInfo where

instance NFData GenericRevisionInfo where

-- | Information about the location of application artifacts stored in GitHub.
--
--
--
-- /See:/ 'gitHubLocation' smart constructor.
data GitHubLocation = GitHubLocation'
  { _ghlCommitId   :: !(Maybe Text)
  , _ghlRepository :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GitHubLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghlCommitId' - The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
--
-- * 'ghlRepository' - The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.  Specified as account/repository.
gitHubLocation
    :: GitHubLocation
gitHubLocation =
  GitHubLocation' {_ghlCommitId = Nothing, _ghlRepository = Nothing}


-- | The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
ghlCommitId :: Lens' GitHubLocation (Maybe Text)
ghlCommitId = lens _ghlCommitId (\ s a -> s{_ghlCommitId = a})

-- | The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.  Specified as account/repository.
ghlRepository :: Lens' GitHubLocation (Maybe Text)
ghlRepository = lens _ghlRepository (\ s a -> s{_ghlRepository = a})

instance FromJSON GitHubLocation where
        parseJSON
          = withObject "GitHubLocation"
              (\ x ->
                 GitHubLocation' <$>
                   (x .:? "commitId") <*> (x .:? "repository"))

instance Hashable GitHubLocation where

instance NFData GitHubLocation where

instance ToJSON GitHubLocation where
        toJSON GitHubLocation'{..}
          = object
              (catMaybes
                 [("commitId" .=) <$> _ghlCommitId,
                  ("repository" .=) <$> _ghlRepository])

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
--
--
-- /See:/ 'greenFleetProvisioningOption' smart constructor.
newtype GreenFleetProvisioningOption = GreenFleetProvisioningOption'
  { _gfpoAction :: Maybe GreenFleetProvisioningAction
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GreenFleetProvisioningOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfpoAction' - The method used to add instances to a replacement environment.     * DISCOVER_EXISTING: Use instances that already exist or will be created manually.     * COPY_AUTO_SCALING_GROUP: Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
greenFleetProvisioningOption
    :: GreenFleetProvisioningOption
greenFleetProvisioningOption =
  GreenFleetProvisioningOption' {_gfpoAction = Nothing}


-- | The method used to add instances to a replacement environment.     * DISCOVER_EXISTING: Use instances that already exist or will be created manually.     * COPY_AUTO_SCALING_GROUP: Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
gfpoAction :: Lens' GreenFleetProvisioningOption (Maybe GreenFleetProvisioningAction)
gfpoAction = lens _gfpoAction (\ s a -> s{_gfpoAction = a})

instance FromJSON GreenFleetProvisioningOption where
        parseJSON
          = withObject "GreenFleetProvisioningOption"
              (\ x ->
                 GreenFleetProvisioningOption' <$> (x .:? "action"))

instance Hashable GreenFleetProvisioningOption where

instance NFData GreenFleetProvisioningOption where

instance ToJSON GreenFleetProvisioningOption where
        toJSON GreenFleetProvisioningOption'{..}
          = object (catMaybes [("action" .=) <$> _gfpoAction])

-- | Information about an on-premises instance.
--
--
--
-- /See:/ 'instanceInfo' smart constructor.
data InstanceInfo = InstanceInfo'
  { _iiRegisterTime   :: !(Maybe POSIX)
  , _iiInstanceARN    :: !(Maybe Text)
  , _iiDeregisterTime :: !(Maybe POSIX)
  , _iiIamUserARN     :: !(Maybe Text)
  , _iiInstanceName   :: !(Maybe Text)
  , _iiIamSessionARN  :: !(Maybe Text)
  , _iiTags           :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiRegisterTime' - The time at which the on-premises instance was registered.
--
-- * 'iiInstanceARN' - The ARN of the on-premises instance.
--
-- * 'iiDeregisterTime' - If the on-premises instance was deregistered, the time at which the on-premises instance was deregistered.
--
-- * 'iiIamUserARN' - The IAM user ARN associated with the on-premises instance.
--
-- * 'iiInstanceName' - The name of the on-premises instance.
--
-- * 'iiIamSessionARN' - The ARN of the IAM session associated with the on-premises instance.
--
-- * 'iiTags' - The tags currently associated with the on-premises instance.
instanceInfo
    :: InstanceInfo
instanceInfo =
  InstanceInfo'
    { _iiRegisterTime = Nothing
    , _iiInstanceARN = Nothing
    , _iiDeregisterTime = Nothing
    , _iiIamUserARN = Nothing
    , _iiInstanceName = Nothing
    , _iiIamSessionARN = Nothing
    , _iiTags = Nothing
    }


-- | The time at which the on-premises instance was registered.
iiRegisterTime :: Lens' InstanceInfo (Maybe UTCTime)
iiRegisterTime = lens _iiRegisterTime (\ s a -> s{_iiRegisterTime = a}) . mapping _Time

-- | The ARN of the on-premises instance.
iiInstanceARN :: Lens' InstanceInfo (Maybe Text)
iiInstanceARN = lens _iiInstanceARN (\ s a -> s{_iiInstanceARN = a})

-- | If the on-premises instance was deregistered, the time at which the on-premises instance was deregistered.
iiDeregisterTime :: Lens' InstanceInfo (Maybe UTCTime)
iiDeregisterTime = lens _iiDeregisterTime (\ s a -> s{_iiDeregisterTime = a}) . mapping _Time

-- | The IAM user ARN associated with the on-premises instance.
iiIamUserARN :: Lens' InstanceInfo (Maybe Text)
iiIamUserARN = lens _iiIamUserARN (\ s a -> s{_iiIamUserARN = a})

-- | The name of the on-premises instance.
iiInstanceName :: Lens' InstanceInfo (Maybe Text)
iiInstanceName = lens _iiInstanceName (\ s a -> s{_iiInstanceName = a})

-- | The ARN of the IAM session associated with the on-premises instance.
iiIamSessionARN :: Lens' InstanceInfo (Maybe Text)
iiIamSessionARN = lens _iiIamSessionARN (\ s a -> s{_iiIamSessionARN = a})

-- | The tags currently associated with the on-premises instance.
iiTags :: Lens' InstanceInfo [Tag]
iiTags = lens _iiTags (\ s a -> s{_iiTags = a}) . _Default . _Coerce

instance FromJSON InstanceInfo where
        parseJSON
          = withObject "InstanceInfo"
              (\ x ->
                 InstanceInfo' <$>
                   (x .:? "registerTime") <*> (x .:? "instanceArn") <*>
                     (x .:? "deregisterTime")
                     <*> (x .:? "iamUserArn")
                     <*> (x .:? "instanceName")
                     <*> (x .:? "iamSessionArn")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable InstanceInfo where

instance NFData InstanceInfo where

-- | Information about an instance in a deployment.
--
--
--
-- /See:/ 'instanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { _isInstanceId      :: !(Maybe Text)
  , _isStatus          :: !(Maybe InstanceStatus)
  , _isDeploymentId    :: !(Maybe Text)
  , _isLastUpdatedAt   :: !(Maybe POSIX)
  , _isLifecycleEvents :: !(Maybe [LifecycleEvent])
  , _isInstanceType    :: !(Maybe InstanceType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isInstanceId' - The instance ID.
--
-- * 'isStatus' - The deployment status for this instance:     * Pending: The deployment is pending for this instance.     * In Progress: The deployment is in progress for this instance.     * Succeeded: The deployment has succeeded for this instance.     * Failed: The deployment has failed for this instance.     * Skipped: The deployment has been skipped for this instance.     * Unknown: The deployment status is unknown for this instance.
--
-- * 'isDeploymentId' - The deployment ID.
--
-- * 'isLastUpdatedAt' - A timestamp indicating when the instance information was last updated.
--
-- * 'isLifecycleEvents' - A list of lifecycle events for this instance.
--
-- * 'isInstanceType' - Information about which environment an instance belongs to in a blue/green deployment.     * BLUE: The instance is part of the original environment.     * GREEN: The instance is part of the replacement environment.
instanceSummary
    :: InstanceSummary
instanceSummary =
  InstanceSummary'
    { _isInstanceId = Nothing
    , _isStatus = Nothing
    , _isDeploymentId = Nothing
    , _isLastUpdatedAt = Nothing
    , _isLifecycleEvents = Nothing
    , _isInstanceType = Nothing
    }


-- | The instance ID.
isInstanceId :: Lens' InstanceSummary (Maybe Text)
isInstanceId = lens _isInstanceId (\ s a -> s{_isInstanceId = a})

-- | The deployment status for this instance:     * Pending: The deployment is pending for this instance.     * In Progress: The deployment is in progress for this instance.     * Succeeded: The deployment has succeeded for this instance.     * Failed: The deployment has failed for this instance.     * Skipped: The deployment has been skipped for this instance.     * Unknown: The deployment status is unknown for this instance.
isStatus :: Lens' InstanceSummary (Maybe InstanceStatus)
isStatus = lens _isStatus (\ s a -> s{_isStatus = a})

-- | The deployment ID.
isDeploymentId :: Lens' InstanceSummary (Maybe Text)
isDeploymentId = lens _isDeploymentId (\ s a -> s{_isDeploymentId = a})

-- | A timestamp indicating when the instance information was last updated.
isLastUpdatedAt :: Lens' InstanceSummary (Maybe UTCTime)
isLastUpdatedAt = lens _isLastUpdatedAt (\ s a -> s{_isLastUpdatedAt = a}) . mapping _Time

-- | A list of lifecycle events for this instance.
isLifecycleEvents :: Lens' InstanceSummary [LifecycleEvent]
isLifecycleEvents = lens _isLifecycleEvents (\ s a -> s{_isLifecycleEvents = a}) . _Default . _Coerce

-- | Information about which environment an instance belongs to in a blue/green deployment.     * BLUE: The instance is part of the original environment.     * GREEN: The instance is part of the replacement environment.
isInstanceType :: Lens' InstanceSummary (Maybe InstanceType)
isInstanceType = lens _isInstanceType (\ s a -> s{_isInstanceType = a})

instance FromJSON InstanceSummary where
        parseJSON
          = withObject "InstanceSummary"
              (\ x ->
                 InstanceSummary' <$>
                   (x .:? "instanceId") <*> (x .:? "status") <*>
                     (x .:? "deploymentId")
                     <*> (x .:? "lastUpdatedAt")
                     <*> (x .:? "lifecycleEvents" .!= mempty)
                     <*> (x .:? "instanceType"))

instance Hashable InstanceSummary where

instance NFData InstanceSummary where

-- | Information about the most recent attempted or successful deployment to a deployment group.
--
--
--
-- /See:/ 'lastDeploymentInfo' smart constructor.
data LastDeploymentInfo = LastDeploymentInfo'
  { _ldiStatus       :: !(Maybe DeploymentStatus)
  , _ldiDeploymentId :: !(Maybe Text)
  , _ldiEndTime      :: !(Maybe POSIX)
  , _ldiCreateTime   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LastDeploymentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldiStatus' - The status of the most recent deployment.
--
-- * 'ldiDeploymentId' - The deployment ID.
--
-- * 'ldiEndTime' - A timestamp indicating when the most recent deployment to the deployment group completed.
--
-- * 'ldiCreateTime' - A timestamp indicating when the most recent deployment to the deployment group started.
lastDeploymentInfo
    :: LastDeploymentInfo
lastDeploymentInfo =
  LastDeploymentInfo'
    { _ldiStatus = Nothing
    , _ldiDeploymentId = Nothing
    , _ldiEndTime = Nothing
    , _ldiCreateTime = Nothing
    }


-- | The status of the most recent deployment.
ldiStatus :: Lens' LastDeploymentInfo (Maybe DeploymentStatus)
ldiStatus = lens _ldiStatus (\ s a -> s{_ldiStatus = a})

-- | The deployment ID.
ldiDeploymentId :: Lens' LastDeploymentInfo (Maybe Text)
ldiDeploymentId = lens _ldiDeploymentId (\ s a -> s{_ldiDeploymentId = a})

-- | A timestamp indicating when the most recent deployment to the deployment group completed.
ldiEndTime :: Lens' LastDeploymentInfo (Maybe UTCTime)
ldiEndTime = lens _ldiEndTime (\ s a -> s{_ldiEndTime = a}) . mapping _Time

-- | A timestamp indicating when the most recent deployment to the deployment group started.
ldiCreateTime :: Lens' LastDeploymentInfo (Maybe UTCTime)
ldiCreateTime = lens _ldiCreateTime (\ s a -> s{_ldiCreateTime = a}) . mapping _Time

instance FromJSON LastDeploymentInfo where
        parseJSON
          = withObject "LastDeploymentInfo"
              (\ x ->
                 LastDeploymentInfo' <$>
                   (x .:? "status") <*> (x .:? "deploymentId") <*>
                     (x .:? "endTime")
                     <*> (x .:? "createTime"))

instance Hashable LastDeploymentInfo where

instance NFData LastDeploymentInfo where

-- | Information about a deployment lifecycle event.
--
--
--
-- /See:/ 'lifecycleEvent' smart constructor.
data LifecycleEvent = LifecycleEvent'
  { _leStatus             :: !(Maybe LifecycleEventStatus)
  , _leLifecycleEventName :: !(Maybe Text)
  , _leStartTime          :: !(Maybe POSIX)
  , _leDiagnostics        :: !(Maybe Diagnostics)
  , _leEndTime            :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leStatus' - The deployment lifecycle event status:     * Pending: The deployment lifecycle event is pending.     * InProgress: The deployment lifecycle event is in progress.     * Succeeded: The deployment lifecycle event ran successfully.     * Failed: The deployment lifecycle event has failed.     * Skipped: The deployment lifecycle event has been skipped.     * Unknown: The deployment lifecycle event is unknown.
--
-- * 'leLifecycleEventName' - The deployment lifecycle event name, such as ApplicationStop, BeforeInstall, AfterInstall, ApplicationStart, or ValidateService.
--
-- * 'leStartTime' - A timestamp indicating when the deployment lifecycle event started.
--
-- * 'leDiagnostics' - Diagnostic information about the deployment lifecycle event.
--
-- * 'leEndTime' - A timestamp indicating when the deployment lifecycle event ended.
lifecycleEvent
    :: LifecycleEvent
lifecycleEvent =
  LifecycleEvent'
    { _leStatus = Nothing
    , _leLifecycleEventName = Nothing
    , _leStartTime = Nothing
    , _leDiagnostics = Nothing
    , _leEndTime = Nothing
    }


-- | The deployment lifecycle event status:     * Pending: The deployment lifecycle event is pending.     * InProgress: The deployment lifecycle event is in progress.     * Succeeded: The deployment lifecycle event ran successfully.     * Failed: The deployment lifecycle event has failed.     * Skipped: The deployment lifecycle event has been skipped.     * Unknown: The deployment lifecycle event is unknown.
leStatus :: Lens' LifecycleEvent (Maybe LifecycleEventStatus)
leStatus = lens _leStatus (\ s a -> s{_leStatus = a})

-- | The deployment lifecycle event name, such as ApplicationStop, BeforeInstall, AfterInstall, ApplicationStart, or ValidateService.
leLifecycleEventName :: Lens' LifecycleEvent (Maybe Text)
leLifecycleEventName = lens _leLifecycleEventName (\ s a -> s{_leLifecycleEventName = a})

-- | A timestamp indicating when the deployment lifecycle event started.
leStartTime :: Lens' LifecycleEvent (Maybe UTCTime)
leStartTime = lens _leStartTime (\ s a -> s{_leStartTime = a}) . mapping _Time

-- | Diagnostic information about the deployment lifecycle event.
leDiagnostics :: Lens' LifecycleEvent (Maybe Diagnostics)
leDiagnostics = lens _leDiagnostics (\ s a -> s{_leDiagnostics = a})

-- | A timestamp indicating when the deployment lifecycle event ended.
leEndTime :: Lens' LifecycleEvent (Maybe UTCTime)
leEndTime = lens _leEndTime (\ s a -> s{_leEndTime = a}) . mapping _Time

instance FromJSON LifecycleEvent where
        parseJSON
          = withObject "LifecycleEvent"
              (\ x ->
                 LifecycleEvent' <$>
                   (x .:? "status") <*> (x .:? "lifecycleEventName") <*>
                     (x .:? "startTime")
                     <*> (x .:? "diagnostics")
                     <*> (x .:? "endTime"))

instance Hashable LifecycleEvent where

instance NFData LifecycleEvent where

-- | Information about the Elastic Load Balancing load balancer or target group used in a deployment.
--
--
--
-- /See:/ 'loadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { _lbiElbInfoList         :: !(Maybe [ELBInfo])
  , _lbiTargetGroupInfoList :: !(Maybe [TargetGroupInfo])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbiElbInfoList' - An array containing information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
--
-- * 'lbiTargetGroupInfoList' - An array containing information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
loadBalancerInfo
    :: LoadBalancerInfo
loadBalancerInfo =
  LoadBalancerInfo'
    {_lbiElbInfoList = Nothing, _lbiTargetGroupInfoList = Nothing}


-- | An array containing information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
lbiElbInfoList :: Lens' LoadBalancerInfo [ELBInfo]
lbiElbInfoList = lens _lbiElbInfoList (\ s a -> s{_lbiElbInfoList = a}) . _Default . _Coerce

-- | An array containing information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
lbiTargetGroupInfoList :: Lens' LoadBalancerInfo [TargetGroupInfo]
lbiTargetGroupInfoList = lens _lbiTargetGroupInfoList (\ s a -> s{_lbiTargetGroupInfoList = a}) . _Default . _Coerce

instance FromJSON LoadBalancerInfo where
        parseJSON
          = withObject "LoadBalancerInfo"
              (\ x ->
                 LoadBalancerInfo' <$>
                   (x .:? "elbInfoList" .!= mempty) <*>
                     (x .:? "targetGroupInfoList" .!= mempty))

instance Hashable LoadBalancerInfo where

instance NFData LoadBalancerInfo where

instance ToJSON LoadBalancerInfo where
        toJSON LoadBalancerInfo'{..}
          = object
              (catMaybes
                 [("elbInfoList" .=) <$> _lbiElbInfoList,
                  ("targetGroupInfoList" .=) <$>
                    _lbiTargetGroupInfoList])

-- | Information about minimum healthy instance.
--
--
--
-- /See:/ 'minimumHealthyHosts' smart constructor.
data MinimumHealthyHosts = MinimumHealthyHosts'
  { _mhhValue :: !(Maybe Int)
  , _mhhType  :: !(Maybe MinimumHealthyHostsType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MinimumHealthyHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhhValue' - The minimum healthy instance value.
--
-- * 'mhhType' - The minimum healthy instance type:     * HOST_COUNT: The minimum number of healthy instance as an absolute value.     * FLEET_PERCENT: The minimum number of healthy instance as a percentage of the total number of instance in the deployment. In an example of nine instance, if a HOST_COUNT of six is specified, deploy to up to three instances at a time. The deployment will be successful if six or more instances are deployed to successfully; otherwise, the deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to five instance at a time. The deployment will be successful if four or more instance are deployed to successfully; otherwise, the deployment fails. For more information, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html AWS CodeDeploy Instance Health> in the /AWS CodeDeploy User Guide/ .
minimumHealthyHosts
    :: MinimumHealthyHosts
minimumHealthyHosts =
  MinimumHealthyHosts' {_mhhValue = Nothing, _mhhType = Nothing}


-- | The minimum healthy instance value.
mhhValue :: Lens' MinimumHealthyHosts (Maybe Int)
mhhValue = lens _mhhValue (\ s a -> s{_mhhValue = a})

-- | The minimum healthy instance type:     * HOST_COUNT: The minimum number of healthy instance as an absolute value.     * FLEET_PERCENT: The minimum number of healthy instance as a percentage of the total number of instance in the deployment. In an example of nine instance, if a HOST_COUNT of six is specified, deploy to up to three instances at a time. The deployment will be successful if six or more instances are deployed to successfully; otherwise, the deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to five instance at a time. The deployment will be successful if four or more instance are deployed to successfully; otherwise, the deployment fails. For more information, see <http://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html AWS CodeDeploy Instance Health> in the /AWS CodeDeploy User Guide/ .
mhhType :: Lens' MinimumHealthyHosts (Maybe MinimumHealthyHostsType)
mhhType = lens _mhhType (\ s a -> s{_mhhType = a})

instance FromJSON MinimumHealthyHosts where
        parseJSON
          = withObject "MinimumHealthyHosts"
              (\ x ->
                 MinimumHealthyHosts' <$>
                   (x .:? "value") <*> (x .:? "type"))

instance Hashable MinimumHealthyHosts where

instance NFData MinimumHealthyHosts where

instance ToJSON MinimumHealthyHosts where
        toJSON MinimumHealthyHosts'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _mhhValue,
                  ("type" .=) <$> _mhhType])

-- | Information about groups of on-premises instance tags.
--
--
--
-- /See:/ 'onPremisesTagSet' smart constructor.
newtype OnPremisesTagSet = OnPremisesTagSet'
  { _optsOnPremisesTagSetList :: Maybe [[TagFilter]]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OnPremisesTagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'optsOnPremisesTagSetList' - A list containing other lists of on-premises instance tag groups. In order for an instance to be included in the deployment group, it must be identified by all the tag groups in the list.
onPremisesTagSet
    :: OnPremisesTagSet
onPremisesTagSet = OnPremisesTagSet' {_optsOnPremisesTagSetList = Nothing}


-- | A list containing other lists of on-premises instance tag groups. In order for an instance to be included in the deployment group, it must be identified by all the tag groups in the list.
optsOnPremisesTagSetList :: Lens' OnPremisesTagSet [[TagFilter]]
optsOnPremisesTagSetList = lens _optsOnPremisesTagSetList (\ s a -> s{_optsOnPremisesTagSetList = a}) . _Default . _Coerce

instance FromJSON OnPremisesTagSet where
        parseJSON
          = withObject "OnPremisesTagSet"
              (\ x ->
                 OnPremisesTagSet' <$>
                   (x .:? "onPremisesTagSetList" .!= mempty))

instance Hashable OnPremisesTagSet where

instance NFData OnPremisesTagSet where

instance ToJSON OnPremisesTagSet where
        toJSON OnPremisesTagSet'{..}
          = object
              (catMaybes
                 [("onPremisesTagSetList" .=) <$>
                    _optsOnPremisesTagSetList])

-- | A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.
--
--
--
-- /See:/ 'rawString' smart constructor.
data RawString = RawString'
  { _rsContent :: !(Maybe Text)
  , _rsSha256  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RawString' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsContent' - The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
--
-- * 'rsSha256' - The SHA256 hash value of the revision that is specified as a RawString.
rawString
    :: RawString
rawString = RawString' {_rsContent = Nothing, _rsSha256 = Nothing}


-- | The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
rsContent :: Lens' RawString (Maybe Text)
rsContent = lens _rsContent (\ s a -> s{_rsContent = a})

-- | The SHA256 hash value of the revision that is specified as a RawString.
rsSha256 :: Lens' RawString (Maybe Text)
rsSha256 = lens _rsSha256 (\ s a -> s{_rsSha256 = a})

instance FromJSON RawString where
        parseJSON
          = withObject "RawString"
              (\ x ->
                 RawString' <$>
                   (x .:? "content") <*> (x .:? "sha256"))

instance Hashable RawString where

instance NFData RawString where

instance ToJSON RawString where
        toJSON RawString'{..}
          = object
              (catMaybes
                 [("content" .=) <$> _rsContent,
                  ("sha256" .=) <$> _rsSha256])

-- | Information about an application revision.
--
--
--
-- /See:/ 'revisionInfo' smart constructor.
data RevisionInfo = RevisionInfo'
  { _riGenericRevisionInfo :: !(Maybe GenericRevisionInfo)
  , _riRevisionLocation    :: !(Maybe RevisionLocation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevisionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riGenericRevisionInfo' - Information about an application revision, including usage details and associated deployment groups.
--
-- * 'riRevisionLocation' - Information about the location and type of an application revision.
revisionInfo
    :: RevisionInfo
revisionInfo =
  RevisionInfo'
    {_riGenericRevisionInfo = Nothing, _riRevisionLocation = Nothing}


-- | Information about an application revision, including usage details and associated deployment groups.
riGenericRevisionInfo :: Lens' RevisionInfo (Maybe GenericRevisionInfo)
riGenericRevisionInfo = lens _riGenericRevisionInfo (\ s a -> s{_riGenericRevisionInfo = a})

-- | Information about the location and type of an application revision.
riRevisionLocation :: Lens' RevisionInfo (Maybe RevisionLocation)
riRevisionLocation = lens _riRevisionLocation (\ s a -> s{_riRevisionLocation = a})

instance FromJSON RevisionInfo where
        parseJSON
          = withObject "RevisionInfo"
              (\ x ->
                 RevisionInfo' <$>
                   (x .:? "genericRevisionInfo") <*>
                     (x .:? "revisionLocation"))

instance Hashable RevisionInfo where

instance NFData RevisionInfo where

-- | Information about the location of an application revision.
--
--
--
-- /See:/ 'revisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { _rlString         :: !(Maybe RawString)
  , _rlRevisionType   :: !(Maybe RevisionLocationType)
  , _rlS3Location     :: !(Maybe S3Location)
  , _rlGitHubLocation :: !(Maybe GitHubLocation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevisionLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlString' - Information about the location of an AWS Lambda deployment revision stored as a RawString.
--
-- * 'rlRevisionType' - The type of application revision:     * S3: An application revision stored in Amazon S3.     * GitHub: An application revision stored in GitHub (EC2/On-premises deployments only)     * String: A YAML-formatted or JSON-formatted string (AWS Lambda deployments only)
--
-- * 'rlS3Location' - Information about the location of a revision stored in Amazon S3.
--
-- * 'rlGitHubLocation' - Information about the location of application artifacts stored in GitHub.
revisionLocation
    :: RevisionLocation
revisionLocation =
  RevisionLocation'
    { _rlString = Nothing
    , _rlRevisionType = Nothing
    , _rlS3Location = Nothing
    , _rlGitHubLocation = Nothing
    }


-- | Information about the location of an AWS Lambda deployment revision stored as a RawString.
rlString :: Lens' RevisionLocation (Maybe RawString)
rlString = lens _rlString (\ s a -> s{_rlString = a})

-- | The type of application revision:     * S3: An application revision stored in Amazon S3.     * GitHub: An application revision stored in GitHub (EC2/On-premises deployments only)     * String: A YAML-formatted or JSON-formatted string (AWS Lambda deployments only)
rlRevisionType :: Lens' RevisionLocation (Maybe RevisionLocationType)
rlRevisionType = lens _rlRevisionType (\ s a -> s{_rlRevisionType = a})

-- | Information about the location of a revision stored in Amazon S3.
rlS3Location :: Lens' RevisionLocation (Maybe S3Location)
rlS3Location = lens _rlS3Location (\ s a -> s{_rlS3Location = a})

-- | Information about the location of application artifacts stored in GitHub.
rlGitHubLocation :: Lens' RevisionLocation (Maybe GitHubLocation)
rlGitHubLocation = lens _rlGitHubLocation (\ s a -> s{_rlGitHubLocation = a})

instance FromJSON RevisionLocation where
        parseJSON
          = withObject "RevisionLocation"
              (\ x ->
                 RevisionLocation' <$>
                   (x .:? "string") <*> (x .:? "revisionType") <*>
                     (x .:? "s3Location")
                     <*> (x .:? "gitHubLocation"))

instance Hashable RevisionLocation where

instance NFData RevisionLocation where

instance ToJSON RevisionLocation where
        toJSON RevisionLocation'{..}
          = object
              (catMaybes
                 [("string" .=) <$> _rlString,
                  ("revisionType" .=) <$> _rlRevisionType,
                  ("s3Location" .=) <$> _rlS3Location,
                  ("gitHubLocation" .=) <$> _rlGitHubLocation])

-- | Information about a deployment rollback.
--
--
--
-- /See:/ 'rollbackInfo' smart constructor.
data RollbackInfo = RollbackInfo'
  { _riRollbackTriggeringDeploymentId :: !(Maybe Text)
  , _riRollbackMessage                :: !(Maybe Text)
  , _riRollbackDeploymentId           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RollbackInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riRollbackTriggeringDeploymentId' - The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
--
-- * 'riRollbackMessage' - Information describing the status of a deployment rollback; for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded.
--
-- * 'riRollbackDeploymentId' - The ID of the deployment rollback.
rollbackInfo
    :: RollbackInfo
rollbackInfo =
  RollbackInfo'
    { _riRollbackTriggeringDeploymentId = Nothing
    , _riRollbackMessage = Nothing
    , _riRollbackDeploymentId = Nothing
    }


-- | The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
riRollbackTriggeringDeploymentId :: Lens' RollbackInfo (Maybe Text)
riRollbackTriggeringDeploymentId = lens _riRollbackTriggeringDeploymentId (\ s a -> s{_riRollbackTriggeringDeploymentId = a})

-- | Information describing the status of a deployment rollback; for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded.
riRollbackMessage :: Lens' RollbackInfo (Maybe Text)
riRollbackMessage = lens _riRollbackMessage (\ s a -> s{_riRollbackMessage = a})

-- | The ID of the deployment rollback.
riRollbackDeploymentId :: Lens' RollbackInfo (Maybe Text)
riRollbackDeploymentId = lens _riRollbackDeploymentId (\ s a -> s{_riRollbackDeploymentId = a})

instance FromJSON RollbackInfo where
        parseJSON
          = withObject "RollbackInfo"
              (\ x ->
                 RollbackInfo' <$>
                   (x .:? "rollbackTriggeringDeploymentId") <*>
                     (x .:? "rollbackMessage")
                     <*> (x .:? "rollbackDeploymentId"))

instance Hashable RollbackInfo where

instance NFData RollbackInfo where

-- | Information about the location of application artifacts stored in Amazon S3.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBundleType :: !(Maybe BundleType)
  , _slETag       :: !(Maybe Text)
  , _slBucket     :: !(Maybe Text)
  , _slKey        :: !(Maybe Text)
  , _slVersion    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBundleType' - The file type of the application revision. Must be one of the following:     * tar: A tar archive file.     * tgz: A compressed tar archive file.     * zip: A zip archive file.
--
-- * 'slETag' - The ETag of the Amazon S3 object that represents the bundled artifacts for the application revision. If the ETag is not specified as an input parameter, ETag validation of the object will be skipped.
--
-- * 'slBucket' - The name of the Amazon S3 bucket where the application revision is stored.
--
-- * 'slKey' - The name of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- * 'slVersion' - A specific version of the Amazon S3 object that represents the bundled artifacts for the application revision. If the version is not specified, the system will use the most recent version by default.
s3Location
    :: S3Location
s3Location =
  S3Location'
    { _slBundleType = Nothing
    , _slETag = Nothing
    , _slBucket = Nothing
    , _slKey = Nothing
    , _slVersion = Nothing
    }


-- | The file type of the application revision. Must be one of the following:     * tar: A tar archive file.     * tgz: A compressed tar archive file.     * zip: A zip archive file.
slBundleType :: Lens' S3Location (Maybe BundleType)
slBundleType = lens _slBundleType (\ s a -> s{_slBundleType = a})

-- | The ETag of the Amazon S3 object that represents the bundled artifacts for the application revision. If the ETag is not specified as an input parameter, ETag validation of the object will be skipped.
slETag :: Lens' S3Location (Maybe Text)
slETag = lens _slETag (\ s a -> s{_slETag = a})

-- | The name of the Amazon S3 bucket where the application revision is stored.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a})

-- | The name of the Amazon S3 object that represents the bundled artifacts for the application revision.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a})

-- | A specific version of the Amazon S3 object that represents the bundled artifacts for the application revision. If the version is not specified, the system will use the most recent version by default.
slVersion :: Lens' S3Location (Maybe Text)
slVersion = lens _slVersion (\ s a -> s{_slVersion = a})

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$>
                   (x .:? "bundleType") <*> (x .:? "eTag") <*>
                     (x .:? "bucket")
                     <*> (x .:? "key")
                     <*> (x .:? "version"))

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("bundleType" .=) <$> _slBundleType,
                  ("eTag" .=) <$> _slETag, ("bucket" .=) <$> _slBucket,
                  ("key" .=) <$> _slKey,
                  ("version" .=) <$> _slVersion])

-- | Information about a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The tag's value.
--
-- * 'tagKey' - The tag's key.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The tag's value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The tag's key.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

-- | Information about an on-premises instance tag filter.
--
--
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter'
  { _tfValue :: !(Maybe Text)
  , _tfKey   :: !(Maybe Text)
  , _tfType  :: !(Maybe TagFilterType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfValue' - The on-premises instance tag filter value.
--
-- * 'tfKey' - The on-premises instance tag filter key.
--
-- * 'tfType' - The on-premises instance tag filter type:     * KEY_ONLY: Key only.     * VALUE_ONLY: Value only.     * KEY_AND_VALUE: Key and value.
tagFilter
    :: TagFilter
tagFilter = TagFilter' {_tfValue = Nothing, _tfKey = Nothing, _tfType = Nothing}


-- | The on-premises instance tag filter value.
tfValue :: Lens' TagFilter (Maybe Text)
tfValue = lens _tfValue (\ s a -> s{_tfValue = a})

-- | The on-premises instance tag filter key.
tfKey :: Lens' TagFilter (Maybe Text)
tfKey = lens _tfKey (\ s a -> s{_tfKey = a})

-- | The on-premises instance tag filter type:     * KEY_ONLY: Key only.     * VALUE_ONLY: Value only.     * KEY_AND_VALUE: Key and value.
tfType :: Lens' TagFilter (Maybe TagFilterType)
tfType = lens _tfType (\ s a -> s{_tfType = a})

instance FromJSON TagFilter where
        parseJSON
          = withObject "TagFilter"
              (\ x ->
                 TagFilter' <$>
                   (x .:? "Value") <*> (x .:? "Key") <*> (x .:? "Type"))

instance Hashable TagFilter where

instance NFData TagFilter where

instance ToJSON TagFilter where
        toJSON TagFilter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tfValue, ("Key" .=) <$> _tfKey,
                  ("Type" .=) <$> _tfType])

-- | Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.
--
--
--
-- /See:/ 'targetGroupInfo' smart constructor.
newtype TargetGroupInfo = TargetGroupInfo'
  { _tgiName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroupInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgiName' - For blue/green deployments, the name of the target group that instances in the original environment are deregistered from, and instances in the replacement environment registered with. For in-place deployments, the name of the target group that instances are deregistered from, so they are not serving traffic during a deployment, and then re-registered with after the deployment completes.
targetGroupInfo
    :: TargetGroupInfo
targetGroupInfo = TargetGroupInfo' {_tgiName = Nothing}


-- | For blue/green deployments, the name of the target group that instances in the original environment are deregistered from, and instances in the replacement environment registered with. For in-place deployments, the name of the target group that instances are deregistered from, so they are not serving traffic during a deployment, and then re-registered with after the deployment completes.
tgiName :: Lens' TargetGroupInfo (Maybe Text)
tgiName = lens _tgiName (\ s a -> s{_tgiName = a})

instance FromJSON TargetGroupInfo where
        parseJSON
          = withObject "TargetGroupInfo"
              (\ x -> TargetGroupInfo' <$> (x .:? "name"))

instance Hashable TargetGroupInfo where

instance NFData TargetGroupInfo where

instance ToJSON TargetGroupInfo where
        toJSON TargetGroupInfo'{..}
          = object (catMaybes [("name" .=) <$> _tgiName])

-- | Information about the instances to be used in the replacement environment in a blue/green deployment.
--
--
--
-- /See:/ 'targetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { _tiEc2TagSet         :: !(Maybe EC2TagSet)
  , _tiTagFilters        :: !(Maybe [EC2TagFilter])
  , _tiAutoScalingGroups :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiEc2TagSet' - Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as tagFilters.
--
-- * 'tiTagFilters' - The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as ec2TagSet.
--
-- * 'tiAutoScalingGroups' - The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
targetInstances
    :: TargetInstances
targetInstances =
  TargetInstances'
    { _tiEc2TagSet = Nothing
    , _tiTagFilters = Nothing
    , _tiAutoScalingGroups = Nothing
    }


-- | Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as tagFilters.
tiEc2TagSet :: Lens' TargetInstances (Maybe EC2TagSet)
tiEc2TagSet = lens _tiEc2TagSet (\ s a -> s{_tiEc2TagSet = a})

-- | The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as ec2TagSet.
tiTagFilters :: Lens' TargetInstances [EC2TagFilter]
tiTagFilters = lens _tiTagFilters (\ s a -> s{_tiTagFilters = a}) . _Default . _Coerce

-- | The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
tiAutoScalingGroups :: Lens' TargetInstances [Text]
tiAutoScalingGroups = lens _tiAutoScalingGroups (\ s a -> s{_tiAutoScalingGroups = a}) . _Default . _Coerce

instance FromJSON TargetInstances where
        parseJSON
          = withObject "TargetInstances"
              (\ x ->
                 TargetInstances' <$>
                   (x .:? "ec2TagSet") <*>
                     (x .:? "tagFilters" .!= mempty)
                     <*> (x .:? "autoScalingGroups" .!= mempty))

instance Hashable TargetInstances where

instance NFData TargetInstances where

instance ToJSON TargetInstances where
        toJSON TargetInstances'{..}
          = object
              (catMaybes
                 [("ec2TagSet" .=) <$> _tiEc2TagSet,
                  ("tagFilters" .=) <$> _tiTagFilters,
                  ("autoScalingGroups" .=) <$> _tiAutoScalingGroups])

-- | A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.
--
--
--
-- /See:/ 'timeBasedCanary' smart constructor.
data TimeBasedCanary = TimeBasedCanary'
  { _tbcCanaryInterval   :: !(Maybe Int)
  , _tbcCanaryPercentage :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeBasedCanary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbcCanaryInterval' - The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
--
-- * 'tbcCanaryPercentage' - The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
timeBasedCanary
    :: TimeBasedCanary
timeBasedCanary =
  TimeBasedCanary'
    {_tbcCanaryInterval = Nothing, _tbcCanaryPercentage = Nothing}


-- | The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
tbcCanaryInterval :: Lens' TimeBasedCanary (Maybe Int)
tbcCanaryInterval = lens _tbcCanaryInterval (\ s a -> s{_tbcCanaryInterval = a})

-- | The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
tbcCanaryPercentage :: Lens' TimeBasedCanary (Maybe Int)
tbcCanaryPercentage = lens _tbcCanaryPercentage (\ s a -> s{_tbcCanaryPercentage = a})

instance FromJSON TimeBasedCanary where
        parseJSON
          = withObject "TimeBasedCanary"
              (\ x ->
                 TimeBasedCanary' <$>
                   (x .:? "canaryInterval") <*>
                     (x .:? "canaryPercentage"))

instance Hashable TimeBasedCanary where

instance NFData TimeBasedCanary where

instance ToJSON TimeBasedCanary where
        toJSON TimeBasedCanary'{..}
          = object
              (catMaybes
                 [("canaryInterval" .=) <$> _tbcCanaryInterval,
                  ("canaryPercentage" .=) <$> _tbcCanaryPercentage])

-- | A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.
--
--
--
-- /See:/ 'timeBasedLinear' smart constructor.
data TimeBasedLinear = TimeBasedLinear'
  { _tblLinearInterval   :: !(Maybe Int)
  , _tblLinearPercentage :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeBasedLinear' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tblLinearInterval' - The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
--
-- * 'tblLinearPercentage' - The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
timeBasedLinear
    :: TimeBasedLinear
timeBasedLinear =
  TimeBasedLinear'
    {_tblLinearInterval = Nothing, _tblLinearPercentage = Nothing}


-- | The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
tblLinearInterval :: Lens' TimeBasedLinear (Maybe Int)
tblLinearInterval = lens _tblLinearInterval (\ s a -> s{_tblLinearInterval = a})

-- | The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
tblLinearPercentage :: Lens' TimeBasedLinear (Maybe Int)
tblLinearPercentage = lens _tblLinearPercentage (\ s a -> s{_tblLinearPercentage = a})

instance FromJSON TimeBasedLinear where
        parseJSON
          = withObject "TimeBasedLinear"
              (\ x ->
                 TimeBasedLinear' <$>
                   (x .:? "linearInterval") <*>
                     (x .:? "linearPercentage"))

instance Hashable TimeBasedLinear where

instance NFData TimeBasedLinear where

instance ToJSON TimeBasedLinear where
        toJSON TimeBasedLinear'{..}
          = object
              (catMaybes
                 [("linearInterval" .=) <$> _tblLinearInterval,
                  ("linearPercentage" .=) <$> _tblLinearPercentage])

-- | Information about a time range.
--
--
--
-- /See:/ 'timeRange' smart constructor.
data TimeRange = TimeRange'
  { _trStart :: !(Maybe POSIX)
  , _trEnd   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trStart' - The start time of the time range.
--
-- * 'trEnd' - The end time of the time range.
timeRange
    :: TimeRange
timeRange = TimeRange' {_trStart = Nothing, _trEnd = Nothing}


-- | The start time of the time range.
trStart :: Lens' TimeRange (Maybe UTCTime)
trStart = lens _trStart (\ s a -> s{_trStart = a}) . mapping _Time

-- | The end time of the time range.
trEnd :: Lens' TimeRange (Maybe UTCTime)
trEnd = lens _trEnd (\ s a -> s{_trEnd = a}) . mapping _Time

instance Hashable TimeRange where

instance NFData TimeRange where

instance ToJSON TimeRange where
        toJSON TimeRange'{..}
          = object
              (catMaybes
                 [("start" .=) <$> _trStart, ("end" .=) <$> _trEnd])

-- | The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment.
--
--
--
-- /See:/ 'trafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { _trcTimeBasedCanary :: !(Maybe TimeBasedCanary)
  , _trcTimeBasedLinear :: !(Maybe TimeBasedLinear)
  , _trcType            :: !(Maybe TrafficRoutingType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrafficRoutingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trcTimeBasedCanary' - A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.
--
-- * 'trcTimeBasedLinear' - A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.
--
-- * 'trcType' - The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration .
trafficRoutingConfig
    :: TrafficRoutingConfig
trafficRoutingConfig =
  TrafficRoutingConfig'
    { _trcTimeBasedCanary = Nothing
    , _trcTimeBasedLinear = Nothing
    , _trcType = Nothing
    }


-- | A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.
trcTimeBasedCanary :: Lens' TrafficRoutingConfig (Maybe TimeBasedCanary)
trcTimeBasedCanary = lens _trcTimeBasedCanary (\ s a -> s{_trcTimeBasedCanary = a})

-- | A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.
trcTimeBasedLinear :: Lens' TrafficRoutingConfig (Maybe TimeBasedLinear)
trcTimeBasedLinear = lens _trcTimeBasedLinear (\ s a -> s{_trcTimeBasedLinear = a})

-- | The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration .
trcType :: Lens' TrafficRoutingConfig (Maybe TrafficRoutingType)
trcType = lens _trcType (\ s a -> s{_trcType = a})

instance FromJSON TrafficRoutingConfig where
        parseJSON
          = withObject "TrafficRoutingConfig"
              (\ x ->
                 TrafficRoutingConfig' <$>
                   (x .:? "timeBasedCanary") <*>
                     (x .:? "timeBasedLinear")
                     <*> (x .:? "type"))

instance Hashable TrafficRoutingConfig where

instance NFData TrafficRoutingConfig where

instance ToJSON TrafficRoutingConfig where
        toJSON TrafficRoutingConfig'{..}
          = object
              (catMaybes
                 [("timeBasedCanary" .=) <$> _trcTimeBasedCanary,
                  ("timeBasedLinear" .=) <$> _trcTimeBasedLinear,
                  ("type" .=) <$> _trcType])

-- | Information about notification triggers for the deployment group.
--
--
--
-- /See:/ 'triggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { _tcTriggerName      :: !(Maybe Text)
  , _tcTriggerEvents    :: !(Maybe [TriggerEventType])
  , _tcTriggerTargetARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TriggerConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcTriggerName' - The name of the notification trigger.
--
-- * 'tcTriggerEvents' - The event type or types for which notifications are triggered.
--
-- * 'tcTriggerTargetARN' - The ARN of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
triggerConfig
    :: TriggerConfig
triggerConfig =
  TriggerConfig'
    { _tcTriggerName = Nothing
    , _tcTriggerEvents = Nothing
    , _tcTriggerTargetARN = Nothing
    }


-- | The name of the notification trigger.
tcTriggerName :: Lens' TriggerConfig (Maybe Text)
tcTriggerName = lens _tcTriggerName (\ s a -> s{_tcTriggerName = a})

-- | The event type or types for which notifications are triggered.
tcTriggerEvents :: Lens' TriggerConfig [TriggerEventType]
tcTriggerEvents = lens _tcTriggerEvents (\ s a -> s{_tcTriggerEvents = a}) . _Default . _Coerce

-- | The ARN of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
tcTriggerTargetARN :: Lens' TriggerConfig (Maybe Text)
tcTriggerTargetARN = lens _tcTriggerTargetARN (\ s a -> s{_tcTriggerTargetARN = a})

instance FromJSON TriggerConfig where
        parseJSON
          = withObject "TriggerConfig"
              (\ x ->
                 TriggerConfig' <$>
                   (x .:? "triggerName") <*>
                     (x .:? "triggerEvents" .!= mempty)
                     <*> (x .:? "triggerTargetArn"))

instance Hashable TriggerConfig where

instance NFData TriggerConfig where

instance ToJSON TriggerConfig where
        toJSON TriggerConfig'{..}
          = object
              (catMaybes
                 [("triggerName" .=) <$> _tcTriggerName,
                  ("triggerEvents" .=) <$> _tcTriggerEvents,
                  ("triggerTargetArn" .=) <$> _tcTriggerTargetARN])
