{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentInfo where

import Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
import Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.DeploymentCreator
import Network.AWS.CodeDeploy.Types.DeploymentOverview
import Network.AWS.CodeDeploy.Types.DeploymentStatus
import Network.AWS.CodeDeploy.Types.DeploymentStyle
import Network.AWS.CodeDeploy.Types.ErrorInformation
import Network.AWS.CodeDeploy.Types.FileExistsBehavior
import Network.AWS.CodeDeploy.Types.LoadBalancerInfo
import Network.AWS.CodeDeploy.Types.RevisionLocation
import Network.AWS.CodeDeploy.Types.RollbackInfo
import Network.AWS.CodeDeploy.Types.TargetInstances
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment.
--
--
--
-- /See:/ 'deploymentInfo' smart constructor.
data DeploymentInfo = DeploymentInfo'
  { _diCreator ::
      !(Maybe DeploymentCreator),
    _diStatus :: !(Maybe DeploymentStatus),
    _diDeploymentId :: !(Maybe Text),
    _diDeploymentConfigName :: !(Maybe Text),
    _diComputePlatform :: !(Maybe ComputePlatform),
    _diPreviousRevision :: !(Maybe RevisionLocation),
    _diInstanceTerminationWaitTimeStarted :: !(Maybe Bool),
    _diDeploymentStatusMessages :: !(Maybe [Text]),
    _diStartTime :: !(Maybe POSIX),
    _diCompleteTime :: !(Maybe POSIX),
    _diBlueGreenDeploymentConfiguration ::
      !(Maybe BlueGreenDeploymentConfiguration),
    _diErrorInformation :: !(Maybe ErrorInformation),
    _diLoadBalancerInfo :: !(Maybe LoadBalancerInfo),
    _diAdditionalDeploymentStatusInfo :: !(Maybe Text),
    _diDeploymentOverview :: !(Maybe DeploymentOverview),
    _diFileExistsBehavior :: !(Maybe FileExistsBehavior),
    _diApplicationName :: !(Maybe Text),
    _diRollbackInfo :: !(Maybe RollbackInfo),
    _diExternalId :: !(Maybe Text),
    _diTargetInstances :: !(Maybe TargetInstances),
    _diRevision :: !(Maybe RevisionLocation),
    _diDescription :: !(Maybe Text),
    _diDeploymentStyle :: !(Maybe DeploymentStyle),
    _diCreateTime :: !(Maybe POSIX),
    _diAutoRollbackConfiguration ::
      !(Maybe AutoRollbackConfiguration),
    _diUpdateOutdatedInstancesOnly :: !(Maybe Bool),
    _diDeploymentGroupName :: !(Maybe Text),
    _diIgnoreApplicationStopFailures :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diCreator' - The means by which the deployment was created:     * @user@ : A user created the deployment.     * @autoscaling@ : Amazon EC2 Auto Scaling created the deployment.     * @codeDeployRollback@ : A rollback process created the deployment.
--
-- * 'diStatus' - The current state of the deployment as a whole.
--
-- * 'diDeploymentId' - The unique ID of a deployment.
--
-- * 'diDeploymentConfigName' - The deployment configuration name.
--
-- * 'diComputePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- * 'diPreviousRevision' - Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
--
-- * 'diInstanceTerminationWaitTimeStarted' - Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified. Otherwise, 'true' as soon as the termination wait period starts.
--
-- * 'diDeploymentStatusMessages' - Messages that contain information about the status of a deployment.
--
-- * 'diStartTime' - A timestamp that indicates when the deployment was deployed to the deployment group. In some cases, the reported value of the start time might be later than the complete time. This is due to differences in the clock settings of backend servers that participate in the deployment process.
--
-- * 'diCompleteTime' - A timestamp that indicates when the deployment was complete.
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
-- * 'diFileExistsBehavior' - Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.     * @DISALLOW@ : The deployment fails. This is also the default behavior if no option is specified.     * @OVERWRITE@ : The version of the file from the application revision currently being deployed replaces the version already on the instance.     * @RETAIN@ : The version of the file already on the instance is kept and used as part of the new deployment.
--
-- * 'diApplicationName' - The application name.
--
-- * 'diRollbackInfo' - Information about a deployment rollback.
--
-- * 'diExternalId' - The unique ID for an external resource (for example, a CloudFormation stack ID) that is linked to this deployment.
--
-- * 'diTargetInstances' - Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- * 'diRevision' - Information about the location of stored application artifacts and the service from which to retrieve them.
--
-- * 'diDescription' - A comment about the deployment.
--
-- * 'diDeploymentStyle' - Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- * 'diCreateTime' - A timestamp that indicates when the deployment was created.
--
-- * 'diAutoRollbackConfiguration' - Information about the automatic rollback configuration associated with the deployment.
--
-- * 'diUpdateOutdatedInstancesOnly' - Indicates whether only instances that are not running the latest application revision are to be deployed to.
--
-- * 'diDeploymentGroupName' - The deployment group name.
--
-- * 'diIgnoreApplicationStopFailures' - If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ .  If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted.  During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail.  If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored.
deploymentInfo ::
  DeploymentInfo
deploymentInfo =
  DeploymentInfo'
    { _diCreator = Nothing,
      _diStatus = Nothing,
      _diDeploymentId = Nothing,
      _diDeploymentConfigName = Nothing,
      _diComputePlatform = Nothing,
      _diPreviousRevision = Nothing,
      _diInstanceTerminationWaitTimeStarted = Nothing,
      _diDeploymentStatusMessages = Nothing,
      _diStartTime = Nothing,
      _diCompleteTime = Nothing,
      _diBlueGreenDeploymentConfiguration = Nothing,
      _diErrorInformation = Nothing,
      _diLoadBalancerInfo = Nothing,
      _diAdditionalDeploymentStatusInfo = Nothing,
      _diDeploymentOverview = Nothing,
      _diFileExistsBehavior = Nothing,
      _diApplicationName = Nothing,
      _diRollbackInfo = Nothing,
      _diExternalId = Nothing,
      _diTargetInstances = Nothing,
      _diRevision = Nothing,
      _diDescription = Nothing,
      _diDeploymentStyle = Nothing,
      _diCreateTime = Nothing,
      _diAutoRollbackConfiguration = Nothing,
      _diUpdateOutdatedInstancesOnly = Nothing,
      _diDeploymentGroupName = Nothing,
      _diIgnoreApplicationStopFailures = Nothing
    }

-- | The means by which the deployment was created:     * @user@ : A user created the deployment.     * @autoscaling@ : Amazon EC2 Auto Scaling created the deployment.     * @codeDeployRollback@ : A rollback process created the deployment.
diCreator :: Lens' DeploymentInfo (Maybe DeploymentCreator)
diCreator = lens _diCreator (\s a -> s {_diCreator = a})

-- | The current state of the deployment as a whole.
diStatus :: Lens' DeploymentInfo (Maybe DeploymentStatus)
diStatus = lens _diStatus (\s a -> s {_diStatus = a})

-- | The unique ID of a deployment.
diDeploymentId :: Lens' DeploymentInfo (Maybe Text)
diDeploymentId = lens _diDeploymentId (\s a -> s {_diDeploymentId = a})

-- | The deployment configuration name.
diDeploymentConfigName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentConfigName = lens _diDeploymentConfigName (\s a -> s {_diDeploymentConfigName = a})

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
diComputePlatform :: Lens' DeploymentInfo (Maybe ComputePlatform)
diComputePlatform = lens _diComputePlatform (\s a -> s {_diComputePlatform = a})

-- | Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
diPreviousRevision :: Lens' DeploymentInfo (Maybe RevisionLocation)
diPreviousRevision = lens _diPreviousRevision (\s a -> s {_diPreviousRevision = a})

-- | Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified. Otherwise, 'true' as soon as the termination wait period starts.
diInstanceTerminationWaitTimeStarted :: Lens' DeploymentInfo (Maybe Bool)
diInstanceTerminationWaitTimeStarted = lens _diInstanceTerminationWaitTimeStarted (\s a -> s {_diInstanceTerminationWaitTimeStarted = a})

-- | Messages that contain information about the status of a deployment.
diDeploymentStatusMessages :: Lens' DeploymentInfo [Text]
diDeploymentStatusMessages = lens _diDeploymentStatusMessages (\s a -> s {_diDeploymentStatusMessages = a}) . _Default . _Coerce

-- | A timestamp that indicates when the deployment was deployed to the deployment group. In some cases, the reported value of the start time might be later than the complete time. This is due to differences in the clock settings of backend servers that participate in the deployment process.
diStartTime :: Lens' DeploymentInfo (Maybe UTCTime)
diStartTime = lens _diStartTime (\s a -> s {_diStartTime = a}) . mapping _Time

-- | A timestamp that indicates when the deployment was complete.
diCompleteTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCompleteTime = lens _diCompleteTime (\s a -> s {_diCompleteTime = a}) . mapping _Time

-- | Information about blue/green deployment options for this deployment.
diBlueGreenDeploymentConfiguration :: Lens' DeploymentInfo (Maybe BlueGreenDeploymentConfiguration)
diBlueGreenDeploymentConfiguration = lens _diBlueGreenDeploymentConfiguration (\s a -> s {_diBlueGreenDeploymentConfiguration = a})

-- | Information about any error associated with this deployment.
diErrorInformation :: Lens' DeploymentInfo (Maybe ErrorInformation)
diErrorInformation = lens _diErrorInformation (\s a -> s {_diErrorInformation = a})

-- | Information about the load balancer used in the deployment.
diLoadBalancerInfo :: Lens' DeploymentInfo (Maybe LoadBalancerInfo)
diLoadBalancerInfo = lens _diLoadBalancerInfo (\s a -> s {_diLoadBalancerInfo = a})

-- | Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
diAdditionalDeploymentStatusInfo :: Lens' DeploymentInfo (Maybe Text)
diAdditionalDeploymentStatusInfo = lens _diAdditionalDeploymentStatusInfo (\s a -> s {_diAdditionalDeploymentStatusInfo = a})

-- | A summary of the deployment status of the instances in the deployment.
diDeploymentOverview :: Lens' DeploymentInfo (Maybe DeploymentOverview)
diDeploymentOverview = lens _diDeploymentOverview (\s a -> s {_diDeploymentOverview = a})

-- | Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.     * @DISALLOW@ : The deployment fails. This is also the default behavior if no option is specified.     * @OVERWRITE@ : The version of the file from the application revision currently being deployed replaces the version already on the instance.     * @RETAIN@ : The version of the file already on the instance is kept and used as part of the new deployment.
diFileExistsBehavior :: Lens' DeploymentInfo (Maybe FileExistsBehavior)
diFileExistsBehavior = lens _diFileExistsBehavior (\s a -> s {_diFileExistsBehavior = a})

-- | The application name.
diApplicationName :: Lens' DeploymentInfo (Maybe Text)
diApplicationName = lens _diApplicationName (\s a -> s {_diApplicationName = a})

-- | Information about a deployment rollback.
diRollbackInfo :: Lens' DeploymentInfo (Maybe RollbackInfo)
diRollbackInfo = lens _diRollbackInfo (\s a -> s {_diRollbackInfo = a})

-- | The unique ID for an external resource (for example, a CloudFormation stack ID) that is linked to this deployment.
diExternalId :: Lens' DeploymentInfo (Maybe Text)
diExternalId = lens _diExternalId (\s a -> s {_diExternalId = a})

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
diTargetInstances :: Lens' DeploymentInfo (Maybe TargetInstances)
diTargetInstances = lens _diTargetInstances (\s a -> s {_diTargetInstances = a})

-- | Information about the location of stored application artifacts and the service from which to retrieve them.
diRevision :: Lens' DeploymentInfo (Maybe RevisionLocation)
diRevision = lens _diRevision (\s a -> s {_diRevision = a})

-- | A comment about the deployment.
diDescription :: Lens' DeploymentInfo (Maybe Text)
diDescription = lens _diDescription (\s a -> s {_diDescription = a})

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
diDeploymentStyle :: Lens' DeploymentInfo (Maybe DeploymentStyle)
diDeploymentStyle = lens _diDeploymentStyle (\s a -> s {_diDeploymentStyle = a})

-- | A timestamp that indicates when the deployment was created.
diCreateTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCreateTime = lens _diCreateTime (\s a -> s {_diCreateTime = a}) . mapping _Time

-- | Information about the automatic rollback configuration associated with the deployment.
diAutoRollbackConfiguration :: Lens' DeploymentInfo (Maybe AutoRollbackConfiguration)
diAutoRollbackConfiguration = lens _diAutoRollbackConfiguration (\s a -> s {_diAutoRollbackConfiguration = a})

-- | Indicates whether only instances that are not running the latest application revision are to be deployed to.
diUpdateOutdatedInstancesOnly :: Lens' DeploymentInfo (Maybe Bool)
diUpdateOutdatedInstancesOnly = lens _diUpdateOutdatedInstancesOnly (\s a -> s {_diUpdateOutdatedInstancesOnly = a})

-- | The deployment group name.
diDeploymentGroupName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentGroupName = lens _diDeploymentGroupName (\s a -> s {_diDeploymentGroupName = a})

-- | If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ .  If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted.  During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail.  If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored.
diIgnoreApplicationStopFailures :: Lens' DeploymentInfo (Maybe Bool)
diIgnoreApplicationStopFailures = lens _diIgnoreApplicationStopFailures (\s a -> s {_diIgnoreApplicationStopFailures = a})

instance FromJSON DeploymentInfo where
  parseJSON =
    withObject
      "DeploymentInfo"
      ( \x ->
          DeploymentInfo'
            <$> (x .:? "creator")
            <*> (x .:? "status")
            <*> (x .:? "deploymentId")
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
            <*> (x .:? "externalId")
            <*> (x .:? "targetInstances")
            <*> (x .:? "revision")
            <*> (x .:? "description")
            <*> (x .:? "deploymentStyle")
            <*> (x .:? "createTime")
            <*> (x .:? "autoRollbackConfiguration")
            <*> (x .:? "updateOutdatedInstancesOnly")
            <*> (x .:? "deploymentGroupName")
            <*> (x .:? "ignoreApplicationStopFailures")
      )

instance Hashable DeploymentInfo

instance NFData DeploymentInfo
