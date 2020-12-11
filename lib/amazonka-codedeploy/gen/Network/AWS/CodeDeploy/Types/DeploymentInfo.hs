-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentInfo
  ( DeploymentInfo (..),

    -- * Smart constructor
    mkDeploymentInfo,

    -- * Lenses
    diCreator,
    diStatus,
    diDeploymentId,
    diDeploymentConfigName,
    diComputePlatform,
    diPreviousRevision,
    diInstanceTerminationWaitTimeStarted,
    diDeploymentStatusMessages,
    diStartTime,
    diCompleteTime,
    diBlueGreenDeploymentConfiguration,
    diErrorInformation,
    diLoadBalancerInfo,
    diAdditionalDeploymentStatusInfo,
    diDeploymentOverview,
    diFileExistsBehavior,
    diApplicationName,
    diRollbackInfo,
    diExternalId,
    diTargetInstances,
    diRevision,
    diDescription,
    diDeploymentStyle,
    diCreateTime,
    diAutoRollbackConfiguration,
    diUpdateOutdatedInstancesOnly,
    diDeploymentGroupName,
    diIgnoreApplicationStopFailures,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment.
--
-- /See:/ 'mkDeploymentInfo' smart constructor.
data DeploymentInfo = DeploymentInfo'
  { creator ::
      Lude.Maybe DeploymentCreator,
    status :: Lude.Maybe DeploymentStatus,
    deploymentId :: Lude.Maybe Lude.Text,
    deploymentConfigName :: Lude.Maybe Lude.Text,
    computePlatform :: Lude.Maybe ComputePlatform,
    previousRevision :: Lude.Maybe RevisionLocation,
    instanceTerminationWaitTimeStarted :: Lude.Maybe Lude.Bool,
    deploymentStatusMessages :: Lude.Maybe [Lude.Text],
    startTime :: Lude.Maybe Lude.Timestamp,
    completeTime :: Lude.Maybe Lude.Timestamp,
    blueGreenDeploymentConfiguration ::
      Lude.Maybe BlueGreenDeploymentConfiguration,
    errorInformation :: Lude.Maybe ErrorInformation,
    loadBalancerInfo :: Lude.Maybe LoadBalancerInfo,
    additionalDeploymentStatusInfo :: Lude.Maybe Lude.Text,
    deploymentOverview :: Lude.Maybe DeploymentOverview,
    fileExistsBehavior :: Lude.Maybe FileExistsBehavior,
    applicationName :: Lude.Maybe Lude.Text,
    rollbackInfo :: Lude.Maybe RollbackInfo,
    externalId :: Lude.Maybe Lude.Text,
    targetInstances :: Lude.Maybe TargetInstances,
    revision :: Lude.Maybe RevisionLocation,
    description :: Lude.Maybe Lude.Text,
    deploymentStyle :: Lude.Maybe DeploymentStyle,
    createTime :: Lude.Maybe Lude.Timestamp,
    autoRollbackConfiguration ::
      Lude.Maybe AutoRollbackConfiguration,
    updateOutdatedInstancesOnly :: Lude.Maybe Lude.Bool,
    deploymentGroupName :: Lude.Maybe Lude.Text,
    ignoreApplicationStopFailures :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentInfo' with the minimum fields required to make a request.
--
-- * 'additionalDeploymentStatusInfo' - Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
-- * 'applicationName' - The application name.
-- * 'autoRollbackConfiguration' - Information about the automatic rollback configuration associated with the deployment.
-- * 'blueGreenDeploymentConfiguration' - Information about blue/green deployment options for this deployment.
-- * 'completeTime' - A timestamp that indicates when the deployment was complete.
-- * 'computePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
-- * 'createTime' - A timestamp that indicates when the deployment was created.
-- * 'creator' - The means by which the deployment was created:
--
--
--     * @user@ : A user created the deployment.
--
--
--     * @autoscaling@ : Amazon EC2 Auto Scaling created the deployment.
--
--
--     * @codeDeployRollback@ : A rollback process created the deployment.
--
--
-- * 'deploymentConfigName' - The deployment configuration name.
-- * 'deploymentGroupName' - The deployment group name.
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'deploymentOverview' - A summary of the deployment status of the instances in the deployment.
-- * 'deploymentStatusMessages' - Messages that contain information about the status of a deployment.
-- * 'deploymentStyle' - Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
-- * 'description' - A comment about the deployment.
-- * 'errorInformation' - Information about any error associated with this deployment.
-- * 'externalId' - The unique ID for an external resource (for example, a CloudFormation stack ID) that is linked to this deployment.
-- * 'fileExistsBehavior' - Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.
--
--
--     * @DISALLOW@ : The deployment fails. This is also the default behavior if no option is specified.
--
--
--     * @OVERWRITE@ : The version of the file from the application revision currently being deployed replaces the version already on the instance.
--
--
--     * @RETAIN@ : The version of the file already on the instance is kept and used as part of the new deployment.
--
--
-- * 'ignoreApplicationStopFailures' - If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ .
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted.
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail.
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored.
-- * 'instanceTerminationWaitTimeStarted' - Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified. Otherwise, 'true' as soon as the termination wait period starts.
-- * 'loadBalancerInfo' - Information about the load balancer used in the deployment.
-- * 'previousRevision' - Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
-- * 'revision' - Information about the location of stored application artifacts and the service from which to retrieve them.
-- * 'rollbackInfo' - Information about a deployment rollback.
-- * 'startTime' - A timestamp that indicates when the deployment was deployed to the deployment group.
--
-- In some cases, the reported value of the start time might be later than the complete time. This is due to differences in the clock settings of backend servers that participate in the deployment process.
-- * 'status' - The current state of the deployment as a whole.
-- * 'targetInstances' - Information about the instances that belong to the replacement environment in a blue/green deployment.
-- * 'updateOutdatedInstancesOnly' - Indicates whether only instances that are not running the latest application revision are to be deployed to.
mkDeploymentInfo ::
  DeploymentInfo
mkDeploymentInfo =
  DeploymentInfo'
    { creator = Lude.Nothing,
      status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      deploymentConfigName = Lude.Nothing,
      computePlatform = Lude.Nothing,
      previousRevision = Lude.Nothing,
      instanceTerminationWaitTimeStarted = Lude.Nothing,
      deploymentStatusMessages = Lude.Nothing,
      startTime = Lude.Nothing,
      completeTime = Lude.Nothing,
      blueGreenDeploymentConfiguration = Lude.Nothing,
      errorInformation = Lude.Nothing,
      loadBalancerInfo = Lude.Nothing,
      additionalDeploymentStatusInfo = Lude.Nothing,
      deploymentOverview = Lude.Nothing,
      fileExistsBehavior = Lude.Nothing,
      applicationName = Lude.Nothing,
      rollbackInfo = Lude.Nothing,
      externalId = Lude.Nothing,
      targetInstances = Lude.Nothing,
      revision = Lude.Nothing,
      description = Lude.Nothing,
      deploymentStyle = Lude.Nothing,
      createTime = Lude.Nothing,
      autoRollbackConfiguration = Lude.Nothing,
      updateOutdatedInstancesOnly = Lude.Nothing,
      deploymentGroupName = Lude.Nothing,
      ignoreApplicationStopFailures = Lude.Nothing
    }

-- | The means by which the deployment was created:
--
--
--     * @user@ : A user created the deployment.
--
--
--     * @autoscaling@ : Amazon EC2 Auto Scaling created the deployment.
--
--
--     * @codeDeployRollback@ : A rollback process created the deployment.
--
--
--
-- /Note:/ Consider using 'creator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreator :: Lens.Lens' DeploymentInfo (Lude.Maybe DeploymentCreator)
diCreator = Lens.lens (creator :: DeploymentInfo -> Lude.Maybe DeploymentCreator) (\s a -> s {creator = a} :: DeploymentInfo)
{-# DEPRECATED diCreator "Use generic-lens or generic-optics with 'creator' instead." #-}

-- | The current state of the deployment as a whole.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatus :: Lens.Lens' DeploymentInfo (Lude.Maybe DeploymentStatus)
diStatus = Lens.lens (status :: DeploymentInfo -> Lude.Maybe DeploymentStatus) (\s a -> s {status = a} :: DeploymentInfo)
{-# DEPRECATED diStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentId :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diDeploymentId = Lens.lens (deploymentId :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: DeploymentInfo)
{-# DEPRECATED diDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The deployment configuration name.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentConfigName :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diDeploymentConfigName = Lens.lens (deploymentConfigName :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigName = a} :: DeploymentInfo)
{-# DEPRECATED diDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diComputePlatform :: Lens.Lens' DeploymentInfo (Lude.Maybe ComputePlatform)
diComputePlatform = Lens.lens (computePlatform :: DeploymentInfo -> Lude.Maybe ComputePlatform) (\s a -> s {computePlatform = a} :: DeploymentInfo)
{-# DEPRECATED diComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
--
-- /Note:/ Consider using 'previousRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPreviousRevision :: Lens.Lens' DeploymentInfo (Lude.Maybe RevisionLocation)
diPreviousRevision = Lens.lens (previousRevision :: DeploymentInfo -> Lude.Maybe RevisionLocation) (\s a -> s {previousRevision = a} :: DeploymentInfo)
{-# DEPRECATED diPreviousRevision "Use generic-lens or generic-optics with 'previousRevision' instead." #-}

-- | Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified. Otherwise, 'true' as soon as the termination wait period starts.
--
-- /Note:/ Consider using 'instanceTerminationWaitTimeStarted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceTerminationWaitTimeStarted :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Bool)
diInstanceTerminationWaitTimeStarted = Lens.lens (instanceTerminationWaitTimeStarted :: DeploymentInfo -> Lude.Maybe Lude.Bool) (\s a -> s {instanceTerminationWaitTimeStarted = a} :: DeploymentInfo)
{-# DEPRECATED diInstanceTerminationWaitTimeStarted "Use generic-lens or generic-optics with 'instanceTerminationWaitTimeStarted' instead." #-}

-- | Messages that contain information about the status of a deployment.
--
-- /Note:/ Consider using 'deploymentStatusMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentStatusMessages :: Lens.Lens' DeploymentInfo (Lude.Maybe [Lude.Text])
diDeploymentStatusMessages = Lens.lens (deploymentStatusMessages :: DeploymentInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {deploymentStatusMessages = a} :: DeploymentInfo)
{-# DEPRECATED diDeploymentStatusMessages "Use generic-lens or generic-optics with 'deploymentStatusMessages' instead." #-}

-- | A timestamp that indicates when the deployment was deployed to the deployment group.
--
-- In some cases, the reported value of the start time might be later than the complete time. This is due to differences in the clock settings of backend servers that participate in the deployment process.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStartTime :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Timestamp)
diStartTime = Lens.lens (startTime :: DeploymentInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DeploymentInfo)
{-# DEPRECATED diStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A timestamp that indicates when the deployment was complete.
--
-- /Note:/ Consider using 'completeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCompleteTime :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Timestamp)
diCompleteTime = Lens.lens (completeTime :: DeploymentInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {completeTime = a} :: DeploymentInfo)
{-# DEPRECATED diCompleteTime "Use generic-lens or generic-optics with 'completeTime' instead." #-}

-- | Information about blue/green deployment options for this deployment.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diBlueGreenDeploymentConfiguration :: Lens.Lens' DeploymentInfo (Lude.Maybe BlueGreenDeploymentConfiguration)
diBlueGreenDeploymentConfiguration = Lens.lens (blueGreenDeploymentConfiguration :: DeploymentInfo -> Lude.Maybe BlueGreenDeploymentConfiguration) (\s a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentInfo)
{-# DEPRECATED diBlueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead." #-}

-- | Information about any error associated with this deployment.
--
-- /Note:/ Consider using 'errorInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diErrorInformation :: Lens.Lens' DeploymentInfo (Lude.Maybe ErrorInformation)
diErrorInformation = Lens.lens (errorInformation :: DeploymentInfo -> Lude.Maybe ErrorInformation) (\s a -> s {errorInformation = a} :: DeploymentInfo)
{-# DEPRECATED diErrorInformation "Use generic-lens or generic-optics with 'errorInformation' instead." #-}

-- | Information about the load balancer used in the deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLoadBalancerInfo :: Lens.Lens' DeploymentInfo (Lude.Maybe LoadBalancerInfo)
diLoadBalancerInfo = Lens.lens (loadBalancerInfo :: DeploymentInfo -> Lude.Maybe LoadBalancerInfo) (\s a -> s {loadBalancerInfo = a} :: DeploymentInfo)
{-# DEPRECATED diLoadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead." #-}

-- | Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
--
-- /Note:/ Consider using 'additionalDeploymentStatusInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAdditionalDeploymentStatusInfo :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diAdditionalDeploymentStatusInfo = Lens.lens (additionalDeploymentStatusInfo :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {additionalDeploymentStatusInfo = a} :: DeploymentInfo)
{-# DEPRECATED diAdditionalDeploymentStatusInfo "Use generic-lens or generic-optics with 'additionalDeploymentStatusInfo' instead." #-}

-- | A summary of the deployment status of the instances in the deployment.
--
-- /Note:/ Consider using 'deploymentOverview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentOverview :: Lens.Lens' DeploymentInfo (Lude.Maybe DeploymentOverview)
diDeploymentOverview = Lens.lens (deploymentOverview :: DeploymentInfo -> Lude.Maybe DeploymentOverview) (\s a -> s {deploymentOverview = a} :: DeploymentInfo)
{-# DEPRECATED diDeploymentOverview "Use generic-lens or generic-optics with 'deploymentOverview' instead." #-}

-- | Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.
--
--
--     * @DISALLOW@ : The deployment fails. This is also the default behavior if no option is specified.
--
--
--     * @OVERWRITE@ : The version of the file from the application revision currently being deployed replaces the version already on the instance.
--
--
--     * @RETAIN@ : The version of the file already on the instance is kept and used as part of the new deployment.
--
--
--
-- /Note:/ Consider using 'fileExistsBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diFileExistsBehavior :: Lens.Lens' DeploymentInfo (Lude.Maybe FileExistsBehavior)
diFileExistsBehavior = Lens.lens (fileExistsBehavior :: DeploymentInfo -> Lude.Maybe FileExistsBehavior) (\s a -> s {fileExistsBehavior = a} :: DeploymentInfo)
{-# DEPRECATED diFileExistsBehavior "Use generic-lens or generic-optics with 'fileExistsBehavior' instead." #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diApplicationName :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diApplicationName = Lens.lens (applicationName :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DeploymentInfo)
{-# DEPRECATED diApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information about a deployment rollback.
--
-- /Note:/ Consider using 'rollbackInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRollbackInfo :: Lens.Lens' DeploymentInfo (Lude.Maybe RollbackInfo)
diRollbackInfo = Lens.lens (rollbackInfo :: DeploymentInfo -> Lude.Maybe RollbackInfo) (\s a -> s {rollbackInfo = a} :: DeploymentInfo)
{-# DEPRECATED diRollbackInfo "Use generic-lens or generic-optics with 'rollbackInfo' instead." #-}

-- | The unique ID for an external resource (for example, a CloudFormation stack ID) that is linked to this deployment.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diExternalId :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diExternalId = Lens.lens (externalId :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: DeploymentInfo)
{-# DEPRECATED diExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- /Note:/ Consider using 'targetInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTargetInstances :: Lens.Lens' DeploymentInfo (Lude.Maybe TargetInstances)
diTargetInstances = Lens.lens (targetInstances :: DeploymentInfo -> Lude.Maybe TargetInstances) (\s a -> s {targetInstances = a} :: DeploymentInfo)
{-# DEPRECATED diTargetInstances "Use generic-lens or generic-optics with 'targetInstances' instead." #-}

-- | Information about the location of stored application artifacts and the service from which to retrieve them.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRevision :: Lens.Lens' DeploymentInfo (Lude.Maybe RevisionLocation)
diRevision = Lens.lens (revision :: DeploymentInfo -> Lude.Maybe RevisionLocation) (\s a -> s {revision = a} :: DeploymentInfo)
{-# DEPRECATED diRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | A comment about the deployment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diDescription = Lens.lens (description :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DeploymentInfo)
{-# DEPRECATED diDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentStyle :: Lens.Lens' DeploymentInfo (Lude.Maybe DeploymentStyle)
diDeploymentStyle = Lens.lens (deploymentStyle :: DeploymentInfo -> Lude.Maybe DeploymentStyle) (\s a -> s {deploymentStyle = a} :: DeploymentInfo)
{-# DEPRECATED diDeploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead." #-}

-- | A timestamp that indicates when the deployment was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreateTime :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Timestamp)
diCreateTime = Lens.lens (createTime :: DeploymentInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: DeploymentInfo)
{-# DEPRECATED diCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Information about the automatic rollback configuration associated with the deployment.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAutoRollbackConfiguration :: Lens.Lens' DeploymentInfo (Lude.Maybe AutoRollbackConfiguration)
diAutoRollbackConfiguration = Lens.lens (autoRollbackConfiguration :: DeploymentInfo -> Lude.Maybe AutoRollbackConfiguration) (\s a -> s {autoRollbackConfiguration = a} :: DeploymentInfo)
{-# DEPRECATED diAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

-- | Indicates whether only instances that are not running the latest application revision are to be deployed to.
--
-- /Note:/ Consider using 'updateOutdatedInstancesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diUpdateOutdatedInstancesOnly :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Bool)
diUpdateOutdatedInstancesOnly = Lens.lens (updateOutdatedInstancesOnly :: DeploymentInfo -> Lude.Maybe Lude.Bool) (\s a -> s {updateOutdatedInstancesOnly = a} :: DeploymentInfo)
{-# DEPRECATED diUpdateOutdatedInstancesOnly "Use generic-lens or generic-optics with 'updateOutdatedInstancesOnly' instead." #-}

-- | The deployment group name.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentGroupName :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Text)
diDeploymentGroupName = Lens.lens (deploymentGroupName :: DeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentGroupName = a} :: DeploymentInfo)
{-# DEPRECATED diDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

-- | If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ .
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted.
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail.
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored.
--
-- /Note:/ Consider using 'ignoreApplicationStopFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIgnoreApplicationStopFailures :: Lens.Lens' DeploymentInfo (Lude.Maybe Lude.Bool)
diIgnoreApplicationStopFailures = Lens.lens (ignoreApplicationStopFailures :: DeploymentInfo -> Lude.Maybe Lude.Bool) (\s a -> s {ignoreApplicationStopFailures = a} :: DeploymentInfo)
{-# DEPRECATED diIgnoreApplicationStopFailures "Use generic-lens or generic-optics with 'ignoreApplicationStopFailures' instead." #-}

instance Lude.FromJSON DeploymentInfo where
  parseJSON =
    Lude.withObject
      "DeploymentInfo"
      ( \x ->
          DeploymentInfo'
            Lude.<$> (x Lude..:? "creator")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "deploymentConfigName")
            Lude.<*> (x Lude..:? "computePlatform")
            Lude.<*> (x Lude..:? "previousRevision")
            Lude.<*> (x Lude..:? "instanceTerminationWaitTimeStarted")
            Lude.<*> (x Lude..:? "deploymentStatusMessages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "completeTime")
            Lude.<*> (x Lude..:? "blueGreenDeploymentConfiguration")
            Lude.<*> (x Lude..:? "errorInformation")
            Lude.<*> (x Lude..:? "loadBalancerInfo")
            Lude.<*> (x Lude..:? "additionalDeploymentStatusInfo")
            Lude.<*> (x Lude..:? "deploymentOverview")
            Lude.<*> (x Lude..:? "fileExistsBehavior")
            Lude.<*> (x Lude..:? "applicationName")
            Lude.<*> (x Lude..:? "rollbackInfo")
            Lude.<*> (x Lude..:? "externalId")
            Lude.<*> (x Lude..:? "targetInstances")
            Lude.<*> (x Lude..:? "revision")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "deploymentStyle")
            Lude.<*> (x Lude..:? "createTime")
            Lude.<*> (x Lude..:? "autoRollbackConfiguration")
            Lude.<*> (x Lude..:? "updateOutdatedInstancesOnly")
            Lude.<*> (x Lude..:? "deploymentGroupName")
            Lude.<*> (x Lude..:? "ignoreApplicationStopFailures")
      )
