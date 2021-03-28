{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentInfo
  ( DeploymentInfo (..)
  -- * Smart constructor
  , mkDeploymentInfo
  -- * Lenses
  , diAdditionalDeploymentStatusInfo
  , diApplicationName
  , diAutoRollbackConfiguration
  , diBlueGreenDeploymentConfiguration
  , diCompleteTime
  , diComputePlatform
  , diCreateTime
  , diCreator
  , diDeploymentConfigName
  , diDeploymentGroupName
  , diDeploymentId
  , diDeploymentOverview
  , diDeploymentStatusMessages
  , diDeploymentStyle
  , diDescription
  , diErrorInformation
  , diExternalId
  , diFileExistsBehavior
  , diIgnoreApplicationStopFailures
  , diInstanceTerminationWaitTimeStarted
  , diLoadBalancerInfo
  , diPreviousRevision
  , diRevision
  , diRollbackInfo
  , diStartTime
  , diStatus
  , diTargetInstances
  , diUpdateOutdatedInstancesOnly
  ) where

import qualified Network.AWS.CodeDeploy.Types.AdditionalDeploymentStatusInfo as Types
import qualified Network.AWS.CodeDeploy.Types.ApplicationName as Types
import qualified Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration as Types
import qualified Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration as Types
import qualified Network.AWS.CodeDeploy.Types.ComputePlatform as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentConfigName as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentCreator as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentGroupName as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentId as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentOverview as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentStatus as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentStyle as Types
import qualified Network.AWS.CodeDeploy.Types.Description as Types
import qualified Network.AWS.CodeDeploy.Types.ErrorInformation as Types
import qualified Network.AWS.CodeDeploy.Types.ErrorMessage as Types
import qualified Network.AWS.CodeDeploy.Types.ExternalId as Types
import qualified Network.AWS.CodeDeploy.Types.FileExistsBehavior as Types
import qualified Network.AWS.CodeDeploy.Types.LoadBalancerInfo as Types
import qualified Network.AWS.CodeDeploy.Types.RevisionLocation as Types
import qualified Network.AWS.CodeDeploy.Types.RollbackInfo as Types
import qualified Network.AWS.CodeDeploy.Types.TargetInstances as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment.
--
-- /See:/ 'mkDeploymentInfo' smart constructor.
data DeploymentInfo = DeploymentInfo'
  { additionalDeploymentStatusInfo :: Core.Maybe Types.AdditionalDeploymentStatusInfo
    -- ^ Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
  , applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The application name.
  , autoRollbackConfiguration :: Core.Maybe Types.AutoRollbackConfiguration
    -- ^ Information about the automatic rollback configuration associated with the deployment.
  , blueGreenDeploymentConfiguration :: Core.Maybe Types.BlueGreenDeploymentConfiguration
    -- ^ Information about blue/green deployment options for this deployment.
  , completeTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that indicates when the deployment was complete.
  , computePlatform :: Core.Maybe Types.ComputePlatform
    -- ^ The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
  , createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that indicates when the deployment was created.
  , creator :: Core.Maybe Types.DeploymentCreator
    -- ^ The means by which the deployment was created:
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
  , deploymentConfigName :: Core.Maybe Types.DeploymentConfigName
    -- ^ The deployment configuration name. 
  , deploymentGroupName :: Core.Maybe Types.DeploymentGroupName
    -- ^ The deployment group name. 
  , deploymentId :: Core.Maybe Types.DeploymentId
    -- ^ The unique ID of a deployment. 
  , deploymentOverview :: Core.Maybe Types.DeploymentOverview
    -- ^ A summary of the deployment status of the instances in the deployment.
  , deploymentStatusMessages :: Core.Maybe [Types.ErrorMessage]
    -- ^ Messages that contain information about the status of a deployment.
  , deploymentStyle :: Core.Maybe Types.DeploymentStyle
    -- ^ Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
  , description :: Core.Maybe Types.Description
    -- ^ A comment about the deployment.
  , errorInformation :: Core.Maybe Types.ErrorInformation
    -- ^ Information about any error associated with this deployment.
  , externalId :: Core.Maybe Types.ExternalId
    -- ^ The unique ID for an external resource (for example, a CloudFormation stack ID) that is linked to this deployment.
  , fileExistsBehavior :: Core.Maybe Types.FileExistsBehavior
    -- ^ Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.
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
  , ignoreApplicationStopFailures :: Core.Maybe Core.Bool
    -- ^ If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ . 
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted. 
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail. 
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored. 
  , instanceTerminationWaitTimeStarted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified. Otherwise, 'true' as soon as the termination wait period starts.
  , loadBalancerInfo :: Core.Maybe Types.LoadBalancerInfo
    -- ^ Information about the load balancer used in the deployment.
  , previousRevision :: Core.Maybe Types.RevisionLocation
    -- ^ Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
  , revision :: Core.Maybe Types.RevisionLocation
    -- ^ Information about the location of stored application artifacts and the service from which to retrieve them.
  , rollbackInfo :: Core.Maybe Types.RollbackInfo
    -- ^ Information about a deployment rollback.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that indicates when the deployment was deployed to the deployment group.
--
-- In some cases, the reported value of the start time might be later than the complete time. This is due to differences in the clock settings of backend servers that participate in the deployment process.
  , status :: Core.Maybe Types.DeploymentStatus
    -- ^ The current state of the deployment as a whole.
  , targetInstances :: Core.Maybe Types.TargetInstances
    -- ^ Information about the instances that belong to the replacement environment in a blue/green deployment.
  , updateOutdatedInstancesOnly :: Core.Maybe Core.Bool
    -- ^ Indicates whether only instances that are not running the latest application revision are to be deployed to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeploymentInfo' value with any optional fields omitted.
mkDeploymentInfo
    :: DeploymentInfo
mkDeploymentInfo
  = DeploymentInfo'{additionalDeploymentStatusInfo = Core.Nothing,
                    applicationName = Core.Nothing,
                    autoRollbackConfiguration = Core.Nothing,
                    blueGreenDeploymentConfiguration = Core.Nothing,
                    completeTime = Core.Nothing, computePlatform = Core.Nothing,
                    createTime = Core.Nothing, creator = Core.Nothing,
                    deploymentConfigName = Core.Nothing,
                    deploymentGroupName = Core.Nothing, deploymentId = Core.Nothing,
                    deploymentOverview = Core.Nothing,
                    deploymentStatusMessages = Core.Nothing,
                    deploymentStyle = Core.Nothing, description = Core.Nothing,
                    errorInformation = Core.Nothing, externalId = Core.Nothing,
                    fileExistsBehavior = Core.Nothing,
                    ignoreApplicationStopFailures = Core.Nothing,
                    instanceTerminationWaitTimeStarted = Core.Nothing,
                    loadBalancerInfo = Core.Nothing, previousRevision = Core.Nothing,
                    revision = Core.Nothing, rollbackInfo = Core.Nothing,
                    startTime = Core.Nothing, status = Core.Nothing,
                    targetInstances = Core.Nothing,
                    updateOutdatedInstancesOnly = Core.Nothing}

-- | Provides information about the results of a deployment, such as whether instances in the original environment in a blue/green deployment were not terminated.
--
-- /Note:/ Consider using 'additionalDeploymentStatusInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAdditionalDeploymentStatusInfo :: Lens.Lens' DeploymentInfo (Core.Maybe Types.AdditionalDeploymentStatusInfo)
diAdditionalDeploymentStatusInfo = Lens.field @"additionalDeploymentStatusInfo"
{-# INLINEABLE diAdditionalDeploymentStatusInfo #-}
{-# DEPRECATED additionalDeploymentStatusInfo "Use generic-lens or generic-optics with 'additionalDeploymentStatusInfo' instead"  #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diApplicationName :: Lens.Lens' DeploymentInfo (Core.Maybe Types.ApplicationName)
diApplicationName = Lens.field @"applicationName"
{-# INLINEABLE diApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Information about the automatic rollback configuration associated with the deployment.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAutoRollbackConfiguration :: Lens.Lens' DeploymentInfo (Core.Maybe Types.AutoRollbackConfiguration)
diAutoRollbackConfiguration = Lens.field @"autoRollbackConfiguration"
{-# INLINEABLE diAutoRollbackConfiguration #-}
{-# DEPRECATED autoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead"  #-}

-- | Information about blue/green deployment options for this deployment.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diBlueGreenDeploymentConfiguration :: Lens.Lens' DeploymentInfo (Core.Maybe Types.BlueGreenDeploymentConfiguration)
diBlueGreenDeploymentConfiguration = Lens.field @"blueGreenDeploymentConfiguration"
{-# INLINEABLE diBlueGreenDeploymentConfiguration #-}
{-# DEPRECATED blueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead"  #-}

-- | A timestamp that indicates when the deployment was complete.
--
-- /Note:/ Consider using 'completeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCompleteTime :: Lens.Lens' DeploymentInfo (Core.Maybe Core.NominalDiffTime)
diCompleteTime = Lens.field @"completeTime"
{-# INLINEABLE diCompleteTime #-}
{-# DEPRECATED completeTime "Use generic-lens or generic-optics with 'completeTime' instead"  #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diComputePlatform :: Lens.Lens' DeploymentInfo (Core.Maybe Types.ComputePlatform)
diComputePlatform = Lens.field @"computePlatform"
{-# INLINEABLE diComputePlatform #-}
{-# DEPRECATED computePlatform "Use generic-lens or generic-optics with 'computePlatform' instead"  #-}

-- | A timestamp that indicates when the deployment was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreateTime :: Lens.Lens' DeploymentInfo (Core.Maybe Core.NominalDiffTime)
diCreateTime = Lens.field @"createTime"
{-# INLINEABLE diCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

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
diCreator :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentCreator)
diCreator = Lens.field @"creator"
{-# INLINEABLE diCreator #-}
{-# DEPRECATED creator "Use generic-lens or generic-optics with 'creator' instead"  #-}

-- | The deployment configuration name. 
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentConfigName :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentConfigName)
diDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# INLINEABLE diDeploymentConfigName #-}
{-# DEPRECATED deploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead"  #-}

-- | The deployment group name. 
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentGroupName :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentGroupName)
diDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# INLINEABLE diDeploymentGroupName #-}
{-# DEPRECATED deploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead"  #-}

-- | The unique ID of a deployment. 
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentId :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentId)
diDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE diDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | A summary of the deployment status of the instances in the deployment.
--
-- /Note:/ Consider using 'deploymentOverview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentOverview :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentOverview)
diDeploymentOverview = Lens.field @"deploymentOverview"
{-# INLINEABLE diDeploymentOverview #-}
{-# DEPRECATED deploymentOverview "Use generic-lens or generic-optics with 'deploymentOverview' instead"  #-}

-- | Messages that contain information about the status of a deployment.
--
-- /Note:/ Consider using 'deploymentStatusMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentStatusMessages :: Lens.Lens' DeploymentInfo (Core.Maybe [Types.ErrorMessage])
diDeploymentStatusMessages = Lens.field @"deploymentStatusMessages"
{-# INLINEABLE diDeploymentStatusMessages #-}
{-# DEPRECATED deploymentStatusMessages "Use generic-lens or generic-optics with 'deploymentStatusMessages' instead"  #-}

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeploymentStyle :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentStyle)
diDeploymentStyle = Lens.field @"deploymentStyle"
{-# INLINEABLE diDeploymentStyle #-}
{-# DEPRECATED deploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead"  #-}

-- | A comment about the deployment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DeploymentInfo (Core.Maybe Types.Description)
diDescription = Lens.field @"description"
{-# INLINEABLE diDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about any error associated with this deployment.
--
-- /Note:/ Consider using 'errorInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diErrorInformation :: Lens.Lens' DeploymentInfo (Core.Maybe Types.ErrorInformation)
diErrorInformation = Lens.field @"errorInformation"
{-# INLINEABLE diErrorInformation #-}
{-# DEPRECATED errorInformation "Use generic-lens or generic-optics with 'errorInformation' instead"  #-}

-- | The unique ID for an external resource (for example, a CloudFormation stack ID) that is linked to this deployment.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diExternalId :: Lens.Lens' DeploymentInfo (Core.Maybe Types.ExternalId)
diExternalId = Lens.field @"externalId"
{-# INLINEABLE diExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

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
diFileExistsBehavior :: Lens.Lens' DeploymentInfo (Core.Maybe Types.FileExistsBehavior)
diFileExistsBehavior = Lens.field @"fileExistsBehavior"
{-# INLINEABLE diFileExistsBehavior #-}
{-# DEPRECATED fileExistsBehavior "Use generic-lens or generic-optics with 'fileExistsBehavior' instead"  #-}

-- | If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ . 
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted. 
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail. 
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored. 
--
-- /Note:/ Consider using 'ignoreApplicationStopFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIgnoreApplicationStopFailures :: Lens.Lens' DeploymentInfo (Core.Maybe Core.Bool)
diIgnoreApplicationStopFailures = Lens.field @"ignoreApplicationStopFailures"
{-# INLINEABLE diIgnoreApplicationStopFailures #-}
{-# DEPRECATED ignoreApplicationStopFailures "Use generic-lens or generic-optics with 'ignoreApplicationStopFailures' instead"  #-}

-- | Indicates whether the wait period set for the termination of instances in the original environment has started. Status is 'false' if the KEEP_ALIVE option is specified. Otherwise, 'true' as soon as the termination wait period starts.
--
-- /Note:/ Consider using 'instanceTerminationWaitTimeStarted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceTerminationWaitTimeStarted :: Lens.Lens' DeploymentInfo (Core.Maybe Core.Bool)
diInstanceTerminationWaitTimeStarted = Lens.field @"instanceTerminationWaitTimeStarted"
{-# INLINEABLE diInstanceTerminationWaitTimeStarted #-}
{-# DEPRECATED instanceTerminationWaitTimeStarted "Use generic-lens or generic-optics with 'instanceTerminationWaitTimeStarted' instead"  #-}

-- | Information about the load balancer used in the deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLoadBalancerInfo :: Lens.Lens' DeploymentInfo (Core.Maybe Types.LoadBalancerInfo)
diLoadBalancerInfo = Lens.field @"loadBalancerInfo"
{-# INLINEABLE diLoadBalancerInfo #-}
{-# DEPRECATED loadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead"  #-}

-- | Information about the application revision that was deployed to the deployment group before the most recent successful deployment.
--
-- /Note:/ Consider using 'previousRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPreviousRevision :: Lens.Lens' DeploymentInfo (Core.Maybe Types.RevisionLocation)
diPreviousRevision = Lens.field @"previousRevision"
{-# INLINEABLE diPreviousRevision #-}
{-# DEPRECATED previousRevision "Use generic-lens or generic-optics with 'previousRevision' instead"  #-}

-- | Information about the location of stored application artifacts and the service from which to retrieve them.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRevision :: Lens.Lens' DeploymentInfo (Core.Maybe Types.RevisionLocation)
diRevision = Lens.field @"revision"
{-# INLINEABLE diRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | Information about a deployment rollback.
--
-- /Note:/ Consider using 'rollbackInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRollbackInfo :: Lens.Lens' DeploymentInfo (Core.Maybe Types.RollbackInfo)
diRollbackInfo = Lens.field @"rollbackInfo"
{-# INLINEABLE diRollbackInfo #-}
{-# DEPRECATED rollbackInfo "Use generic-lens or generic-optics with 'rollbackInfo' instead"  #-}

-- | A timestamp that indicates when the deployment was deployed to the deployment group.
--
-- In some cases, the reported value of the start time might be later than the complete time. This is due to differences in the clock settings of backend servers that participate in the deployment process.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStartTime :: Lens.Lens' DeploymentInfo (Core.Maybe Core.NominalDiffTime)
diStartTime = Lens.field @"startTime"
{-# INLINEABLE diStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The current state of the deployment as a whole.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatus :: Lens.Lens' DeploymentInfo (Core.Maybe Types.DeploymentStatus)
diStatus = Lens.field @"status"
{-# INLINEABLE diStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- /Note:/ Consider using 'targetInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTargetInstances :: Lens.Lens' DeploymentInfo (Core.Maybe Types.TargetInstances)
diTargetInstances = Lens.field @"targetInstances"
{-# INLINEABLE diTargetInstances #-}
{-# DEPRECATED targetInstances "Use generic-lens or generic-optics with 'targetInstances' instead"  #-}

-- | Indicates whether only instances that are not running the latest application revision are to be deployed to.
--
-- /Note:/ Consider using 'updateOutdatedInstancesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diUpdateOutdatedInstancesOnly :: Lens.Lens' DeploymentInfo (Core.Maybe Core.Bool)
diUpdateOutdatedInstancesOnly = Lens.field @"updateOutdatedInstancesOnly"
{-# INLINEABLE diUpdateOutdatedInstancesOnly #-}
{-# DEPRECATED updateOutdatedInstancesOnly "Use generic-lens or generic-optics with 'updateOutdatedInstancesOnly' instead"  #-}

instance Core.FromJSON DeploymentInfo where
        parseJSON
          = Core.withObject "DeploymentInfo" Core.$
              \ x ->
                DeploymentInfo' Core.<$>
                  (x Core..:? "additionalDeploymentStatusInfo") Core.<*>
                    x Core..:? "applicationName"
                    Core.<*> x Core..:? "autoRollbackConfiguration"
                    Core.<*> x Core..:? "blueGreenDeploymentConfiguration"
                    Core.<*> x Core..:? "completeTime"
                    Core.<*> x Core..:? "computePlatform"
                    Core.<*> x Core..:? "createTime"
                    Core.<*> x Core..:? "creator"
                    Core.<*> x Core..:? "deploymentConfigName"
                    Core.<*> x Core..:? "deploymentGroupName"
                    Core.<*> x Core..:? "deploymentId"
                    Core.<*> x Core..:? "deploymentOverview"
                    Core.<*> x Core..:? "deploymentStatusMessages"
                    Core.<*> x Core..:? "deploymentStyle"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "errorInformation"
                    Core.<*> x Core..:? "externalId"
                    Core.<*> x Core..:? "fileExistsBehavior"
                    Core.<*> x Core..:? "ignoreApplicationStopFailures"
                    Core.<*> x Core..:? "instanceTerminationWaitTimeStarted"
                    Core.<*> x Core..:? "loadBalancerInfo"
                    Core.<*> x Core..:? "previousRevision"
                    Core.<*> x Core..:? "revision"
                    Core.<*> x Core..:? "rollbackInfo"
                    Core.<*> x Core..:? "startTime"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "targetInstances"
                    Core.<*> x Core..:? "updateOutdatedInstancesOnly"
