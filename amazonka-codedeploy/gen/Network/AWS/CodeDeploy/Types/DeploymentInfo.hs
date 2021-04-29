{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a deployment.
--
-- /See:/ 'newDeploymentInfo' smart constructor.
data DeploymentInfo = DeploymentInfo'
  { -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the deployment as a whole.
    status :: Prelude.Maybe DeploymentStatus,
    -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | If true, then if an @ApplicationStop@, @BeforeBlockTraffic@, or
    -- @AfterBlockTraffic@ deployment lifecycle event to an instance fails,
    -- then the deployment continues to the next deployment lifecycle event.
    -- For example, if @ApplicationStop@ fails, the deployment continues with
    -- DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues
    -- with @BlockTraffic@. If @AfterBlockTraffic@ fails, the deployment
    -- continues with @ApplicationStop@.
    --
    -- If false or not specified, then if a lifecycle event fails during a
    -- deployment to an instance, that deployment fails. If deployment to that
    -- instance is part of an overall deployment and the number of healthy
    -- hosts is not less than the minimum number of healthy hosts, then a
    -- deployment to the next instance is attempted.
    --
    -- During a deployment, the AWS CodeDeploy agent runs the scripts specified
    -- for @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in
    -- the AppSpec file from the previous successful deployment. (All other
    -- scripts are run from the AppSpec file in the current deployment.) If one
    -- of these scripts contains an error and does not run successfully, the
    -- deployment can fail.
    --
    -- If the cause of the failure is a script from the last successful
    -- deployment that will never run successfully, create a new deployment and
    -- use @ignoreApplicationStopFailures@ to specify that the
    -- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
    -- failures should be ignored.
    ignoreApplicationStopFailures :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether only instances that are not running the latest
    -- application revision are to be deployed to.
    updateOutdatedInstancesOnly :: Prelude.Maybe Prelude.Bool,
    -- | Information about the automatic rollback configuration associated with
    -- the deployment.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | The deployment group name.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | Information about the instances that belong to the replacement
    -- environment in a blue\/green deployment.
    targetInstances :: Prelude.Maybe TargetInstances,
    -- | A timestamp that indicates when the deployment was deployed to the
    -- deployment group.
    --
    -- In some cases, the reported value of the start time might be later than
    -- the complete time. This is due to differences in the clock settings of
    -- backend servers that participate in the deployment process.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | Indicates whether the wait period set for the termination of instances
    -- in the original environment has started. Status is \'false\' if the
    -- KEEP_ALIVE option is specified. Otherwise, \'true\' as soon as the
    -- termination wait period starts.
    instanceTerminationWaitTimeStarted :: Prelude.Maybe Prelude.Bool,
    -- | Information about the application revision that was deployed to the
    -- deployment group before the most recent successful deployment.
    previousRevision :: Prelude.Maybe RevisionLocation,
    -- | Information about the load balancer used in the deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | A timestamp that indicates when the deployment was complete.
    completeTime :: Prelude.Maybe Prelude.POSIX,
    -- | Information about any error associated with this deployment.
    errorInformation :: Prelude.Maybe ErrorInformation,
    -- | Information about blue\/green deployment options for this deployment.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | The means by which the deployment was created:
    --
    -- -   @user@: A user created the deployment.
    --
    -- -   @autoscaling@: Amazon EC2 Auto Scaling created the deployment.
    --
    -- -   @codeDeployRollback@: A rollback process created the deployment.
    creator :: Prelude.Maybe DeploymentCreator,
    -- | A timestamp that indicates when the deployment was created.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | A comment about the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | Information about the location of stored application artifacts and the
    -- service from which to retrieve them.
    revision :: Prelude.Maybe RevisionLocation,
    -- | Information about a deployment rollback.
    rollbackInfo :: Prelude.Maybe RollbackInfo,
    -- | The unique ID for an external resource (for example, a CloudFormation
    -- stack ID) that is linked to this deployment.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | Messages that contain information about the status of a deployment.
    deploymentStatusMessages :: Prelude.Maybe [Prelude.Text],
    -- | Information about how AWS CodeDeploy handles files that already exist in
    -- a deployment target location but weren\'t part of the previous
    -- successful deployment.
    --
    -- -   @DISALLOW@: The deployment fails. This is also the default behavior
    --     if no option is specified.
    --
    -- -   @OVERWRITE@: The version of the file from the application revision
    --     currently being deployed replaces the version already on the
    --     instance.
    --
    -- -   @RETAIN@: The version of the file already on the instance is kept
    --     and used as part of the new deployment.
    fileExistsBehavior :: Prelude.Maybe FileExistsBehavior,
    -- | Provides information about the results of a deployment, such as whether
    -- instances in the original environment in a blue\/green deployment were
    -- not terminated.
    additionalDeploymentStatusInfo :: Prelude.Maybe Prelude.Text,
    -- | A summary of the deployment status of the instances in the deployment.
    deploymentOverview :: Prelude.Maybe DeploymentOverview,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeploymentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'deploymentInfo_deploymentId' - The unique ID of a deployment.
--
-- 'status', 'deploymentInfo_status' - The current state of the deployment as a whole.
--
-- 'deploymentConfigName', 'deploymentInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'ignoreApplicationStopFailures', 'deploymentInfo_ignoreApplicationStopFailures' - If true, then if an @ApplicationStop@, @BeforeBlockTraffic@, or
-- @AfterBlockTraffic@ deployment lifecycle event to an instance fails,
-- then the deployment continues to the next deployment lifecycle event.
-- For example, if @ApplicationStop@ fails, the deployment continues with
-- DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues
-- with @BlockTraffic@. If @AfterBlockTraffic@ fails, the deployment
-- continues with @ApplicationStop@.
--
-- If false or not specified, then if a lifecycle event fails during a
-- deployment to an instance, that deployment fails. If deployment to that
-- instance is part of an overall deployment and the number of healthy
-- hosts is not less than the minimum number of healthy hosts, then a
-- deployment to the next instance is attempted.
--
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified
-- for @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in
-- the AppSpec file from the previous successful deployment. (All other
-- scripts are run from the AppSpec file in the current deployment.) If one
-- of these scripts contains an error and does not run successfully, the
-- deployment can fail.
--
-- If the cause of the failure is a script from the last successful
-- deployment that will never run successfully, create a new deployment and
-- use @ignoreApplicationStopFailures@ to specify that the
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
-- failures should be ignored.
--
-- 'updateOutdatedInstancesOnly', 'deploymentInfo_updateOutdatedInstancesOnly' - Indicates whether only instances that are not running the latest
-- application revision are to be deployed to.
--
-- 'autoRollbackConfiguration', 'deploymentInfo_autoRollbackConfiguration' - Information about the automatic rollback configuration associated with
-- the deployment.
--
-- 'deploymentGroupName', 'deploymentInfo_deploymentGroupName' - The deployment group name.
--
-- 'targetInstances', 'deploymentInfo_targetInstances' - Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
--
-- 'startTime', 'deploymentInfo_startTime' - A timestamp that indicates when the deployment was deployed to the
-- deployment group.
--
-- In some cases, the reported value of the start time might be later than
-- the complete time. This is due to differences in the clock settings of
-- backend servers that participate in the deployment process.
--
-- 'instanceTerminationWaitTimeStarted', 'deploymentInfo_instanceTerminationWaitTimeStarted' - Indicates whether the wait period set for the termination of instances
-- in the original environment has started. Status is \'false\' if the
-- KEEP_ALIVE option is specified. Otherwise, \'true\' as soon as the
-- termination wait period starts.
--
-- 'previousRevision', 'deploymentInfo_previousRevision' - Information about the application revision that was deployed to the
-- deployment group before the most recent successful deployment.
--
-- 'loadBalancerInfo', 'deploymentInfo_loadBalancerInfo' - Information about the load balancer used in the deployment.
--
-- 'completeTime', 'deploymentInfo_completeTime' - A timestamp that indicates when the deployment was complete.
--
-- 'errorInformation', 'deploymentInfo_errorInformation' - Information about any error associated with this deployment.
--
-- 'blueGreenDeploymentConfiguration', 'deploymentInfo_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for this deployment.
--
-- 'creator', 'deploymentInfo_creator' - The means by which the deployment was created:
--
-- -   @user@: A user created the deployment.
--
-- -   @autoscaling@: Amazon EC2 Auto Scaling created the deployment.
--
-- -   @codeDeployRollback@: A rollback process created the deployment.
--
-- 'createTime', 'deploymentInfo_createTime' - A timestamp that indicates when the deployment was created.
--
-- 'description', 'deploymentInfo_description' - A comment about the deployment.
--
-- 'deploymentStyle', 'deploymentInfo_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'revision', 'deploymentInfo_revision' - Information about the location of stored application artifacts and the
-- service from which to retrieve them.
--
-- 'rollbackInfo', 'deploymentInfo_rollbackInfo' - Information about a deployment rollback.
--
-- 'externalId', 'deploymentInfo_externalId' - The unique ID for an external resource (for example, a CloudFormation
-- stack ID) that is linked to this deployment.
--
-- 'applicationName', 'deploymentInfo_applicationName' - The application name.
--
-- 'deploymentStatusMessages', 'deploymentInfo_deploymentStatusMessages' - Messages that contain information about the status of a deployment.
--
-- 'fileExistsBehavior', 'deploymentInfo_fileExistsBehavior' - Information about how AWS CodeDeploy handles files that already exist in
-- a deployment target location but weren\'t part of the previous
-- successful deployment.
--
-- -   @DISALLOW@: The deployment fails. This is also the default behavior
--     if no option is specified.
--
-- -   @OVERWRITE@: The version of the file from the application revision
--     currently being deployed replaces the version already on the
--     instance.
--
-- -   @RETAIN@: The version of the file already on the instance is kept
--     and used as part of the new deployment.
--
-- 'additionalDeploymentStatusInfo', 'deploymentInfo_additionalDeploymentStatusInfo' - Provides information about the results of a deployment, such as whether
-- instances in the original environment in a blue\/green deployment were
-- not terminated.
--
-- 'deploymentOverview', 'deploymentInfo_deploymentOverview' - A summary of the deployment status of the instances in the deployment.
--
-- 'computePlatform', 'deploymentInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
newDeploymentInfo ::
  DeploymentInfo
newDeploymentInfo =
  DeploymentInfo'
    { deploymentId = Prelude.Nothing,
      status = Prelude.Nothing,
      deploymentConfigName = Prelude.Nothing,
      ignoreApplicationStopFailures = Prelude.Nothing,
      updateOutdatedInstancesOnly = Prelude.Nothing,
      autoRollbackConfiguration = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      targetInstances = Prelude.Nothing,
      startTime = Prelude.Nothing,
      instanceTerminationWaitTimeStarted = Prelude.Nothing,
      previousRevision = Prelude.Nothing,
      loadBalancerInfo = Prelude.Nothing,
      completeTime = Prelude.Nothing,
      errorInformation = Prelude.Nothing,
      blueGreenDeploymentConfiguration = Prelude.Nothing,
      creator = Prelude.Nothing,
      createTime = Prelude.Nothing,
      description = Prelude.Nothing,
      deploymentStyle = Prelude.Nothing,
      revision = Prelude.Nothing,
      rollbackInfo = Prelude.Nothing,
      externalId = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      deploymentStatusMessages = Prelude.Nothing,
      fileExistsBehavior = Prelude.Nothing,
      additionalDeploymentStatusInfo = Prelude.Nothing,
      deploymentOverview = Prelude.Nothing,
      computePlatform = Prelude.Nothing
    }

-- | The unique ID of a deployment.
deploymentInfo_deploymentId :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_deploymentId = Lens.lens (\DeploymentInfo' {deploymentId} -> deploymentId) (\s@DeploymentInfo' {} a -> s {deploymentId = a} :: DeploymentInfo)

-- | The current state of the deployment as a whole.
deploymentInfo_status :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentStatus)
deploymentInfo_status = Lens.lens (\DeploymentInfo' {status} -> status) (\s@DeploymentInfo' {} a -> s {status = a} :: DeploymentInfo)

-- | The deployment configuration name.
deploymentInfo_deploymentConfigName :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_deploymentConfigName = Lens.lens (\DeploymentInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentInfo' {} a -> s {deploymentConfigName = a} :: DeploymentInfo)

-- | If true, then if an @ApplicationStop@, @BeforeBlockTraffic@, or
-- @AfterBlockTraffic@ deployment lifecycle event to an instance fails,
-- then the deployment continues to the next deployment lifecycle event.
-- For example, if @ApplicationStop@ fails, the deployment continues with
-- DownloadBundle. If @BeforeBlockTraffic@ fails, the deployment continues
-- with @BlockTraffic@. If @AfterBlockTraffic@ fails, the deployment
-- continues with @ApplicationStop@.
--
-- If false or not specified, then if a lifecycle event fails during a
-- deployment to an instance, that deployment fails. If deployment to that
-- instance is part of an overall deployment and the number of healthy
-- hosts is not less than the minimum number of healthy hosts, then a
-- deployment to the next instance is attempted.
--
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified
-- for @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in
-- the AppSpec file from the previous successful deployment. (All other
-- scripts are run from the AppSpec file in the current deployment.) If one
-- of these scripts contains an error and does not run successfully, the
-- deployment can fail.
--
-- If the cause of the failure is a script from the last successful
-- deployment that will never run successfully, create a new deployment and
-- use @ignoreApplicationStopFailures@ to specify that the
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
-- failures should be ignored.
deploymentInfo_ignoreApplicationStopFailures :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Bool)
deploymentInfo_ignoreApplicationStopFailures = Lens.lens (\DeploymentInfo' {ignoreApplicationStopFailures} -> ignoreApplicationStopFailures) (\s@DeploymentInfo' {} a -> s {ignoreApplicationStopFailures = a} :: DeploymentInfo)

-- | Indicates whether only instances that are not running the latest
-- application revision are to be deployed to.
deploymentInfo_updateOutdatedInstancesOnly :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Bool)
deploymentInfo_updateOutdatedInstancesOnly = Lens.lens (\DeploymentInfo' {updateOutdatedInstancesOnly} -> updateOutdatedInstancesOnly) (\s@DeploymentInfo' {} a -> s {updateOutdatedInstancesOnly = a} :: DeploymentInfo)

-- | Information about the automatic rollback configuration associated with
-- the deployment.
deploymentInfo_autoRollbackConfiguration :: Lens.Lens' DeploymentInfo (Prelude.Maybe AutoRollbackConfiguration)
deploymentInfo_autoRollbackConfiguration = Lens.lens (\DeploymentInfo' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentInfo' {} a -> s {autoRollbackConfiguration = a} :: DeploymentInfo)

-- | The deployment group name.
deploymentInfo_deploymentGroupName :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_deploymentGroupName = Lens.lens (\DeploymentInfo' {deploymentGroupName} -> deploymentGroupName) (\s@DeploymentInfo' {} a -> s {deploymentGroupName = a} :: DeploymentInfo)

-- | Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
deploymentInfo_targetInstances :: Lens.Lens' DeploymentInfo (Prelude.Maybe TargetInstances)
deploymentInfo_targetInstances = Lens.lens (\DeploymentInfo' {targetInstances} -> targetInstances) (\s@DeploymentInfo' {} a -> s {targetInstances = a} :: DeploymentInfo)

-- | A timestamp that indicates when the deployment was deployed to the
-- deployment group.
--
-- In some cases, the reported value of the start time might be later than
-- the complete time. This is due to differences in the clock settings of
-- backend servers that participate in the deployment process.
deploymentInfo_startTime :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.UTCTime)
deploymentInfo_startTime = Lens.lens (\DeploymentInfo' {startTime} -> startTime) (\s@DeploymentInfo' {} a -> s {startTime = a} :: DeploymentInfo) Prelude.. Lens.mapping Prelude._Time

-- | Indicates whether the wait period set for the termination of instances
-- in the original environment has started. Status is \'false\' if the
-- KEEP_ALIVE option is specified. Otherwise, \'true\' as soon as the
-- termination wait period starts.
deploymentInfo_instanceTerminationWaitTimeStarted :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Bool)
deploymentInfo_instanceTerminationWaitTimeStarted = Lens.lens (\DeploymentInfo' {instanceTerminationWaitTimeStarted} -> instanceTerminationWaitTimeStarted) (\s@DeploymentInfo' {} a -> s {instanceTerminationWaitTimeStarted = a} :: DeploymentInfo)

-- | Information about the application revision that was deployed to the
-- deployment group before the most recent successful deployment.
deploymentInfo_previousRevision :: Lens.Lens' DeploymentInfo (Prelude.Maybe RevisionLocation)
deploymentInfo_previousRevision = Lens.lens (\DeploymentInfo' {previousRevision} -> previousRevision) (\s@DeploymentInfo' {} a -> s {previousRevision = a} :: DeploymentInfo)

-- | Information about the load balancer used in the deployment.
deploymentInfo_loadBalancerInfo :: Lens.Lens' DeploymentInfo (Prelude.Maybe LoadBalancerInfo)
deploymentInfo_loadBalancerInfo = Lens.lens (\DeploymentInfo' {loadBalancerInfo} -> loadBalancerInfo) (\s@DeploymentInfo' {} a -> s {loadBalancerInfo = a} :: DeploymentInfo)

-- | A timestamp that indicates when the deployment was complete.
deploymentInfo_completeTime :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.UTCTime)
deploymentInfo_completeTime = Lens.lens (\DeploymentInfo' {completeTime} -> completeTime) (\s@DeploymentInfo' {} a -> s {completeTime = a} :: DeploymentInfo) Prelude.. Lens.mapping Prelude._Time

-- | Information about any error associated with this deployment.
deploymentInfo_errorInformation :: Lens.Lens' DeploymentInfo (Prelude.Maybe ErrorInformation)
deploymentInfo_errorInformation = Lens.lens (\DeploymentInfo' {errorInformation} -> errorInformation) (\s@DeploymentInfo' {} a -> s {errorInformation = a} :: DeploymentInfo)

-- | Information about blue\/green deployment options for this deployment.
deploymentInfo_blueGreenDeploymentConfiguration :: Lens.Lens' DeploymentInfo (Prelude.Maybe BlueGreenDeploymentConfiguration)
deploymentInfo_blueGreenDeploymentConfiguration = Lens.lens (\DeploymentInfo' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@DeploymentInfo' {} a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentInfo)

-- | The means by which the deployment was created:
--
-- -   @user@: A user created the deployment.
--
-- -   @autoscaling@: Amazon EC2 Auto Scaling created the deployment.
--
-- -   @codeDeployRollback@: A rollback process created the deployment.
deploymentInfo_creator :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentCreator)
deploymentInfo_creator = Lens.lens (\DeploymentInfo' {creator} -> creator) (\s@DeploymentInfo' {} a -> s {creator = a} :: DeploymentInfo)

-- | A timestamp that indicates when the deployment was created.
deploymentInfo_createTime :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.UTCTime)
deploymentInfo_createTime = Lens.lens (\DeploymentInfo' {createTime} -> createTime) (\s@DeploymentInfo' {} a -> s {createTime = a} :: DeploymentInfo) Prelude.. Lens.mapping Prelude._Time

-- | A comment about the deployment.
deploymentInfo_description :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_description = Lens.lens (\DeploymentInfo' {description} -> description) (\s@DeploymentInfo' {} a -> s {description = a} :: DeploymentInfo)

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
deploymentInfo_deploymentStyle :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentStyle)
deploymentInfo_deploymentStyle = Lens.lens (\DeploymentInfo' {deploymentStyle} -> deploymentStyle) (\s@DeploymentInfo' {} a -> s {deploymentStyle = a} :: DeploymentInfo)

-- | Information about the location of stored application artifacts and the
-- service from which to retrieve them.
deploymentInfo_revision :: Lens.Lens' DeploymentInfo (Prelude.Maybe RevisionLocation)
deploymentInfo_revision = Lens.lens (\DeploymentInfo' {revision} -> revision) (\s@DeploymentInfo' {} a -> s {revision = a} :: DeploymentInfo)

-- | Information about a deployment rollback.
deploymentInfo_rollbackInfo :: Lens.Lens' DeploymentInfo (Prelude.Maybe RollbackInfo)
deploymentInfo_rollbackInfo = Lens.lens (\DeploymentInfo' {rollbackInfo} -> rollbackInfo) (\s@DeploymentInfo' {} a -> s {rollbackInfo = a} :: DeploymentInfo)

-- | The unique ID for an external resource (for example, a CloudFormation
-- stack ID) that is linked to this deployment.
deploymentInfo_externalId :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_externalId = Lens.lens (\DeploymentInfo' {externalId} -> externalId) (\s@DeploymentInfo' {} a -> s {externalId = a} :: DeploymentInfo)

-- | The application name.
deploymentInfo_applicationName :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_applicationName = Lens.lens (\DeploymentInfo' {applicationName} -> applicationName) (\s@DeploymentInfo' {} a -> s {applicationName = a} :: DeploymentInfo)

-- | Messages that contain information about the status of a deployment.
deploymentInfo_deploymentStatusMessages :: Lens.Lens' DeploymentInfo (Prelude.Maybe [Prelude.Text])
deploymentInfo_deploymentStatusMessages = Lens.lens (\DeploymentInfo' {deploymentStatusMessages} -> deploymentStatusMessages) (\s@DeploymentInfo' {} a -> s {deploymentStatusMessages = a} :: DeploymentInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about how AWS CodeDeploy handles files that already exist in
-- a deployment target location but weren\'t part of the previous
-- successful deployment.
--
-- -   @DISALLOW@: The deployment fails. This is also the default behavior
--     if no option is specified.
--
-- -   @OVERWRITE@: The version of the file from the application revision
--     currently being deployed replaces the version already on the
--     instance.
--
-- -   @RETAIN@: The version of the file already on the instance is kept
--     and used as part of the new deployment.
deploymentInfo_fileExistsBehavior :: Lens.Lens' DeploymentInfo (Prelude.Maybe FileExistsBehavior)
deploymentInfo_fileExistsBehavior = Lens.lens (\DeploymentInfo' {fileExistsBehavior} -> fileExistsBehavior) (\s@DeploymentInfo' {} a -> s {fileExistsBehavior = a} :: DeploymentInfo)

-- | Provides information about the results of a deployment, such as whether
-- instances in the original environment in a blue\/green deployment were
-- not terminated.
deploymentInfo_additionalDeploymentStatusInfo :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_additionalDeploymentStatusInfo = Lens.lens (\DeploymentInfo' {additionalDeploymentStatusInfo} -> additionalDeploymentStatusInfo) (\s@DeploymentInfo' {} a -> s {additionalDeploymentStatusInfo = a} :: DeploymentInfo)

-- | A summary of the deployment status of the instances in the deployment.
deploymentInfo_deploymentOverview :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentOverview)
deploymentInfo_deploymentOverview = Lens.lens (\DeploymentInfo' {deploymentOverview} -> deploymentOverview) (\s@DeploymentInfo' {} a -> s {deploymentOverview = a} :: DeploymentInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentInfo_computePlatform :: Lens.Lens' DeploymentInfo (Prelude.Maybe ComputePlatform)
deploymentInfo_computePlatform = Lens.lens (\DeploymentInfo' {computePlatform} -> computePlatform) (\s@DeploymentInfo' {} a -> s {computePlatform = a} :: DeploymentInfo)

instance Prelude.FromJSON DeploymentInfo where
  parseJSON =
    Prelude.withObject
      "DeploymentInfo"
      ( \x ->
          DeploymentInfo'
            Prelude.<$> (x Prelude..:? "deploymentId")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "deploymentConfigName")
            Prelude.<*> (x Prelude..:? "ignoreApplicationStopFailures")
            Prelude.<*> (x Prelude..:? "updateOutdatedInstancesOnly")
            Prelude.<*> (x Prelude..:? "autoRollbackConfiguration")
            Prelude.<*> (x Prelude..:? "deploymentGroupName")
            Prelude.<*> (x Prelude..:? "targetInstances")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "instanceTerminationWaitTimeStarted")
            Prelude.<*> (x Prelude..:? "previousRevision")
            Prelude.<*> (x Prelude..:? "loadBalancerInfo")
            Prelude.<*> (x Prelude..:? "completeTime")
            Prelude.<*> (x Prelude..:? "errorInformation")
            Prelude.<*> (x Prelude..:? "blueGreenDeploymentConfiguration")
            Prelude.<*> (x Prelude..:? "creator")
            Prelude.<*> (x Prelude..:? "createTime")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "deploymentStyle")
            Prelude.<*> (x Prelude..:? "revision")
            Prelude.<*> (x Prelude..:? "rollbackInfo")
            Prelude.<*> (x Prelude..:? "externalId")
            Prelude.<*> (x Prelude..:? "applicationName")
            Prelude.<*> ( x Prelude..:? "deploymentStatusMessages"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "fileExistsBehavior")
            Prelude.<*> (x Prelude..:? "additionalDeploymentStatusInfo")
            Prelude.<*> (x Prelude..:? "deploymentOverview")
            Prelude.<*> (x Prelude..:? "computePlatform")
      )

instance Prelude.Hashable DeploymentInfo

instance Prelude.NFData DeploymentInfo
