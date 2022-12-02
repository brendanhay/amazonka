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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentInfo where

import Amazonka.CodeDeploy.Types.AlarmConfiguration
import Amazonka.CodeDeploy.Types.AutoRollbackConfiguration
import Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Amazonka.CodeDeploy.Types.ComputePlatform
import Amazonka.CodeDeploy.Types.DeploymentCreator
import Amazonka.CodeDeploy.Types.DeploymentOverview
import Amazonka.CodeDeploy.Types.DeploymentStatus
import Amazonka.CodeDeploy.Types.DeploymentStyle
import Amazonka.CodeDeploy.Types.ErrorInformation
import Amazonka.CodeDeploy.Types.FileExistsBehavior
import Amazonka.CodeDeploy.Types.LoadBalancerInfo
import Amazonka.CodeDeploy.Types.RelatedDeployments
import Amazonka.CodeDeploy.Types.RevisionLocation
import Amazonka.CodeDeploy.Types.RollbackInfo
import Amazonka.CodeDeploy.Types.TargetInstances
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment.
--
-- /See:/ 'newDeploymentInfo' smart constructor.
data DeploymentInfo = DeploymentInfo'
  { overrideAlarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | Information about the load balancer used in the deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | The deployment group name.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Information about how CodeDeploy handles files that already exist in a
    -- deployment target location but weren\'t part of the previous successful
    -- deployment.
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
    -- | A summary of the deployment status of the instances in the deployment.
    deploymentOverview :: Prelude.Maybe DeploymentOverview,
    -- | Information about the location of stored application artifacts and the
    -- service from which to retrieve them.
    revision :: Prelude.Maybe RevisionLocation,
    -- | Information about a deployment rollback.
    rollbackInfo :: Prelude.Maybe RollbackInfo,
    -- | The unique ID for an external resource (for example, a CloudFormation
    -- stack ID) that is linked to this deployment.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the deployment as a whole.
    status :: Prelude.Maybe DeploymentStatus,
    -- | A comment about the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | Indicates whether only instances that are not running the latest
    -- application revision are to be deployed to.
    updateOutdatedInstancesOnly :: Prelude.Maybe Prelude.Bool,
    -- | A timestamp that indicates when the deployment was complete.
    completeTime :: Prelude.Maybe Data.POSIX,
    -- | Messages that contain information about the status of a deployment.
    deploymentStatusMessages :: Prelude.Maybe [Prelude.Text],
    -- | Information about the automatic rollback configuration associated with
    -- the deployment.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | Information about blue\/green deployment options for this deployment.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | Information about any error associated with this deployment.
    errorInformation :: Prelude.Maybe ErrorInformation,
    relatedDeployments :: Prelude.Maybe RelatedDeployments,
    -- | Information about the instances that belong to the replacement
    -- environment in a blue\/green deployment.
    targetInstances :: Prelude.Maybe TargetInstances,
    -- | A timestamp that indicates when the deployment was created.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | Provides information about the results of a deployment, such as whether
    -- instances in the original environment in a blue\/green deployment were
    -- not terminated.
    additionalDeploymentStatusInfo :: Prelude.Maybe Prelude.Text,
    -- | The means by which the deployment was created:
    --
    -- -   @user@: A user created the deployment.
    --
    -- -   @autoscaling@: Amazon EC2 Auto Scaling created the deployment.
    --
    -- -   @codeDeployRollback@: A rollback process created the deployment.
    --
    -- -   @CodeDeployAutoUpdate@: An auto-update process created the
    --     deployment when it detected outdated Amazon EC2 instances.
    creator :: Prelude.Maybe DeploymentCreator,
    -- | Information about the application revision that was deployed to the
    -- deployment group before the most recent successful deployment.
    previousRevision :: Prelude.Maybe RevisionLocation,
    -- | A timestamp that indicates when the deployment was deployed to the
    -- deployment group.
    --
    -- In some cases, the reported value of the start time might be later than
    -- the complete time. This is due to differences in the clock settings of
    -- backend servers that participate in the deployment process.
    startTime :: Prelude.Maybe Data.POSIX,
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
    -- During a deployment, the CodeDeploy agent runs the scripts specified for
    -- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in the
    -- AppSpec file from the previous successful deployment. (All other scripts
    -- are run from the AppSpec file in the current deployment.) If one of
    -- these scripts contains an error and does not run successfully, the
    -- deployment can fail.
    --
    -- If the cause of the failure is a script from the last successful
    -- deployment that will never run successfully, create a new deployment and
    -- use @ignoreApplicationStopFailures@ to specify that the
    -- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
    -- failures should be ignored.
    ignoreApplicationStopFailures :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the wait period set for the termination of instances
    -- in the original environment has started. Status is \'false\' if the
    -- KEEP_ALIVE option is specified. Otherwise, \'true\' as soon as the
    -- termination wait period starts.
    instanceTerminationWaitTimeStarted :: Prelude.Maybe Prelude.Bool,
    -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrideAlarmConfiguration', 'deploymentInfo_overrideAlarmConfiguration' - Undocumented member.
--
-- 'loadBalancerInfo', 'deploymentInfo_loadBalancerInfo' - Information about the load balancer used in the deployment.
--
-- 'deploymentGroupName', 'deploymentInfo_deploymentGroupName' - The deployment group name.
--
-- 'deploymentId', 'deploymentInfo_deploymentId' - The unique ID of a deployment.
--
-- 'fileExistsBehavior', 'deploymentInfo_fileExistsBehavior' - Information about how CodeDeploy handles files that already exist in a
-- deployment target location but weren\'t part of the previous successful
-- deployment.
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
-- 'deploymentOverview', 'deploymentInfo_deploymentOverview' - A summary of the deployment status of the instances in the deployment.
--
-- 'revision', 'deploymentInfo_revision' - Information about the location of stored application artifacts and the
-- service from which to retrieve them.
--
-- 'rollbackInfo', 'deploymentInfo_rollbackInfo' - Information about a deployment rollback.
--
-- 'externalId', 'deploymentInfo_externalId' - The unique ID for an external resource (for example, a CloudFormation
-- stack ID) that is linked to this deployment.
--
-- 'status', 'deploymentInfo_status' - The current state of the deployment as a whole.
--
-- 'description', 'deploymentInfo_description' - A comment about the deployment.
--
-- 'computePlatform', 'deploymentInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
--
-- 'updateOutdatedInstancesOnly', 'deploymentInfo_updateOutdatedInstancesOnly' - Indicates whether only instances that are not running the latest
-- application revision are to be deployed to.
--
-- 'completeTime', 'deploymentInfo_completeTime' - A timestamp that indicates when the deployment was complete.
--
-- 'deploymentStatusMessages', 'deploymentInfo_deploymentStatusMessages' - Messages that contain information about the status of a deployment.
--
-- 'autoRollbackConfiguration', 'deploymentInfo_autoRollbackConfiguration' - Information about the automatic rollback configuration associated with
-- the deployment.
--
-- 'deploymentStyle', 'deploymentInfo_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'blueGreenDeploymentConfiguration', 'deploymentInfo_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for this deployment.
--
-- 'errorInformation', 'deploymentInfo_errorInformation' - Information about any error associated with this deployment.
--
-- 'relatedDeployments', 'deploymentInfo_relatedDeployments' - Undocumented member.
--
-- 'targetInstances', 'deploymentInfo_targetInstances' - Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
--
-- 'createTime', 'deploymentInfo_createTime' - A timestamp that indicates when the deployment was created.
--
-- 'additionalDeploymentStatusInfo', 'deploymentInfo_additionalDeploymentStatusInfo' - Provides information about the results of a deployment, such as whether
-- instances in the original environment in a blue\/green deployment were
-- not terminated.
--
-- 'creator', 'deploymentInfo_creator' - The means by which the deployment was created:
--
-- -   @user@: A user created the deployment.
--
-- -   @autoscaling@: Amazon EC2 Auto Scaling created the deployment.
--
-- -   @codeDeployRollback@: A rollback process created the deployment.
--
-- -   @CodeDeployAutoUpdate@: An auto-update process created the
--     deployment when it detected outdated Amazon EC2 instances.
--
-- 'previousRevision', 'deploymentInfo_previousRevision' - Information about the application revision that was deployed to the
-- deployment group before the most recent successful deployment.
--
-- 'startTime', 'deploymentInfo_startTime' - A timestamp that indicates when the deployment was deployed to the
-- deployment group.
--
-- In some cases, the reported value of the start time might be later than
-- the complete time. This is due to differences in the clock settings of
-- backend servers that participate in the deployment process.
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
-- During a deployment, the CodeDeploy agent runs the scripts specified for
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in the
-- AppSpec file from the previous successful deployment. (All other scripts
-- are run from the AppSpec file in the current deployment.) If one of
-- these scripts contains an error and does not run successfully, the
-- deployment can fail.
--
-- If the cause of the failure is a script from the last successful
-- deployment that will never run successfully, create a new deployment and
-- use @ignoreApplicationStopFailures@ to specify that the
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
-- failures should be ignored.
--
-- 'instanceTerminationWaitTimeStarted', 'deploymentInfo_instanceTerminationWaitTimeStarted' - Indicates whether the wait period set for the termination of instances
-- in the original environment has started. Status is \'false\' if the
-- KEEP_ALIVE option is specified. Otherwise, \'true\' as soon as the
-- termination wait period starts.
--
-- 'deploymentConfigName', 'deploymentInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'applicationName', 'deploymentInfo_applicationName' - The application name.
newDeploymentInfo ::
  DeploymentInfo
newDeploymentInfo =
  DeploymentInfo'
    { overrideAlarmConfiguration =
        Prelude.Nothing,
      loadBalancerInfo = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      fileExistsBehavior = Prelude.Nothing,
      deploymentOverview = Prelude.Nothing,
      revision = Prelude.Nothing,
      rollbackInfo = Prelude.Nothing,
      externalId = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      computePlatform = Prelude.Nothing,
      updateOutdatedInstancesOnly = Prelude.Nothing,
      completeTime = Prelude.Nothing,
      deploymentStatusMessages = Prelude.Nothing,
      autoRollbackConfiguration = Prelude.Nothing,
      deploymentStyle = Prelude.Nothing,
      blueGreenDeploymentConfiguration = Prelude.Nothing,
      errorInformation = Prelude.Nothing,
      relatedDeployments = Prelude.Nothing,
      targetInstances = Prelude.Nothing,
      createTime = Prelude.Nothing,
      additionalDeploymentStatusInfo = Prelude.Nothing,
      creator = Prelude.Nothing,
      previousRevision = Prelude.Nothing,
      startTime = Prelude.Nothing,
      ignoreApplicationStopFailures = Prelude.Nothing,
      instanceTerminationWaitTimeStarted = Prelude.Nothing,
      deploymentConfigName = Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | Undocumented member.
deploymentInfo_overrideAlarmConfiguration :: Lens.Lens' DeploymentInfo (Prelude.Maybe AlarmConfiguration)
deploymentInfo_overrideAlarmConfiguration = Lens.lens (\DeploymentInfo' {overrideAlarmConfiguration} -> overrideAlarmConfiguration) (\s@DeploymentInfo' {} a -> s {overrideAlarmConfiguration = a} :: DeploymentInfo)

-- | Information about the load balancer used in the deployment.
deploymentInfo_loadBalancerInfo :: Lens.Lens' DeploymentInfo (Prelude.Maybe LoadBalancerInfo)
deploymentInfo_loadBalancerInfo = Lens.lens (\DeploymentInfo' {loadBalancerInfo} -> loadBalancerInfo) (\s@DeploymentInfo' {} a -> s {loadBalancerInfo = a} :: DeploymentInfo)

-- | The deployment group name.
deploymentInfo_deploymentGroupName :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_deploymentGroupName = Lens.lens (\DeploymentInfo' {deploymentGroupName} -> deploymentGroupName) (\s@DeploymentInfo' {} a -> s {deploymentGroupName = a} :: DeploymentInfo)

-- | The unique ID of a deployment.
deploymentInfo_deploymentId :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_deploymentId = Lens.lens (\DeploymentInfo' {deploymentId} -> deploymentId) (\s@DeploymentInfo' {} a -> s {deploymentId = a} :: DeploymentInfo)

-- | Information about how CodeDeploy handles files that already exist in a
-- deployment target location but weren\'t part of the previous successful
-- deployment.
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

-- | A summary of the deployment status of the instances in the deployment.
deploymentInfo_deploymentOverview :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentOverview)
deploymentInfo_deploymentOverview = Lens.lens (\DeploymentInfo' {deploymentOverview} -> deploymentOverview) (\s@DeploymentInfo' {} a -> s {deploymentOverview = a} :: DeploymentInfo)

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

-- | The current state of the deployment as a whole.
deploymentInfo_status :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentStatus)
deploymentInfo_status = Lens.lens (\DeploymentInfo' {status} -> status) (\s@DeploymentInfo' {} a -> s {status = a} :: DeploymentInfo)

-- | A comment about the deployment.
deploymentInfo_description :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_description = Lens.lens (\DeploymentInfo' {description} -> description) (\s@DeploymentInfo' {} a -> s {description = a} :: DeploymentInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentInfo_computePlatform :: Lens.Lens' DeploymentInfo (Prelude.Maybe ComputePlatform)
deploymentInfo_computePlatform = Lens.lens (\DeploymentInfo' {computePlatform} -> computePlatform) (\s@DeploymentInfo' {} a -> s {computePlatform = a} :: DeploymentInfo)

-- | Indicates whether only instances that are not running the latest
-- application revision are to be deployed to.
deploymentInfo_updateOutdatedInstancesOnly :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Bool)
deploymentInfo_updateOutdatedInstancesOnly = Lens.lens (\DeploymentInfo' {updateOutdatedInstancesOnly} -> updateOutdatedInstancesOnly) (\s@DeploymentInfo' {} a -> s {updateOutdatedInstancesOnly = a} :: DeploymentInfo)

-- | A timestamp that indicates when the deployment was complete.
deploymentInfo_completeTime :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.UTCTime)
deploymentInfo_completeTime = Lens.lens (\DeploymentInfo' {completeTime} -> completeTime) (\s@DeploymentInfo' {} a -> s {completeTime = a} :: DeploymentInfo) Prelude.. Lens.mapping Data._Time

-- | Messages that contain information about the status of a deployment.
deploymentInfo_deploymentStatusMessages :: Lens.Lens' DeploymentInfo (Prelude.Maybe [Prelude.Text])
deploymentInfo_deploymentStatusMessages = Lens.lens (\DeploymentInfo' {deploymentStatusMessages} -> deploymentStatusMessages) (\s@DeploymentInfo' {} a -> s {deploymentStatusMessages = a} :: DeploymentInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about the automatic rollback configuration associated with
-- the deployment.
deploymentInfo_autoRollbackConfiguration :: Lens.Lens' DeploymentInfo (Prelude.Maybe AutoRollbackConfiguration)
deploymentInfo_autoRollbackConfiguration = Lens.lens (\DeploymentInfo' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentInfo' {} a -> s {autoRollbackConfiguration = a} :: DeploymentInfo)

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
deploymentInfo_deploymentStyle :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentStyle)
deploymentInfo_deploymentStyle = Lens.lens (\DeploymentInfo' {deploymentStyle} -> deploymentStyle) (\s@DeploymentInfo' {} a -> s {deploymentStyle = a} :: DeploymentInfo)

-- | Information about blue\/green deployment options for this deployment.
deploymentInfo_blueGreenDeploymentConfiguration :: Lens.Lens' DeploymentInfo (Prelude.Maybe BlueGreenDeploymentConfiguration)
deploymentInfo_blueGreenDeploymentConfiguration = Lens.lens (\DeploymentInfo' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@DeploymentInfo' {} a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentInfo)

-- | Information about any error associated with this deployment.
deploymentInfo_errorInformation :: Lens.Lens' DeploymentInfo (Prelude.Maybe ErrorInformation)
deploymentInfo_errorInformation = Lens.lens (\DeploymentInfo' {errorInformation} -> errorInformation) (\s@DeploymentInfo' {} a -> s {errorInformation = a} :: DeploymentInfo)

-- | Undocumented member.
deploymentInfo_relatedDeployments :: Lens.Lens' DeploymentInfo (Prelude.Maybe RelatedDeployments)
deploymentInfo_relatedDeployments = Lens.lens (\DeploymentInfo' {relatedDeployments} -> relatedDeployments) (\s@DeploymentInfo' {} a -> s {relatedDeployments = a} :: DeploymentInfo)

-- | Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
deploymentInfo_targetInstances :: Lens.Lens' DeploymentInfo (Prelude.Maybe TargetInstances)
deploymentInfo_targetInstances = Lens.lens (\DeploymentInfo' {targetInstances} -> targetInstances) (\s@DeploymentInfo' {} a -> s {targetInstances = a} :: DeploymentInfo)

-- | A timestamp that indicates when the deployment was created.
deploymentInfo_createTime :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.UTCTime)
deploymentInfo_createTime = Lens.lens (\DeploymentInfo' {createTime} -> createTime) (\s@DeploymentInfo' {} a -> s {createTime = a} :: DeploymentInfo) Prelude.. Lens.mapping Data._Time

-- | Provides information about the results of a deployment, such as whether
-- instances in the original environment in a blue\/green deployment were
-- not terminated.
deploymentInfo_additionalDeploymentStatusInfo :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_additionalDeploymentStatusInfo = Lens.lens (\DeploymentInfo' {additionalDeploymentStatusInfo} -> additionalDeploymentStatusInfo) (\s@DeploymentInfo' {} a -> s {additionalDeploymentStatusInfo = a} :: DeploymentInfo)

-- | The means by which the deployment was created:
--
-- -   @user@: A user created the deployment.
--
-- -   @autoscaling@: Amazon EC2 Auto Scaling created the deployment.
--
-- -   @codeDeployRollback@: A rollback process created the deployment.
--
-- -   @CodeDeployAutoUpdate@: An auto-update process created the
--     deployment when it detected outdated Amazon EC2 instances.
deploymentInfo_creator :: Lens.Lens' DeploymentInfo (Prelude.Maybe DeploymentCreator)
deploymentInfo_creator = Lens.lens (\DeploymentInfo' {creator} -> creator) (\s@DeploymentInfo' {} a -> s {creator = a} :: DeploymentInfo)

-- | Information about the application revision that was deployed to the
-- deployment group before the most recent successful deployment.
deploymentInfo_previousRevision :: Lens.Lens' DeploymentInfo (Prelude.Maybe RevisionLocation)
deploymentInfo_previousRevision = Lens.lens (\DeploymentInfo' {previousRevision} -> previousRevision) (\s@DeploymentInfo' {} a -> s {previousRevision = a} :: DeploymentInfo)

-- | A timestamp that indicates when the deployment was deployed to the
-- deployment group.
--
-- In some cases, the reported value of the start time might be later than
-- the complete time. This is due to differences in the clock settings of
-- backend servers that participate in the deployment process.
deploymentInfo_startTime :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.UTCTime)
deploymentInfo_startTime = Lens.lens (\DeploymentInfo' {startTime} -> startTime) (\s@DeploymentInfo' {} a -> s {startTime = a} :: DeploymentInfo) Prelude.. Lens.mapping Data._Time

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
-- During a deployment, the CodeDeploy agent runs the scripts specified for
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in the
-- AppSpec file from the previous successful deployment. (All other scripts
-- are run from the AppSpec file in the current deployment.) If one of
-- these scripts contains an error and does not run successfully, the
-- deployment can fail.
--
-- If the cause of the failure is a script from the last successful
-- deployment that will never run successfully, create a new deployment and
-- use @ignoreApplicationStopFailures@ to specify that the
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
-- failures should be ignored.
deploymentInfo_ignoreApplicationStopFailures :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Bool)
deploymentInfo_ignoreApplicationStopFailures = Lens.lens (\DeploymentInfo' {ignoreApplicationStopFailures} -> ignoreApplicationStopFailures) (\s@DeploymentInfo' {} a -> s {ignoreApplicationStopFailures = a} :: DeploymentInfo)

-- | Indicates whether the wait period set for the termination of instances
-- in the original environment has started. Status is \'false\' if the
-- KEEP_ALIVE option is specified. Otherwise, \'true\' as soon as the
-- termination wait period starts.
deploymentInfo_instanceTerminationWaitTimeStarted :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Bool)
deploymentInfo_instanceTerminationWaitTimeStarted = Lens.lens (\DeploymentInfo' {instanceTerminationWaitTimeStarted} -> instanceTerminationWaitTimeStarted) (\s@DeploymentInfo' {} a -> s {instanceTerminationWaitTimeStarted = a} :: DeploymentInfo)

-- | The deployment configuration name.
deploymentInfo_deploymentConfigName :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_deploymentConfigName = Lens.lens (\DeploymentInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentInfo' {} a -> s {deploymentConfigName = a} :: DeploymentInfo)

-- | The application name.
deploymentInfo_applicationName :: Lens.Lens' DeploymentInfo (Prelude.Maybe Prelude.Text)
deploymentInfo_applicationName = Lens.lens (\DeploymentInfo' {applicationName} -> applicationName) (\s@DeploymentInfo' {} a -> s {applicationName = a} :: DeploymentInfo)

instance Data.FromJSON DeploymentInfo where
  parseJSON =
    Data.withObject
      "DeploymentInfo"
      ( \x ->
          DeploymentInfo'
            Prelude.<$> (x Data..:? "overrideAlarmConfiguration")
            Prelude.<*> (x Data..:? "loadBalancerInfo")
            Prelude.<*> (x Data..:? "deploymentGroupName")
            Prelude.<*> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "fileExistsBehavior")
            Prelude.<*> (x Data..:? "deploymentOverview")
            Prelude.<*> (x Data..:? "revision")
            Prelude.<*> (x Data..:? "rollbackInfo")
            Prelude.<*> (x Data..:? "externalId")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "computePlatform")
            Prelude.<*> (x Data..:? "updateOutdatedInstancesOnly")
            Prelude.<*> (x Data..:? "completeTime")
            Prelude.<*> ( x Data..:? "deploymentStatusMessages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "autoRollbackConfiguration")
            Prelude.<*> (x Data..:? "deploymentStyle")
            Prelude.<*> (x Data..:? "blueGreenDeploymentConfiguration")
            Prelude.<*> (x Data..:? "errorInformation")
            Prelude.<*> (x Data..:? "relatedDeployments")
            Prelude.<*> (x Data..:? "targetInstances")
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "additionalDeploymentStatusInfo")
            Prelude.<*> (x Data..:? "creator")
            Prelude.<*> (x Data..:? "previousRevision")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "ignoreApplicationStopFailures")
            Prelude.<*> (x Data..:? "instanceTerminationWaitTimeStarted")
            Prelude.<*> (x Data..:? "deploymentConfigName")
            Prelude.<*> (x Data..:? "applicationName")
      )

instance Prelude.Hashable DeploymentInfo where
  hashWithSalt _salt DeploymentInfo' {..} =
    _salt
      `Prelude.hashWithSalt` overrideAlarmConfiguration
      `Prelude.hashWithSalt` loadBalancerInfo
      `Prelude.hashWithSalt` deploymentGroupName
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` fileExistsBehavior
      `Prelude.hashWithSalt` deploymentOverview
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` rollbackInfo
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` computePlatform
      `Prelude.hashWithSalt` updateOutdatedInstancesOnly
      `Prelude.hashWithSalt` completeTime
      `Prelude.hashWithSalt` deploymentStatusMessages
      `Prelude.hashWithSalt` autoRollbackConfiguration
      `Prelude.hashWithSalt` deploymentStyle
      `Prelude.hashWithSalt` blueGreenDeploymentConfiguration
      `Prelude.hashWithSalt` errorInformation
      `Prelude.hashWithSalt` relatedDeployments
      `Prelude.hashWithSalt` targetInstances
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` additionalDeploymentStatusInfo
      `Prelude.hashWithSalt` creator
      `Prelude.hashWithSalt` previousRevision
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` ignoreApplicationStopFailures
      `Prelude.hashWithSalt` instanceTerminationWaitTimeStarted
      `Prelude.hashWithSalt` deploymentConfigName
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DeploymentInfo where
  rnf DeploymentInfo' {..} =
    Prelude.rnf overrideAlarmConfiguration
      `Prelude.seq` Prelude.rnf loadBalancerInfo
      `Prelude.seq` Prelude.rnf deploymentGroupName
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf fileExistsBehavior
      `Prelude.seq` Prelude.rnf deploymentOverview
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf rollbackInfo
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf computePlatform
      `Prelude.seq` Prelude.rnf updateOutdatedInstancesOnly
      `Prelude.seq` Prelude.rnf completeTime
      `Prelude.seq` Prelude.rnf deploymentStatusMessages
      `Prelude.seq` Prelude.rnf autoRollbackConfiguration
      `Prelude.seq` Prelude.rnf deploymentStyle
      `Prelude.seq` Prelude.rnf
        blueGreenDeploymentConfiguration
      `Prelude.seq` Prelude.rnf errorInformation
      `Prelude.seq` Prelude.rnf relatedDeployments
      `Prelude.seq` Prelude.rnf targetInstances
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf
        additionalDeploymentStatusInfo
      `Prelude.seq` Prelude.rnf creator
      `Prelude.seq` Prelude.rnf
        previousRevision
      `Prelude.seq` Prelude.rnf
        startTime
      `Prelude.seq` Prelude.rnf
        ignoreApplicationStopFailures
      `Prelude.seq` Prelude.rnf
        instanceTerminationWaitTimeStarted
      `Prelude.seq` Prelude.rnf
        deploymentConfigName
      `Prelude.seq` Prelude.rnf
        applicationName
