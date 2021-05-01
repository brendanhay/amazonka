{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Lens
  ( -- * Operations

    -- ** BatchGetOnPremisesInstances
    batchGetOnPremisesInstances_instanceNames,
    batchGetOnPremisesInstancesResponse_instanceInfos,
    batchGetOnPremisesInstancesResponse_httpStatus,

    -- ** GetApplicationRevision
    getApplicationRevision_applicationName,
    getApplicationRevision_revision,
    getApplicationRevisionResponse_revisionInfo,
    getApplicationRevisionResponse_revision,
    getApplicationRevisionResponse_applicationName,
    getApplicationRevisionResponse_httpStatus,

    -- ** BatchGetDeploymentGroups
    batchGetDeploymentGroups_applicationName,
    batchGetDeploymentGroups_deploymentGroupNames,
    batchGetDeploymentGroupsResponse_deploymentGroupsInfo,
    batchGetDeploymentGroupsResponse_errorMessage,
    batchGetDeploymentGroupsResponse_httpStatus,

    -- ** CreateDeploymentConfig
    createDeploymentConfig_trafficRoutingConfig,
    createDeploymentConfig_minimumHealthyHosts,
    createDeploymentConfig_computePlatform,
    createDeploymentConfig_deploymentConfigName,
    createDeploymentConfigResponse_deploymentConfigId,
    createDeploymentConfigResponse_httpStatus,

    -- ** StopDeployment
    stopDeployment_autoRollbackEnabled,
    stopDeployment_deploymentId,
    stopDeploymentResponse_statusMessage,
    stopDeploymentResponse_status,
    stopDeploymentResponse_httpStatus,

    -- ** ListDeploymentTargets
    listDeploymentTargets_deploymentId,
    listDeploymentTargets_nextToken,
    listDeploymentTargets_targetFilters,
    listDeploymentTargetsResponse_targetIds,
    listDeploymentTargetsResponse_nextToken,
    listDeploymentTargetsResponse_httpStatus,

    -- ** CreateApplication
    createApplication_tags,
    createApplication_computePlatform,
    createApplication_applicationName,
    createApplicationResponse_applicationId,
    createApplicationResponse_httpStatus,

    -- ** AddTagsToOnPremisesInstances
    addTagsToOnPremisesInstances_tags,
    addTagsToOnPremisesInstances_instanceNames,

    -- ** GetDeploymentTarget
    getDeploymentTarget_deploymentId,
    getDeploymentTarget_targetId,
    getDeploymentTargetResponse_deploymentTarget,
    getDeploymentTargetResponse_httpStatus,

    -- ** DeleteResourcesByExternalId
    deleteResourcesByExternalId_externalId,
    deleteResourcesByExternalIdResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** BatchGetApplications
    batchGetApplications_applicationNames,
    batchGetApplicationsResponse_applicationsInfo,
    batchGetApplicationsResponse_httpStatus,

    -- ** BatchGetApplicationRevisions
    batchGetApplicationRevisions_applicationName,
    batchGetApplicationRevisions_revisions,
    batchGetApplicationRevisionsResponse_revisions,
    batchGetApplicationRevisionsResponse_errorMessage,
    batchGetApplicationRevisionsResponse_applicationName,
    batchGetApplicationRevisionsResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_deploymentGroupName,
    listDeployments_createTimeRange,
    listDeployments_includeOnlyStatuses,
    listDeployments_externalId,
    listDeployments_applicationName,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ContinueDeployment
    continueDeployment_deploymentId,
    continueDeployment_deploymentWaitType,

    -- ** GetDeploymentConfig
    getDeploymentConfig_deploymentConfigName,
    getDeploymentConfigResponse_deploymentConfigInfo,
    getDeploymentConfigResponse_httpStatus,

    -- ** DeleteDeploymentConfig
    deleteDeploymentConfig_deploymentConfigName,

    -- ** CreateDeploymentGroup
    createDeploymentGroup_onPremisesTagSet,
    createDeploymentGroup_deploymentConfigName,
    createDeploymentGroup_autoRollbackConfiguration,
    createDeploymentGroup_triggerConfigurations,
    createDeploymentGroup_ec2TagFilters,
    createDeploymentGroup_onPremisesInstanceTagFilters,
    createDeploymentGroup_loadBalancerInfo,
    createDeploymentGroup_ec2TagSet,
    createDeploymentGroup_blueGreenDeploymentConfiguration,
    createDeploymentGroup_tags,
    createDeploymentGroup_autoScalingGroups,
    createDeploymentGroup_deploymentStyle,
    createDeploymentGroup_alarmConfiguration,
    createDeploymentGroup_ecsServices,
    createDeploymentGroup_applicationName,
    createDeploymentGroup_deploymentGroupName,
    createDeploymentGroup_serviceRoleArn,
    createDeploymentGroupResponse_deploymentGroupId,
    createDeploymentGroupResponse_httpStatus,

    -- ** ListDeploymentConfigs
    listDeploymentConfigs_nextToken,
    listDeploymentConfigsResponse_nextToken,
    listDeploymentConfigsResponse_deploymentConfigsList,
    listDeploymentConfigsResponse_httpStatus,

    -- ** DeleteDeploymentGroup
    deleteDeploymentGroup_applicationName,
    deleteDeploymentGroup_deploymentGroupName,
    deleteDeploymentGroupResponse_hooksNotCleanedUp,
    deleteDeploymentGroupResponse_httpStatus,

    -- ** ListDeploymentGroups
    listDeploymentGroups_nextToken,
    listDeploymentGroups_applicationName,
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_httpStatus,

    -- ** ListOnPremisesInstances
    listOnPremisesInstances_nextToken,
    listOnPremisesInstances_tagFilters,
    listOnPremisesInstances_registrationStatus,
    listOnPremisesInstancesResponse_nextToken,
    listOnPremisesInstancesResponse_instanceNames,
    listOnPremisesInstancesResponse_httpStatus,

    -- ** UpdateDeploymentGroup
    updateDeploymentGroup_onPremisesTagSet,
    updateDeploymentGroup_serviceRoleArn,
    updateDeploymentGroup_deploymentConfigName,
    updateDeploymentGroup_autoRollbackConfiguration,
    updateDeploymentGroup_triggerConfigurations,
    updateDeploymentGroup_ec2TagFilters,
    updateDeploymentGroup_onPremisesInstanceTagFilters,
    updateDeploymentGroup_loadBalancerInfo,
    updateDeploymentGroup_ec2TagSet,
    updateDeploymentGroup_blueGreenDeploymentConfiguration,
    updateDeploymentGroup_autoScalingGroups,
    updateDeploymentGroup_deploymentStyle,
    updateDeploymentGroup_alarmConfiguration,
    updateDeploymentGroup_ecsServices,
    updateDeploymentGroup_newDeploymentGroupName,
    updateDeploymentGroup_applicationName,
    updateDeploymentGroup_currentDeploymentGroupName,
    updateDeploymentGroupResponse_hooksNotCleanedUp,
    updateDeploymentGroupResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_deploymentId,
    getDeploymentResponse_deploymentInfo,
    getDeploymentResponse_httpStatus,

    -- ** RegisterOnPremisesInstance
    registerOnPremisesInstance_iamUserArn,
    registerOnPremisesInstance_iamSessionArn,
    registerOnPremisesInstance_instanceName,

    -- ** RemoveTagsFromOnPremisesInstances
    removeTagsFromOnPremisesInstances_tags,
    removeTagsFromOnPremisesInstances_instanceNames,

    -- ** GetApplication
    getApplication_applicationName,
    getApplicationResponse_application,
    getApplicationResponse_httpStatus,

    -- ** BatchGetDeploymentTargets
    batchGetDeploymentTargets_targetIds,
    batchGetDeploymentTargets_deploymentId,
    batchGetDeploymentTargetsResponse_deploymentTargets,
    batchGetDeploymentTargetsResponse_httpStatus,

    -- ** ListGitHubAccountTokenNames
    listGitHubAccountTokenNames_nextToken,
    listGitHubAccountTokenNamesResponse_nextToken,
    listGitHubAccountTokenNamesResponse_tokenNameList,
    listGitHubAccountTokenNamesResponse_httpStatus,

    -- ** DeleteGitHubAccountToken
    deleteGitHubAccountToken_tokenName,
    deleteGitHubAccountTokenResponse_tokenName,
    deleteGitHubAccountTokenResponse_httpStatus,

    -- ** PutLifecycleEventHookExecutionStatus
    putLifecycleEventHookExecutionStatus_deploymentId,
    putLifecycleEventHookExecutionStatus_status,
    putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatusResponse_httpStatus,

    -- ** DeregisterOnPremisesInstance
    deregisterOnPremisesInstance_instanceName,

    -- ** DeleteApplication
    deleteApplication_applicationName,

    -- ** ListApplications
    listApplications_nextToken,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_newApplicationName,
    updateApplication_applicationName,

    -- ** RegisterApplicationRevision
    registerApplicationRevision_description,
    registerApplicationRevision_applicationName,
    registerApplicationRevision_revision,

    -- ** GetOnPremisesInstance
    getOnPremisesInstance_instanceName,
    getOnPremisesInstanceResponse_instanceInfo,
    getOnPremisesInstanceResponse_httpStatus,

    -- ** ListApplicationRevisions
    listApplicationRevisions_sortOrder,
    listApplicationRevisions_nextToken,
    listApplicationRevisions_s3Bucket,
    listApplicationRevisions_deployed,
    listApplicationRevisions_s3KeyPrefix,
    listApplicationRevisions_sortBy,
    listApplicationRevisions_applicationName,
    listApplicationRevisionsResponse_nextToken,
    listApplicationRevisionsResponse_revisions,
    listApplicationRevisionsResponse_httpStatus,

    -- ** BatchGetDeployments
    batchGetDeployments_deploymentIds,
    batchGetDeploymentsResponse_deploymentsInfo,
    batchGetDeploymentsResponse_httpStatus,

    -- ** GetDeploymentGroup
    getDeploymentGroup_applicationName,
    getDeploymentGroup_deploymentGroupName,
    getDeploymentGroupResponse_deploymentGroupInfo,
    getDeploymentGroupResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_deploymentConfigName,
    createDeployment_ignoreApplicationStopFailures,
    createDeployment_updateOutdatedInstancesOnly,
    createDeployment_autoRollbackConfiguration,
    createDeployment_deploymentGroupName,
    createDeployment_targetInstances,
    createDeployment_description,
    createDeployment_revision,
    createDeployment_fileExistsBehavior,
    createDeployment_applicationName,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** Alarm
    alarm_name,

    -- ** AlarmConfiguration
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_enabled,
    alarmConfiguration_alarms,

    -- ** AppSpecContent
    appSpecContent_content,
    appSpecContent_sha256,

    -- ** ApplicationInfo
    applicationInfo_applicationId,
    applicationInfo_linkedToGitHub,
    applicationInfo_gitHubAccountName,
    applicationInfo_createTime,
    applicationInfo_applicationName,
    applicationInfo_computePlatform,

    -- ** AutoRollbackConfiguration
    autoRollbackConfiguration_enabled,
    autoRollbackConfiguration_events,

    -- ** AutoScalingGroup
    autoScalingGroup_hook,
    autoScalingGroup_name,

    -- ** BlueGreenDeploymentConfiguration
    blueGreenDeploymentConfiguration_greenFleetProvisioningOption,
    blueGreenDeploymentConfiguration_deploymentReadyOption,
    blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess,

    -- ** BlueInstanceTerminationOption
    blueInstanceTerminationOption_action,
    blueInstanceTerminationOption_terminationWaitTimeInMinutes,

    -- ** CloudFormationTarget
    cloudFormationTarget_deploymentId,
    cloudFormationTarget_status,
    cloudFormationTarget_targetId,
    cloudFormationTarget_targetVersionWeight,
    cloudFormationTarget_resourceType,
    cloudFormationTarget_lifecycleEvents,
    cloudFormationTarget_lastUpdatedAt,

    -- ** DeploymentConfigInfo
    deploymentConfigInfo_deploymentConfigName,
    deploymentConfigInfo_deploymentConfigId,
    deploymentConfigInfo_createTime,
    deploymentConfigInfo_trafficRoutingConfig,
    deploymentConfigInfo_minimumHealthyHosts,
    deploymentConfigInfo_computePlatform,

    -- ** DeploymentGroupInfo
    deploymentGroupInfo_onPremisesTagSet,
    deploymentGroupInfo_serviceRoleArn,
    deploymentGroupInfo_deploymentConfigName,
    deploymentGroupInfo_autoRollbackConfiguration,
    deploymentGroupInfo_deploymentGroupName,
    deploymentGroupInfo_triggerConfigurations,
    deploymentGroupInfo_deploymentGroupId,
    deploymentGroupInfo_ec2TagFilters,
    deploymentGroupInfo_targetRevision,
    deploymentGroupInfo_lastSuccessfulDeployment,
    deploymentGroupInfo_lastAttemptedDeployment,
    deploymentGroupInfo_onPremisesInstanceTagFilters,
    deploymentGroupInfo_loadBalancerInfo,
    deploymentGroupInfo_ec2TagSet,
    deploymentGroupInfo_blueGreenDeploymentConfiguration,
    deploymentGroupInfo_autoScalingGroups,
    deploymentGroupInfo_deploymentStyle,
    deploymentGroupInfo_alarmConfiguration,
    deploymentGroupInfo_ecsServices,
    deploymentGroupInfo_applicationName,
    deploymentGroupInfo_computePlatform,

    -- ** DeploymentInfo
    deploymentInfo_deploymentId,
    deploymentInfo_status,
    deploymentInfo_deploymentConfigName,
    deploymentInfo_ignoreApplicationStopFailures,
    deploymentInfo_updateOutdatedInstancesOnly,
    deploymentInfo_autoRollbackConfiguration,
    deploymentInfo_deploymentGroupName,
    deploymentInfo_targetInstances,
    deploymentInfo_startTime,
    deploymentInfo_instanceTerminationWaitTimeStarted,
    deploymentInfo_previousRevision,
    deploymentInfo_loadBalancerInfo,
    deploymentInfo_completeTime,
    deploymentInfo_errorInformation,
    deploymentInfo_blueGreenDeploymentConfiguration,
    deploymentInfo_creator,
    deploymentInfo_createTime,
    deploymentInfo_description,
    deploymentInfo_deploymentStyle,
    deploymentInfo_revision,
    deploymentInfo_rollbackInfo,
    deploymentInfo_externalId,
    deploymentInfo_applicationName,
    deploymentInfo_deploymentStatusMessages,
    deploymentInfo_fileExistsBehavior,
    deploymentInfo_additionalDeploymentStatusInfo,
    deploymentInfo_deploymentOverview,
    deploymentInfo_computePlatform,

    -- ** DeploymentOverview
    deploymentOverview_succeeded,
    deploymentOverview_ready,
    deploymentOverview_pending,
    deploymentOverview_failed,
    deploymentOverview_skipped,
    deploymentOverview_inProgress,

    -- ** DeploymentReadyOption
    deploymentReadyOption_waitTimeInMinutes,
    deploymentReadyOption_actionOnTimeout,

    -- ** DeploymentStyle
    deploymentStyle_deploymentType,
    deploymentStyle_deploymentOption,

    -- ** DeploymentTarget
    deploymentTarget_ecsTarget,
    deploymentTarget_lambdaTarget,
    deploymentTarget_cloudFormationTarget,
    deploymentTarget_instanceTarget,
    deploymentTarget_deploymentTargetType,

    -- ** Diagnostics
    diagnostics_logTail,
    diagnostics_message,
    diagnostics_scriptName,
    diagnostics_errorCode,

    -- ** EC2TagFilter
    eC2TagFilter_key,
    eC2TagFilter_value,
    eC2TagFilter_type,

    -- ** EC2TagSet
    eC2TagSet_ec2TagSetList,

    -- ** ECSService
    eCSService_serviceName,
    eCSService_clusterName,

    -- ** ECSTarget
    eCSTarget_deploymentId,
    eCSTarget_status,
    eCSTarget_targetId,
    eCSTarget_taskSetsInfo,
    eCSTarget_targetArn,
    eCSTarget_lifecycleEvents,
    eCSTarget_lastUpdatedAt,

    -- ** ECSTaskSet
    eCSTaskSet_status,
    eCSTaskSet_runningCount,
    eCSTaskSet_desiredCount,
    eCSTaskSet_pendingCount,
    eCSTaskSet_taskSetLabel,
    eCSTaskSet_targetGroup,
    eCSTaskSet_trafficWeight,
    eCSTaskSet_identifer,

    -- ** ELBInfo
    eLBInfo_name,

    -- ** ErrorInformation
    errorInformation_message,
    errorInformation_code,

    -- ** GenericRevisionInfo
    genericRevisionInfo_registerTime,
    genericRevisionInfo_deploymentGroups,
    genericRevisionInfo_description,
    genericRevisionInfo_firstUsedTime,
    genericRevisionInfo_lastUsedTime,

    -- ** GitHubLocation
    gitHubLocation_commitId,
    gitHubLocation_repository,

    -- ** GreenFleetProvisioningOption
    greenFleetProvisioningOption_action,

    -- ** InstanceInfo
    instanceInfo_registerTime,
    instanceInfo_iamUserArn,
    instanceInfo_instanceName,
    instanceInfo_instanceArn,
    instanceInfo_tags,
    instanceInfo_iamSessionArn,
    instanceInfo_deregisterTime,

    -- ** InstanceTarget
    instanceTarget_deploymentId,
    instanceTarget_status,
    instanceTarget_targetId,
    instanceTarget_instanceLabel,
    instanceTarget_targetArn,
    instanceTarget_lifecycleEvents,
    instanceTarget_lastUpdatedAt,

    -- ** LambdaFunctionInfo
    lambdaFunctionInfo_functionAlias,
    lambdaFunctionInfo_targetVersion,
    lambdaFunctionInfo_targetVersionWeight,
    lambdaFunctionInfo_currentVersion,
    lambdaFunctionInfo_functionName,

    -- ** LambdaTarget
    lambdaTarget_deploymentId,
    lambdaTarget_status,
    lambdaTarget_targetId,
    lambdaTarget_targetArn,
    lambdaTarget_lifecycleEvents,
    lambdaTarget_lambdaFunctionInfo,
    lambdaTarget_lastUpdatedAt,

    -- ** LastDeploymentInfo
    lastDeploymentInfo_deploymentId,
    lastDeploymentInfo_status,
    lastDeploymentInfo_endTime,
    lastDeploymentInfo_createTime,

    -- ** LifecycleEvent
    lifecycleEvent_status,
    lifecycleEvent_diagnostics,
    lifecycleEvent_startTime,
    lifecycleEvent_endTime,
    lifecycleEvent_lifecycleEventName,

    -- ** LoadBalancerInfo
    loadBalancerInfo_targetGroupPairInfoList,
    loadBalancerInfo_elbInfoList,
    loadBalancerInfo_targetGroupInfoList,

    -- ** MinimumHealthyHosts
    minimumHealthyHosts_value,
    minimumHealthyHosts_type,

    -- ** OnPremisesTagSet
    onPremisesTagSet_onPremisesTagSetList,

    -- ** RawString
    rawString_content,
    rawString_sha256,

    -- ** RevisionInfo
    revisionInfo_genericRevisionInfo,
    revisionInfo_revisionLocation,

    -- ** RevisionLocation
    revisionLocation_revisionType,
    revisionLocation_s3Location,
    revisionLocation_appSpecContent,
    revisionLocation_gitHubLocation,
    revisionLocation_string,

    -- ** RollbackInfo
    rollbackInfo_rollbackMessage,
    rollbackInfo_rollbackTriggeringDeploymentId,
    rollbackInfo_rollbackDeploymentId,

    -- ** S3Location
    s3Location_eTag,
    s3Location_key,
    s3Location_bundleType,
    s3Location_version,
    s3Location_bucket,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagFilter
    tagFilter_key,
    tagFilter_value,
    tagFilter_type,

    -- ** TargetGroupInfo
    targetGroupInfo_name,

    -- ** TargetGroupPairInfo
    targetGroupPairInfo_targetGroups,
    targetGroupPairInfo_prodTrafficRoute,
    targetGroupPairInfo_testTrafficRoute,

    -- ** TargetInstances
    targetInstances_tagFilters,
    targetInstances_ec2TagSet,
    targetInstances_autoScalingGroups,

    -- ** TimeBasedCanary
    timeBasedCanary_canaryInterval,
    timeBasedCanary_canaryPercentage,

    -- ** TimeBasedLinear
    timeBasedLinear_linearInterval,
    timeBasedLinear_linearPercentage,

    -- ** TimeRange
    timeRange_end,
    timeRange_start,

    -- ** TrafficRoute
    trafficRoute_listenerArns,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_timeBasedLinear,
    trafficRoutingConfig_type,
    trafficRoutingConfig_timeBasedCanary,

    -- ** TriggerConfig
    triggerConfig_triggerEvents,
    triggerConfig_triggerName,
    triggerConfig_triggerTargetArn,
  )
where

import Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
import Network.AWS.CodeDeploy.BatchGetApplicationRevisions
import Network.AWS.CodeDeploy.BatchGetApplications
import Network.AWS.CodeDeploy.BatchGetDeploymentGroups
import Network.AWS.CodeDeploy.BatchGetDeploymentTargets
import Network.AWS.CodeDeploy.BatchGetDeployments
import Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
import Network.AWS.CodeDeploy.ContinueDeployment
import Network.AWS.CodeDeploy.CreateApplication
import Network.AWS.CodeDeploy.CreateDeployment
import Network.AWS.CodeDeploy.CreateDeploymentConfig
import Network.AWS.CodeDeploy.CreateDeploymentGroup
import Network.AWS.CodeDeploy.DeleteApplication
import Network.AWS.CodeDeploy.DeleteDeploymentConfig
import Network.AWS.CodeDeploy.DeleteDeploymentGroup
import Network.AWS.CodeDeploy.DeleteGitHubAccountToken
import Network.AWS.CodeDeploy.DeleteResourcesByExternalId
import Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
import Network.AWS.CodeDeploy.GetApplication
import Network.AWS.CodeDeploy.GetApplicationRevision
import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.GetDeploymentConfig
import Network.AWS.CodeDeploy.GetDeploymentGroup
import Network.AWS.CodeDeploy.GetDeploymentTarget
import Network.AWS.CodeDeploy.GetOnPremisesInstance
import Network.AWS.CodeDeploy.ListApplicationRevisions
import Network.AWS.CodeDeploy.ListApplications
import Network.AWS.CodeDeploy.ListDeploymentConfigs
import Network.AWS.CodeDeploy.ListDeploymentGroups
import Network.AWS.CodeDeploy.ListDeploymentTargets
import Network.AWS.CodeDeploy.ListDeployments
import Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
import Network.AWS.CodeDeploy.ListOnPremisesInstances
import Network.AWS.CodeDeploy.ListTagsForResource
import Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
import Network.AWS.CodeDeploy.RegisterApplicationRevision
import Network.AWS.CodeDeploy.RegisterOnPremisesInstance
import Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
import Network.AWS.CodeDeploy.StopDeployment
import Network.AWS.CodeDeploy.TagResource
import Network.AWS.CodeDeploy.Types.Alarm
import Network.AWS.CodeDeploy.Types.AlarmConfiguration
import Network.AWS.CodeDeploy.Types.AppSpecContent
import Network.AWS.CodeDeploy.Types.ApplicationInfo
import Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
import Network.AWS.CodeDeploy.Types.AutoScalingGroup
import Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
import Network.AWS.CodeDeploy.Types.CloudFormationTarget
import Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
import Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
import Network.AWS.CodeDeploy.Types.DeploymentInfo
import Network.AWS.CodeDeploy.Types.DeploymentOverview
import Network.AWS.CodeDeploy.Types.DeploymentReadyOption
import Network.AWS.CodeDeploy.Types.DeploymentStyle
import Network.AWS.CodeDeploy.Types.DeploymentTarget
import Network.AWS.CodeDeploy.Types.Diagnostics
import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import Network.AWS.CodeDeploy.Types.ECSService
import Network.AWS.CodeDeploy.Types.ECSTarget
import Network.AWS.CodeDeploy.Types.ECSTaskSet
import Network.AWS.CodeDeploy.Types.ELBInfo
import Network.AWS.CodeDeploy.Types.ErrorInformation
import Network.AWS.CodeDeploy.Types.GenericRevisionInfo
import Network.AWS.CodeDeploy.Types.GitHubLocation
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
import Network.AWS.CodeDeploy.Types.InstanceInfo
import Network.AWS.CodeDeploy.Types.InstanceTarget
import Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
import Network.AWS.CodeDeploy.Types.LambdaTarget
import Network.AWS.CodeDeploy.Types.LastDeploymentInfo
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.LoadBalancerInfo
import Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
import Network.AWS.CodeDeploy.Types.OnPremisesTagSet
import Network.AWS.CodeDeploy.Types.RawString
import Network.AWS.CodeDeploy.Types.RevisionInfo
import Network.AWS.CodeDeploy.Types.RevisionLocation
import Network.AWS.CodeDeploy.Types.RollbackInfo
import Network.AWS.CodeDeploy.Types.S3Location
import Network.AWS.CodeDeploy.Types.Tag
import Network.AWS.CodeDeploy.Types.TagFilter
import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
import Network.AWS.CodeDeploy.Types.TargetInstances
import Network.AWS.CodeDeploy.Types.TimeBasedCanary
import Network.AWS.CodeDeploy.Types.TimeBasedLinear
import Network.AWS.CodeDeploy.Types.TimeRange
import Network.AWS.CodeDeploy.Types.TrafficRoute
import Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
import Network.AWS.CodeDeploy.Types.TriggerConfig
import Network.AWS.CodeDeploy.UntagResource
import Network.AWS.CodeDeploy.UpdateApplication
import Network.AWS.CodeDeploy.UpdateDeploymentGroup
