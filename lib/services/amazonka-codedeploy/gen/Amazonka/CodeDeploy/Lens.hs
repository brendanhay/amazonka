{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Lens
  ( -- * Operations

    -- ** AddTagsToOnPremisesInstances
    addTagsToOnPremisesInstances_tags,
    addTagsToOnPremisesInstances_instanceNames,

    -- ** BatchGetApplicationRevisions
    batchGetApplicationRevisions_applicationName,
    batchGetApplicationRevisions_revisions,
    batchGetApplicationRevisionsResponse_applicationName,
    batchGetApplicationRevisionsResponse_errorMessage,
    batchGetApplicationRevisionsResponse_revisions,
    batchGetApplicationRevisionsResponse_httpStatus,

    -- ** BatchGetApplications
    batchGetApplications_applicationNames,
    batchGetApplicationsResponse_applicationsInfo,
    batchGetApplicationsResponse_httpStatus,

    -- ** BatchGetDeploymentGroups
    batchGetDeploymentGroups_applicationName,
    batchGetDeploymentGroups_deploymentGroupNames,
    batchGetDeploymentGroupsResponse_deploymentGroupsInfo,
    batchGetDeploymentGroupsResponse_errorMessage,
    batchGetDeploymentGroupsResponse_httpStatus,

    -- ** BatchGetDeploymentTargets
    batchGetDeploymentTargets_deploymentId,
    batchGetDeploymentTargets_targetIds,
    batchGetDeploymentTargetsResponse_deploymentTargets,
    batchGetDeploymentTargetsResponse_httpStatus,

    -- ** BatchGetDeployments
    batchGetDeployments_deploymentIds,
    batchGetDeploymentsResponse_deploymentsInfo,
    batchGetDeploymentsResponse_httpStatus,

    -- ** BatchGetOnPremisesInstances
    batchGetOnPremisesInstances_instanceNames,
    batchGetOnPremisesInstancesResponse_instanceInfos,
    batchGetOnPremisesInstancesResponse_httpStatus,

    -- ** ContinueDeployment
    continueDeployment_deploymentId,
    continueDeployment_deploymentWaitType,

    -- ** CreateApplication
    createApplication_computePlatform,
    createApplication_tags,
    createApplication_applicationName,
    createApplicationResponse_applicationId,
    createApplicationResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_autoRollbackConfiguration,
    createDeployment_deploymentConfigName,
    createDeployment_deploymentGroupName,
    createDeployment_description,
    createDeployment_fileExistsBehavior,
    createDeployment_ignoreApplicationStopFailures,
    createDeployment_overrideAlarmConfiguration,
    createDeployment_revision,
    createDeployment_targetInstances,
    createDeployment_updateOutdatedInstancesOnly,
    createDeployment_applicationName,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** CreateDeploymentConfig
    createDeploymentConfig_computePlatform,
    createDeploymentConfig_minimumHealthyHosts,
    createDeploymentConfig_trafficRoutingConfig,
    createDeploymentConfig_deploymentConfigName,
    createDeploymentConfigResponse_deploymentConfigId,
    createDeploymentConfigResponse_httpStatus,

    -- ** CreateDeploymentGroup
    createDeploymentGroup_alarmConfiguration,
    createDeploymentGroup_autoRollbackConfiguration,
    createDeploymentGroup_autoScalingGroups,
    createDeploymentGroup_blueGreenDeploymentConfiguration,
    createDeploymentGroup_deploymentConfigName,
    createDeploymentGroup_deploymentStyle,
    createDeploymentGroup_ec2TagFilters,
    createDeploymentGroup_ec2TagSet,
    createDeploymentGroup_ecsServices,
    createDeploymentGroup_loadBalancerInfo,
    createDeploymentGroup_onPremisesInstanceTagFilters,
    createDeploymentGroup_onPremisesTagSet,
    createDeploymentGroup_outdatedInstancesStrategy,
    createDeploymentGroup_tags,
    createDeploymentGroup_triggerConfigurations,
    createDeploymentGroup_applicationName,
    createDeploymentGroup_deploymentGroupName,
    createDeploymentGroup_serviceRoleArn,
    createDeploymentGroupResponse_deploymentGroupId,
    createDeploymentGroupResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationName,

    -- ** DeleteDeploymentConfig
    deleteDeploymentConfig_deploymentConfigName,

    -- ** DeleteDeploymentGroup
    deleteDeploymentGroup_applicationName,
    deleteDeploymentGroup_deploymentGroupName,
    deleteDeploymentGroupResponse_hooksNotCleanedUp,
    deleteDeploymentGroupResponse_httpStatus,

    -- ** DeleteGitHubAccountToken
    deleteGitHubAccountToken_tokenName,
    deleteGitHubAccountTokenResponse_tokenName,
    deleteGitHubAccountTokenResponse_httpStatus,

    -- ** DeleteResourcesByExternalId
    deleteResourcesByExternalId_externalId,
    deleteResourcesByExternalIdResponse_httpStatus,

    -- ** DeregisterOnPremisesInstance
    deregisterOnPremisesInstance_instanceName,

    -- ** GetApplication
    getApplication_applicationName,
    getApplicationResponse_application,
    getApplicationResponse_httpStatus,

    -- ** GetApplicationRevision
    getApplicationRevision_applicationName,
    getApplicationRevision_revision,
    getApplicationRevisionResponse_applicationName,
    getApplicationRevisionResponse_revision,
    getApplicationRevisionResponse_revisionInfo,
    getApplicationRevisionResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_deploymentId,
    getDeploymentResponse_deploymentInfo,
    getDeploymentResponse_httpStatus,

    -- ** GetDeploymentConfig
    getDeploymentConfig_deploymentConfigName,
    getDeploymentConfigResponse_deploymentConfigInfo,
    getDeploymentConfigResponse_httpStatus,

    -- ** GetDeploymentGroup
    getDeploymentGroup_applicationName,
    getDeploymentGroup_deploymentGroupName,
    getDeploymentGroupResponse_deploymentGroupInfo,
    getDeploymentGroupResponse_httpStatus,

    -- ** GetDeploymentTarget
    getDeploymentTarget_deploymentId,
    getDeploymentTarget_targetId,
    getDeploymentTargetResponse_deploymentTarget,
    getDeploymentTargetResponse_httpStatus,

    -- ** GetOnPremisesInstance
    getOnPremisesInstance_instanceName,
    getOnPremisesInstanceResponse_instanceInfo,
    getOnPremisesInstanceResponse_httpStatus,

    -- ** ListApplicationRevisions
    listApplicationRevisions_deployed,
    listApplicationRevisions_nextToken,
    listApplicationRevisions_s3Bucket,
    listApplicationRevisions_s3KeyPrefix,
    listApplicationRevisions_sortBy,
    listApplicationRevisions_sortOrder,
    listApplicationRevisions_applicationName,
    listApplicationRevisionsResponse_nextToken,
    listApplicationRevisionsResponse_revisions,
    listApplicationRevisionsResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListDeploymentConfigs
    listDeploymentConfigs_nextToken,
    listDeploymentConfigsResponse_deploymentConfigsList,
    listDeploymentConfigsResponse_nextToken,
    listDeploymentConfigsResponse_httpStatus,

    -- ** ListDeploymentGroups
    listDeploymentGroups_nextToken,
    listDeploymentGroups_applicationName,
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_httpStatus,

    -- ** ListDeploymentTargets
    listDeploymentTargets_deploymentId,
    listDeploymentTargets_nextToken,
    listDeploymentTargets_targetFilters,
    listDeploymentTargetsResponse_nextToken,
    listDeploymentTargetsResponse_targetIds,
    listDeploymentTargetsResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_applicationName,
    listDeployments_createTimeRange,
    listDeployments_deploymentGroupName,
    listDeployments_externalId,
    listDeployments_includeOnlyStatuses,
    listDeployments_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,

    -- ** ListGitHubAccountTokenNames
    listGitHubAccountTokenNames_nextToken,
    listGitHubAccountTokenNamesResponse_nextToken,
    listGitHubAccountTokenNamesResponse_tokenNameList,
    listGitHubAccountTokenNamesResponse_httpStatus,

    -- ** ListOnPremisesInstances
    listOnPremisesInstances_nextToken,
    listOnPremisesInstances_registrationStatus,
    listOnPremisesInstances_tagFilters,
    listOnPremisesInstancesResponse_instanceNames,
    listOnPremisesInstancesResponse_nextToken,
    listOnPremisesInstancesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutLifecycleEventHookExecutionStatus
    putLifecycleEventHookExecutionStatus_deploymentId,
    putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatus_status,
    putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatusResponse_httpStatus,

    -- ** RegisterApplicationRevision
    registerApplicationRevision_description,
    registerApplicationRevision_applicationName,
    registerApplicationRevision_revision,

    -- ** RegisterOnPremisesInstance
    registerOnPremisesInstance_iamSessionArn,
    registerOnPremisesInstance_iamUserArn,
    registerOnPremisesInstance_instanceName,

    -- ** RemoveTagsFromOnPremisesInstances
    removeTagsFromOnPremisesInstances_tags,
    removeTagsFromOnPremisesInstances_instanceNames,

    -- ** StopDeployment
    stopDeployment_autoRollbackEnabled,
    stopDeployment_deploymentId,
    stopDeploymentResponse_status,
    stopDeploymentResponse_statusMessage,
    stopDeploymentResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_applicationName,
    updateApplication_newApplicationName,

    -- ** UpdateDeploymentGroup
    updateDeploymentGroup_alarmConfiguration,
    updateDeploymentGroup_autoRollbackConfiguration,
    updateDeploymentGroup_autoScalingGroups,
    updateDeploymentGroup_blueGreenDeploymentConfiguration,
    updateDeploymentGroup_deploymentConfigName,
    updateDeploymentGroup_deploymentStyle,
    updateDeploymentGroup_ec2TagFilters,
    updateDeploymentGroup_ec2TagSet,
    updateDeploymentGroup_ecsServices,
    updateDeploymentGroup_loadBalancerInfo,
    updateDeploymentGroup_newDeploymentGroupName,
    updateDeploymentGroup_onPremisesInstanceTagFilters,
    updateDeploymentGroup_onPremisesTagSet,
    updateDeploymentGroup_outdatedInstancesStrategy,
    updateDeploymentGroup_serviceRoleArn,
    updateDeploymentGroup_triggerConfigurations,
    updateDeploymentGroup_applicationName,
    updateDeploymentGroup_currentDeploymentGroupName,
    updateDeploymentGroupResponse_hooksNotCleanedUp,
    updateDeploymentGroupResponse_httpStatus,

    -- * Types

    -- ** Alarm
    alarm_name,

    -- ** AlarmConfiguration
    alarmConfiguration_alarms,
    alarmConfiguration_enabled,
    alarmConfiguration_ignorePollAlarmFailure,

    -- ** AppSpecContent
    appSpecContent_content,
    appSpecContent_sha256,

    -- ** ApplicationInfo
    applicationInfo_applicationId,
    applicationInfo_applicationName,
    applicationInfo_computePlatform,
    applicationInfo_createTime,
    applicationInfo_gitHubAccountName,
    applicationInfo_linkedToGitHub,

    -- ** AutoRollbackConfiguration
    autoRollbackConfiguration_enabled,
    autoRollbackConfiguration_events,

    -- ** AutoScalingGroup
    autoScalingGroup_hook,
    autoScalingGroup_name,

    -- ** BlueGreenDeploymentConfiguration
    blueGreenDeploymentConfiguration_deploymentReadyOption,
    blueGreenDeploymentConfiguration_greenFleetProvisioningOption,
    blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess,

    -- ** BlueInstanceTerminationOption
    blueInstanceTerminationOption_action,
    blueInstanceTerminationOption_terminationWaitTimeInMinutes,

    -- ** CloudFormationTarget
    cloudFormationTarget_deploymentId,
    cloudFormationTarget_lastUpdatedAt,
    cloudFormationTarget_lifecycleEvents,
    cloudFormationTarget_resourceType,
    cloudFormationTarget_status,
    cloudFormationTarget_targetId,
    cloudFormationTarget_targetVersionWeight,

    -- ** DeploymentConfigInfo
    deploymentConfigInfo_computePlatform,
    deploymentConfigInfo_createTime,
    deploymentConfigInfo_deploymentConfigId,
    deploymentConfigInfo_deploymentConfigName,
    deploymentConfigInfo_minimumHealthyHosts,
    deploymentConfigInfo_trafficRoutingConfig,

    -- ** DeploymentGroupInfo
    deploymentGroupInfo_alarmConfiguration,
    deploymentGroupInfo_applicationName,
    deploymentGroupInfo_autoRollbackConfiguration,
    deploymentGroupInfo_autoScalingGroups,
    deploymentGroupInfo_blueGreenDeploymentConfiguration,
    deploymentGroupInfo_computePlatform,
    deploymentGroupInfo_deploymentConfigName,
    deploymentGroupInfo_deploymentGroupId,
    deploymentGroupInfo_deploymentGroupName,
    deploymentGroupInfo_deploymentStyle,
    deploymentGroupInfo_ec2TagFilters,
    deploymentGroupInfo_ec2TagSet,
    deploymentGroupInfo_ecsServices,
    deploymentGroupInfo_lastAttemptedDeployment,
    deploymentGroupInfo_lastSuccessfulDeployment,
    deploymentGroupInfo_loadBalancerInfo,
    deploymentGroupInfo_onPremisesInstanceTagFilters,
    deploymentGroupInfo_onPremisesTagSet,
    deploymentGroupInfo_outdatedInstancesStrategy,
    deploymentGroupInfo_serviceRoleArn,
    deploymentGroupInfo_targetRevision,
    deploymentGroupInfo_triggerConfigurations,

    -- ** DeploymentInfo
    deploymentInfo_additionalDeploymentStatusInfo,
    deploymentInfo_applicationName,
    deploymentInfo_autoRollbackConfiguration,
    deploymentInfo_blueGreenDeploymentConfiguration,
    deploymentInfo_completeTime,
    deploymentInfo_computePlatform,
    deploymentInfo_createTime,
    deploymentInfo_creator,
    deploymentInfo_deploymentConfigName,
    deploymentInfo_deploymentGroupName,
    deploymentInfo_deploymentId,
    deploymentInfo_deploymentOverview,
    deploymentInfo_deploymentStatusMessages,
    deploymentInfo_deploymentStyle,
    deploymentInfo_description,
    deploymentInfo_errorInformation,
    deploymentInfo_externalId,
    deploymentInfo_fileExistsBehavior,
    deploymentInfo_ignoreApplicationStopFailures,
    deploymentInfo_instanceTerminationWaitTimeStarted,
    deploymentInfo_loadBalancerInfo,
    deploymentInfo_overrideAlarmConfiguration,
    deploymentInfo_previousRevision,
    deploymentInfo_relatedDeployments,
    deploymentInfo_revision,
    deploymentInfo_rollbackInfo,
    deploymentInfo_startTime,
    deploymentInfo_status,
    deploymentInfo_targetInstances,
    deploymentInfo_updateOutdatedInstancesOnly,

    -- ** DeploymentOverview
    deploymentOverview_failed,
    deploymentOverview_inProgress,
    deploymentOverview_pending,
    deploymentOverview_ready,
    deploymentOverview_skipped,
    deploymentOverview_succeeded,

    -- ** DeploymentReadyOption
    deploymentReadyOption_actionOnTimeout,
    deploymentReadyOption_waitTimeInMinutes,

    -- ** DeploymentStyle
    deploymentStyle_deploymentOption,
    deploymentStyle_deploymentType,

    -- ** DeploymentTarget
    deploymentTarget_cloudFormationTarget,
    deploymentTarget_deploymentTargetType,
    deploymentTarget_ecsTarget,
    deploymentTarget_instanceTarget,
    deploymentTarget_lambdaTarget,

    -- ** Diagnostics
    diagnostics_errorCode,
    diagnostics_logTail,
    diagnostics_message,
    diagnostics_scriptName,

    -- ** EC2TagFilter
    eC2TagFilter_key,
    eC2TagFilter_type,
    eC2TagFilter_value,

    -- ** EC2TagSet
    eC2TagSet_ec2TagSetList,

    -- ** ECSService
    eCSService_clusterName,
    eCSService_serviceName,

    -- ** ECSTarget
    eCSTarget_deploymentId,
    eCSTarget_lastUpdatedAt,
    eCSTarget_lifecycleEvents,
    eCSTarget_status,
    eCSTarget_targetArn,
    eCSTarget_targetId,
    eCSTarget_taskSetsInfo,

    -- ** ECSTaskSet
    eCSTaskSet_desiredCount,
    eCSTaskSet_identifer,
    eCSTaskSet_pendingCount,
    eCSTaskSet_runningCount,
    eCSTaskSet_status,
    eCSTaskSet_targetGroup,
    eCSTaskSet_taskSetLabel,
    eCSTaskSet_trafficWeight,

    -- ** ELBInfo
    eLBInfo_name,

    -- ** ErrorInformation
    errorInformation_code,
    errorInformation_message,

    -- ** GenericRevisionInfo
    genericRevisionInfo_deploymentGroups,
    genericRevisionInfo_description,
    genericRevisionInfo_firstUsedTime,
    genericRevisionInfo_lastUsedTime,
    genericRevisionInfo_registerTime,

    -- ** GitHubLocation
    gitHubLocation_commitId,
    gitHubLocation_repository,

    -- ** GreenFleetProvisioningOption
    greenFleetProvisioningOption_action,

    -- ** InstanceInfo
    instanceInfo_deregisterTime,
    instanceInfo_iamSessionArn,
    instanceInfo_iamUserArn,
    instanceInfo_instanceArn,
    instanceInfo_instanceName,
    instanceInfo_registerTime,
    instanceInfo_tags,

    -- ** InstanceTarget
    instanceTarget_deploymentId,
    instanceTarget_instanceLabel,
    instanceTarget_lastUpdatedAt,
    instanceTarget_lifecycleEvents,
    instanceTarget_status,
    instanceTarget_targetArn,
    instanceTarget_targetId,

    -- ** LambdaFunctionInfo
    lambdaFunctionInfo_currentVersion,
    lambdaFunctionInfo_functionAlias,
    lambdaFunctionInfo_functionName,
    lambdaFunctionInfo_targetVersion,
    lambdaFunctionInfo_targetVersionWeight,

    -- ** LambdaTarget
    lambdaTarget_deploymentId,
    lambdaTarget_lambdaFunctionInfo,
    lambdaTarget_lastUpdatedAt,
    lambdaTarget_lifecycleEvents,
    lambdaTarget_status,
    lambdaTarget_targetArn,
    lambdaTarget_targetId,

    -- ** LastDeploymentInfo
    lastDeploymentInfo_createTime,
    lastDeploymentInfo_deploymentId,
    lastDeploymentInfo_endTime,
    lastDeploymentInfo_status,

    -- ** LifecycleEvent
    lifecycleEvent_diagnostics,
    lifecycleEvent_endTime,
    lifecycleEvent_lifecycleEventName,
    lifecycleEvent_startTime,
    lifecycleEvent_status,

    -- ** LoadBalancerInfo
    loadBalancerInfo_elbInfoList,
    loadBalancerInfo_targetGroupInfoList,
    loadBalancerInfo_targetGroupPairInfoList,

    -- ** MinimumHealthyHosts
    minimumHealthyHosts_type,
    minimumHealthyHosts_value,

    -- ** OnPremisesTagSet
    onPremisesTagSet_onPremisesTagSetList,

    -- ** RawString
    rawString_content,
    rawString_sha256,

    -- ** RelatedDeployments
    relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds,
    relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId,

    -- ** RevisionInfo
    revisionInfo_genericRevisionInfo,
    revisionInfo_revisionLocation,

    -- ** RevisionLocation
    revisionLocation_appSpecContent,
    revisionLocation_gitHubLocation,
    revisionLocation_revisionType,
    revisionLocation_s3Location,
    revisionLocation_string,

    -- ** RollbackInfo
    rollbackInfo_rollbackDeploymentId,
    rollbackInfo_rollbackMessage,
    rollbackInfo_rollbackTriggeringDeploymentId,

    -- ** S3Location
    s3Location_bucket,
    s3Location_bundleType,
    s3Location_eTag,
    s3Location_key,
    s3Location_version,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagFilter
    tagFilter_key,
    tagFilter_type,
    tagFilter_value,

    -- ** TargetGroupInfo
    targetGroupInfo_name,

    -- ** TargetGroupPairInfo
    targetGroupPairInfo_prodTrafficRoute,
    targetGroupPairInfo_targetGroups,
    targetGroupPairInfo_testTrafficRoute,

    -- ** TargetInstances
    targetInstances_autoScalingGroups,
    targetInstances_ec2TagSet,
    targetInstances_tagFilters,

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
    trafficRoutingConfig_timeBasedCanary,
    trafficRoutingConfig_timeBasedLinear,
    trafficRoutingConfig_type,

    -- ** TriggerConfig
    triggerConfig_triggerEvents,
    triggerConfig_triggerName,
    triggerConfig_triggerTargetArn,
  )
where

import Amazonka.CodeDeploy.AddTagsToOnPremisesInstances
import Amazonka.CodeDeploy.BatchGetApplicationRevisions
import Amazonka.CodeDeploy.BatchGetApplications
import Amazonka.CodeDeploy.BatchGetDeploymentGroups
import Amazonka.CodeDeploy.BatchGetDeploymentTargets
import Amazonka.CodeDeploy.BatchGetDeployments
import Amazonka.CodeDeploy.BatchGetOnPremisesInstances
import Amazonka.CodeDeploy.ContinueDeployment
import Amazonka.CodeDeploy.CreateApplication
import Amazonka.CodeDeploy.CreateDeployment
import Amazonka.CodeDeploy.CreateDeploymentConfig
import Amazonka.CodeDeploy.CreateDeploymentGroup
import Amazonka.CodeDeploy.DeleteApplication
import Amazonka.CodeDeploy.DeleteDeploymentConfig
import Amazonka.CodeDeploy.DeleteDeploymentGroup
import Amazonka.CodeDeploy.DeleteGitHubAccountToken
import Amazonka.CodeDeploy.DeleteResourcesByExternalId
import Amazonka.CodeDeploy.DeregisterOnPremisesInstance
import Amazonka.CodeDeploy.GetApplication
import Amazonka.CodeDeploy.GetApplicationRevision
import Amazonka.CodeDeploy.GetDeployment
import Amazonka.CodeDeploy.GetDeploymentConfig
import Amazonka.CodeDeploy.GetDeploymentGroup
import Amazonka.CodeDeploy.GetDeploymentTarget
import Amazonka.CodeDeploy.GetOnPremisesInstance
import Amazonka.CodeDeploy.ListApplicationRevisions
import Amazonka.CodeDeploy.ListApplications
import Amazonka.CodeDeploy.ListDeploymentConfigs
import Amazonka.CodeDeploy.ListDeploymentGroups
import Amazonka.CodeDeploy.ListDeploymentTargets
import Amazonka.CodeDeploy.ListDeployments
import Amazonka.CodeDeploy.ListGitHubAccountTokenNames
import Amazonka.CodeDeploy.ListOnPremisesInstances
import Amazonka.CodeDeploy.ListTagsForResource
import Amazonka.CodeDeploy.PutLifecycleEventHookExecutionStatus
import Amazonka.CodeDeploy.RegisterApplicationRevision
import Amazonka.CodeDeploy.RegisterOnPremisesInstance
import Amazonka.CodeDeploy.RemoveTagsFromOnPremisesInstances
import Amazonka.CodeDeploy.StopDeployment
import Amazonka.CodeDeploy.TagResource
import Amazonka.CodeDeploy.Types.Alarm
import Amazonka.CodeDeploy.Types.AlarmConfiguration
import Amazonka.CodeDeploy.Types.AppSpecContent
import Amazonka.CodeDeploy.Types.ApplicationInfo
import Amazonka.CodeDeploy.Types.AutoRollbackConfiguration
import Amazonka.CodeDeploy.Types.AutoScalingGroup
import Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Amazonka.CodeDeploy.Types.BlueInstanceTerminationOption
import Amazonka.CodeDeploy.Types.CloudFormationTarget
import Amazonka.CodeDeploy.Types.DeploymentConfigInfo
import Amazonka.CodeDeploy.Types.DeploymentGroupInfo
import Amazonka.CodeDeploy.Types.DeploymentInfo
import Amazonka.CodeDeploy.Types.DeploymentOverview
import Amazonka.CodeDeploy.Types.DeploymentReadyOption
import Amazonka.CodeDeploy.Types.DeploymentStyle
import Amazonka.CodeDeploy.Types.DeploymentTarget
import Amazonka.CodeDeploy.Types.Diagnostics
import Amazonka.CodeDeploy.Types.EC2TagFilter
import Amazonka.CodeDeploy.Types.EC2TagSet
import Amazonka.CodeDeploy.Types.ECSService
import Amazonka.CodeDeploy.Types.ECSTarget
import Amazonka.CodeDeploy.Types.ECSTaskSet
import Amazonka.CodeDeploy.Types.ELBInfo
import Amazonka.CodeDeploy.Types.ErrorInformation
import Amazonka.CodeDeploy.Types.GenericRevisionInfo
import Amazonka.CodeDeploy.Types.GitHubLocation
import Amazonka.CodeDeploy.Types.GreenFleetProvisioningOption
import Amazonka.CodeDeploy.Types.InstanceInfo
import Amazonka.CodeDeploy.Types.InstanceTarget
import Amazonka.CodeDeploy.Types.LambdaFunctionInfo
import Amazonka.CodeDeploy.Types.LambdaTarget
import Amazonka.CodeDeploy.Types.LastDeploymentInfo
import Amazonka.CodeDeploy.Types.LifecycleEvent
import Amazonka.CodeDeploy.Types.LoadBalancerInfo
import Amazonka.CodeDeploy.Types.MinimumHealthyHosts
import Amazonka.CodeDeploy.Types.OnPremisesTagSet
import Amazonka.CodeDeploy.Types.RawString
import Amazonka.CodeDeploy.Types.RelatedDeployments
import Amazonka.CodeDeploy.Types.RevisionInfo
import Amazonka.CodeDeploy.Types.RevisionLocation
import Amazonka.CodeDeploy.Types.RollbackInfo
import Amazonka.CodeDeploy.Types.S3Location
import Amazonka.CodeDeploy.Types.Tag
import Amazonka.CodeDeploy.Types.TagFilter
import Amazonka.CodeDeploy.Types.TargetGroupInfo
import Amazonka.CodeDeploy.Types.TargetGroupPairInfo
import Amazonka.CodeDeploy.Types.TargetInstances
import Amazonka.CodeDeploy.Types.TimeBasedCanary
import Amazonka.CodeDeploy.Types.TimeBasedLinear
import Amazonka.CodeDeploy.Types.TimeRange
import Amazonka.CodeDeploy.Types.TrafficRoute
import Amazonka.CodeDeploy.Types.TrafficRoutingConfig
import Amazonka.CodeDeploy.Types.TriggerConfig
import Amazonka.CodeDeploy.UntagResource
import Amazonka.CodeDeploy.UpdateApplication
import Amazonka.CodeDeploy.UpdateDeploymentGroup
