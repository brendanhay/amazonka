{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    batchGetApplicationRevisionsResponse_errorMessage,
    batchGetApplicationRevisionsResponse_revisions,
    batchGetApplicationRevisionsResponse_applicationName,
    batchGetApplicationRevisionsResponse_httpStatus,

    -- ** BatchGetApplications
    batchGetApplications_applicationNames,
    batchGetApplicationsResponse_applicationsInfo,
    batchGetApplicationsResponse_httpStatus,

    -- ** BatchGetDeploymentGroups
    batchGetDeploymentGroups_applicationName,
    batchGetDeploymentGroups_deploymentGroupNames,
    batchGetDeploymentGroupsResponse_errorMessage,
    batchGetDeploymentGroupsResponse_deploymentGroupsInfo,
    batchGetDeploymentGroupsResponse_httpStatus,

    -- ** BatchGetDeploymentTargets
    batchGetDeploymentTargets_targetIds,
    batchGetDeploymentTargets_deploymentId,
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
    createApplication_tags,
    createApplication_computePlatform,
    createApplication_applicationName,
    createApplicationResponse_applicationId,
    createApplicationResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_deploymentGroupName,
    createDeployment_fileExistsBehavior,
    createDeployment_revision,
    createDeployment_description,
    createDeployment_updateOutdatedInstancesOnly,
    createDeployment_autoRollbackConfiguration,
    createDeployment_targetInstances,
    createDeployment_ignoreApplicationStopFailures,
    createDeployment_deploymentConfigName,
    createDeployment_applicationName,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** CreateDeploymentConfig
    createDeploymentConfig_computePlatform,
    createDeploymentConfig_trafficRoutingConfig,
    createDeploymentConfig_minimumHealthyHosts,
    createDeploymentConfig_deploymentConfigName,
    createDeploymentConfigResponse_deploymentConfigId,
    createDeploymentConfigResponse_httpStatus,

    -- ** CreateDeploymentGroup
    createDeploymentGroup_tags,
    createDeploymentGroup_loadBalancerInfo,
    createDeploymentGroup_autoScalingGroups,
    createDeploymentGroup_triggerConfigurations,
    createDeploymentGroup_ec2TagFilters,
    createDeploymentGroup_autoRollbackConfiguration,
    createDeploymentGroup_deploymentStyle,
    createDeploymentGroup_blueGreenDeploymentConfiguration,
    createDeploymentGroup_alarmConfiguration,
    createDeploymentGroup_ecsServices,
    createDeploymentGroup_onPremisesTagSet,
    createDeploymentGroup_outdatedInstancesStrategy,
    createDeploymentGroup_onPremisesInstanceTagFilters,
    createDeploymentGroup_ec2TagSet,
    createDeploymentGroup_deploymentConfigName,
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
    getApplicationRevisionResponse_revision,
    getApplicationRevisionResponse_revisionInfo,
    getApplicationRevisionResponse_applicationName,
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
    getDeploymentTarget_targetId,
    getDeploymentTarget_deploymentId,
    getDeploymentTargetResponse_deploymentTarget,
    getDeploymentTargetResponse_httpStatus,

    -- ** GetOnPremisesInstance
    getOnPremisesInstance_instanceName,
    getOnPremisesInstanceResponse_instanceInfo,
    getOnPremisesInstanceResponse_httpStatus,

    -- ** ListApplicationRevisions
    listApplicationRevisions_s3KeyPrefix,
    listApplicationRevisions_s3Bucket,
    listApplicationRevisions_sortOrder,
    listApplicationRevisions_nextToken,
    listApplicationRevisions_deployed,
    listApplicationRevisions_sortBy,
    listApplicationRevisions_applicationName,
    listApplicationRevisionsResponse_nextToken,
    listApplicationRevisionsResponse_revisions,
    listApplicationRevisionsResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

    -- ** ListDeploymentConfigs
    listDeploymentConfigs_nextToken,
    listDeploymentConfigsResponse_nextToken,
    listDeploymentConfigsResponse_deploymentConfigsList,
    listDeploymentConfigsResponse_httpStatus,

    -- ** ListDeploymentGroups
    listDeploymentGroups_nextToken,
    listDeploymentGroups_applicationName,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_httpStatus,

    -- ** ListDeploymentTargets
    listDeploymentTargets_nextToken,
    listDeploymentTargets_targetFilters,
    listDeploymentTargets_deploymentId,
    listDeploymentTargetsResponse_nextToken,
    listDeploymentTargetsResponse_targetIds,
    listDeploymentTargetsResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_deploymentGroupName,
    listDeployments_externalId,
    listDeployments_includeOnlyStatuses,
    listDeployments_createTimeRange,
    listDeployments_applicationName,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
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
    listOnPremisesInstancesResponse_nextToken,
    listOnPremisesInstancesResponse_instanceNames,
    listOnPremisesInstancesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** PutLifecycleEventHookExecutionStatus
    putLifecycleEventHookExecutionStatus_deploymentId,
    putLifecycleEventHookExecutionStatus_status,
    putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId,
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
    updateApplication_newApplicationName,
    updateApplication_applicationName,

    -- ** UpdateDeploymentGroup
    updateDeploymentGroup_loadBalancerInfo,
    updateDeploymentGroup_autoScalingGroups,
    updateDeploymentGroup_serviceRoleArn,
    updateDeploymentGroup_triggerConfigurations,
    updateDeploymentGroup_newDeploymentGroupName,
    updateDeploymentGroup_ec2TagFilters,
    updateDeploymentGroup_autoRollbackConfiguration,
    updateDeploymentGroup_deploymentStyle,
    updateDeploymentGroup_blueGreenDeploymentConfiguration,
    updateDeploymentGroup_alarmConfiguration,
    updateDeploymentGroup_ecsServices,
    updateDeploymentGroup_onPremisesTagSet,
    updateDeploymentGroup_outdatedInstancesStrategy,
    updateDeploymentGroup_onPremisesInstanceTagFilters,
    updateDeploymentGroup_ec2TagSet,
    updateDeploymentGroup_deploymentConfigName,
    updateDeploymentGroup_applicationName,
    updateDeploymentGroup_currentDeploymentGroupName,
    updateDeploymentGroupResponse_hooksNotCleanedUp,
    updateDeploymentGroupResponse_httpStatus,

    -- * Types

    -- ** Alarm
    alarm_name,

    -- ** AlarmConfiguration
    alarmConfiguration_alarms,
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_enabled,

    -- ** AppSpecContent
    appSpecContent_sha256,
    appSpecContent_content,

    -- ** ApplicationInfo
    applicationInfo_linkedToGitHub,
    applicationInfo_gitHubAccountName,
    applicationInfo_computePlatform,
    applicationInfo_createTime,
    applicationInfo_applicationId,
    applicationInfo_applicationName,

    -- ** AutoRollbackConfiguration
    autoRollbackConfiguration_enabled,
    autoRollbackConfiguration_events,

    -- ** AutoScalingGroup
    autoScalingGroup_name,
    autoScalingGroup_hook,

    -- ** BlueGreenDeploymentConfiguration
    blueGreenDeploymentConfiguration_greenFleetProvisioningOption,
    blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess,
    blueGreenDeploymentConfiguration_deploymentReadyOption,

    -- ** BlueInstanceTerminationOption
    blueInstanceTerminationOption_terminationWaitTimeInMinutes,
    blueInstanceTerminationOption_action,

    -- ** CloudFormationTarget
    cloudFormationTarget_resourceType,
    cloudFormationTarget_targetId,
    cloudFormationTarget_lifecycleEvents,
    cloudFormationTarget_lastUpdatedAt,
    cloudFormationTarget_targetVersionWeight,
    cloudFormationTarget_deploymentId,
    cloudFormationTarget_status,

    -- ** DeploymentConfigInfo
    deploymentConfigInfo_deploymentConfigId,
    deploymentConfigInfo_computePlatform,
    deploymentConfigInfo_trafficRoutingConfig,
    deploymentConfigInfo_createTime,
    deploymentConfigInfo_minimumHealthyHosts,
    deploymentConfigInfo_deploymentConfigName,

    -- ** DeploymentGroupInfo
    deploymentGroupInfo_targetRevision,
    deploymentGroupInfo_loadBalancerInfo,
    deploymentGroupInfo_autoScalingGroups,
    deploymentGroupInfo_serviceRoleArn,
    deploymentGroupInfo_lastSuccessfulDeployment,
    deploymentGroupInfo_triggerConfigurations,
    deploymentGroupInfo_deploymentGroupName,
    deploymentGroupInfo_computePlatform,
    deploymentGroupInfo_ec2TagFilters,
    deploymentGroupInfo_autoRollbackConfiguration,
    deploymentGroupInfo_deploymentStyle,
    deploymentGroupInfo_blueGreenDeploymentConfiguration,
    deploymentGroupInfo_alarmConfiguration,
    deploymentGroupInfo_ecsServices,
    deploymentGroupInfo_onPremisesTagSet,
    deploymentGroupInfo_outdatedInstancesStrategy,
    deploymentGroupInfo_onPremisesInstanceTagFilters,
    deploymentGroupInfo_ec2TagSet,
    deploymentGroupInfo_deploymentGroupId,
    deploymentGroupInfo_lastAttemptedDeployment,
    deploymentGroupInfo_deploymentConfigName,
    deploymentGroupInfo_applicationName,

    -- ** DeploymentInfo
    deploymentInfo_loadBalancerInfo,
    deploymentInfo_deploymentGroupName,
    deploymentInfo_deploymentId,
    deploymentInfo_fileExistsBehavior,
    deploymentInfo_deploymentOverview,
    deploymentInfo_revision,
    deploymentInfo_rollbackInfo,
    deploymentInfo_externalId,
    deploymentInfo_status,
    deploymentInfo_description,
    deploymentInfo_computePlatform,
    deploymentInfo_updateOutdatedInstancesOnly,
    deploymentInfo_completeTime,
    deploymentInfo_deploymentStatusMessages,
    deploymentInfo_autoRollbackConfiguration,
    deploymentInfo_deploymentStyle,
    deploymentInfo_blueGreenDeploymentConfiguration,
    deploymentInfo_errorInformation,
    deploymentInfo_relatedDeployments,
    deploymentInfo_targetInstances,
    deploymentInfo_createTime,
    deploymentInfo_additionalDeploymentStatusInfo,
    deploymentInfo_creator,
    deploymentInfo_previousRevision,
    deploymentInfo_startTime,
    deploymentInfo_ignoreApplicationStopFailures,
    deploymentInfo_instanceTerminationWaitTimeStarted,
    deploymentInfo_deploymentConfigName,
    deploymentInfo_applicationName,

    -- ** DeploymentOverview
    deploymentOverview_failed,
    deploymentOverview_succeeded,
    deploymentOverview_ready,
    deploymentOverview_skipped,
    deploymentOverview_pending,
    deploymentOverview_inProgress,

    -- ** DeploymentReadyOption
    deploymentReadyOption_waitTimeInMinutes,
    deploymentReadyOption_actionOnTimeout,

    -- ** DeploymentStyle
    deploymentStyle_deploymentType,
    deploymentStyle_deploymentOption,

    -- ** DeploymentTarget
    deploymentTarget_instanceTarget,
    deploymentTarget_ecsTarget,
    deploymentTarget_deploymentTargetType,
    deploymentTarget_lambdaTarget,
    deploymentTarget_cloudFormationTarget,

    -- ** Diagnostics
    diagnostics_message,
    diagnostics_logTail,
    diagnostics_errorCode,
    diagnostics_scriptName,

    -- ** EC2TagFilter
    eC2TagFilter_key,
    eC2TagFilter_type,
    eC2TagFilter_value,

    -- ** EC2TagSet
    eC2TagSet_ec2TagSetList,

    -- ** ECSService
    eCSService_serviceName,
    eCSService_clusterName,

    -- ** ECSTarget
    eCSTarget_targetId,
    eCSTarget_lifecycleEvents,
    eCSTarget_lastUpdatedAt,
    eCSTarget_deploymentId,
    eCSTarget_targetArn,
    eCSTarget_status,
    eCSTarget_taskSetsInfo,

    -- ** ECSTaskSet
    eCSTaskSet_identifer,
    eCSTaskSet_status,
    eCSTaskSet_targetGroup,
    eCSTaskSet_desiredCount,
    eCSTaskSet_pendingCount,
    eCSTaskSet_runningCount,
    eCSTaskSet_taskSetLabel,
    eCSTaskSet_trafficWeight,

    -- ** ELBInfo
    eLBInfo_name,

    -- ** ErrorInformation
    errorInformation_message,
    errorInformation_code,

    -- ** GenericRevisionInfo
    genericRevisionInfo_deploymentGroups,
    genericRevisionInfo_registerTime,
    genericRevisionInfo_description,
    genericRevisionInfo_firstUsedTime,
    genericRevisionInfo_lastUsedTime,

    -- ** GitHubLocation
    gitHubLocation_commitId,
    gitHubLocation_repository,

    -- ** GreenFleetProvisioningOption
    greenFleetProvisioningOption_action,

    -- ** InstanceInfo
    instanceInfo_tags,
    instanceInfo_iamSessionArn,
    instanceInfo_deregisterTime,
    instanceInfo_iamUserArn,
    instanceInfo_instanceName,
    instanceInfo_registerTime,
    instanceInfo_instanceArn,

    -- ** InstanceTarget
    instanceTarget_targetId,
    instanceTarget_instanceLabel,
    instanceTarget_lifecycleEvents,
    instanceTarget_lastUpdatedAt,
    instanceTarget_deploymentId,
    instanceTarget_targetArn,
    instanceTarget_status,

    -- ** LambdaFunctionInfo
    lambdaFunctionInfo_targetVersionWeight,
    lambdaFunctionInfo_functionName,
    lambdaFunctionInfo_targetVersion,
    lambdaFunctionInfo_currentVersion,
    lambdaFunctionInfo_functionAlias,

    -- ** LambdaTarget
    lambdaTarget_targetId,
    lambdaTarget_lifecycleEvents,
    lambdaTarget_lambdaFunctionInfo,
    lambdaTarget_lastUpdatedAt,
    lambdaTarget_deploymentId,
    lambdaTarget_targetArn,
    lambdaTarget_status,

    -- ** LastDeploymentInfo
    lastDeploymentInfo_deploymentId,
    lastDeploymentInfo_status,
    lastDeploymentInfo_endTime,
    lastDeploymentInfo_createTime,

    -- ** LifecycleEvent
    lifecycleEvent_lifecycleEventName,
    lifecycleEvent_status,
    lifecycleEvent_endTime,
    lifecycleEvent_diagnostics,
    lifecycleEvent_startTime,

    -- ** LoadBalancerInfo
    loadBalancerInfo_targetGroupPairInfoList,
    loadBalancerInfo_elbInfoList,
    loadBalancerInfo_targetGroupInfoList,

    -- ** MinimumHealthyHosts
    minimumHealthyHosts_type,
    minimumHealthyHosts_value,

    -- ** OnPremisesTagSet
    onPremisesTagSet_onPremisesTagSetList,

    -- ** RawString
    rawString_sha256,
    rawString_content,

    -- ** RelatedDeployments
    relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds,
    relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId,

    -- ** RevisionInfo
    revisionInfo_revisionLocation,
    revisionInfo_genericRevisionInfo,

    -- ** RevisionLocation
    revisionLocation_string,
    revisionLocation_appSpecContent,
    revisionLocation_s3Location,
    revisionLocation_gitHubLocation,
    revisionLocation_revisionType,

    -- ** RollbackInfo
    rollbackInfo_rollbackMessage,
    rollbackInfo_rollbackDeploymentId,
    rollbackInfo_rollbackTriggeringDeploymentId,

    -- ** S3Location
    s3Location_key,
    s3Location_bundleType,
    s3Location_bucket,
    s3Location_eTag,
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
    targetGroupPairInfo_testTrafficRoute,
    targetGroupPairInfo_prodTrafficRoute,
    targetGroupPairInfo_targetGroups,

    -- ** TargetInstances
    targetInstances_autoScalingGroups,
    targetInstances_tagFilters,
    targetInstances_ec2TagSet,

    -- ** TimeBasedCanary
    timeBasedCanary_canaryPercentage,
    timeBasedCanary_canaryInterval,

    -- ** TimeBasedLinear
    timeBasedLinear_linearInterval,
    timeBasedLinear_linearPercentage,

    -- ** TimeRange
    timeRange_start,
    timeRange_end,

    -- ** TrafficRoute
    trafficRoute_listenerArns,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_type,
    trafficRoutingConfig_timeBasedLinear,
    trafficRoutingConfig_timeBasedCanary,

    -- ** TriggerConfig
    triggerConfig_triggerName,
    triggerConfig_triggerEvents,
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
