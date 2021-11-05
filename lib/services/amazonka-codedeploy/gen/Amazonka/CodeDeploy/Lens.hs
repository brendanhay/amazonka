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

    -- ** RemoveTagsFromOnPremisesInstances
    removeTagsFromOnPremisesInstances_tags,
    removeTagsFromOnPremisesInstances_instanceNames,

    -- ** BatchGetDeploymentGroups
    batchGetDeploymentGroups_applicationName,
    batchGetDeploymentGroups_deploymentGroupNames,
    batchGetDeploymentGroupsResponse_deploymentGroupsInfo,
    batchGetDeploymentGroupsResponse_errorMessage,
    batchGetDeploymentGroupsResponse_httpStatus,

    -- ** DeleteDeploymentGroup
    deleteDeploymentGroup_applicationName,
    deleteDeploymentGroup_deploymentGroupName,
    deleteDeploymentGroupResponse_hooksNotCleanedUp,
    deleteDeploymentGroupResponse_httpStatus,

    -- ** UpdateDeploymentGroup
    updateDeploymentGroup_serviceRoleArn,
    updateDeploymentGroup_ec2TagSet,
    updateDeploymentGroup_deploymentConfigName,
    updateDeploymentGroup_onPremisesTagSet,
    updateDeploymentGroup_newDeploymentGroupName,
    updateDeploymentGroup_ec2TagFilters,
    updateDeploymentGroup_ecsServices,
    updateDeploymentGroup_blueGreenDeploymentConfiguration,
    updateDeploymentGroup_loadBalancerInfo,
    updateDeploymentGroup_outdatedInstancesStrategy,
    updateDeploymentGroup_onPremisesInstanceTagFilters,
    updateDeploymentGroup_alarmConfiguration,
    updateDeploymentGroup_triggerConfigurations,
    updateDeploymentGroup_autoScalingGroups,
    updateDeploymentGroup_deploymentStyle,
    updateDeploymentGroup_autoRollbackConfiguration,
    updateDeploymentGroup_applicationName,
    updateDeploymentGroup_currentDeploymentGroupName,
    updateDeploymentGroupResponse_hooksNotCleanedUp,
    updateDeploymentGroupResponse_httpStatus,

    -- ** ListOnPremisesInstances
    listOnPremisesInstances_tagFilters,
    listOnPremisesInstances_nextToken,
    listOnPremisesInstances_registrationStatus,
    listOnPremisesInstancesResponse_nextToken,
    listOnPremisesInstancesResponse_instanceNames,
    listOnPremisesInstancesResponse_httpStatus,

    -- ** CreateDeploymentConfig
    createDeploymentConfig_computePlatform,
    createDeploymentConfig_minimumHealthyHosts,
    createDeploymentConfig_trafficRoutingConfig,
    createDeploymentConfig_deploymentConfigName,
    createDeploymentConfigResponse_deploymentConfigId,
    createDeploymentConfigResponse_httpStatus,

    -- ** GetApplicationRevision
    getApplicationRevision_applicationName,
    getApplicationRevision_revision,
    getApplicationRevisionResponse_applicationName,
    getApplicationRevisionResponse_revisionInfo,
    getApplicationRevisionResponse_revision,
    getApplicationRevisionResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_deploymentId,
    getDeploymentResponse_deploymentInfo,
    getDeploymentResponse_httpStatus,

    -- ** DeleteDeploymentConfig
    deleteDeploymentConfig_deploymentConfigName,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetDeploymentConfig
    getDeploymentConfig_deploymentConfigName,
    getDeploymentConfigResponse_deploymentConfigInfo,
    getDeploymentConfigResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_deploymentConfigName,
    createDeployment_fileExistsBehavior,
    createDeployment_targetInstances,
    createDeployment_revision,
    createDeployment_description,
    createDeployment_autoRollbackConfiguration,
    createDeployment_updateOutdatedInstancesOnly,
    createDeployment_deploymentGroupName,
    createDeployment_ignoreApplicationStopFailures,
    createDeployment_applicationName,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** BatchGetApplicationRevisions
    batchGetApplicationRevisions_applicationName,
    batchGetApplicationRevisions_revisions,
    batchGetApplicationRevisionsResponse_applicationName,
    batchGetApplicationRevisionsResponse_revisions,
    batchGetApplicationRevisionsResponse_errorMessage,
    batchGetApplicationRevisionsResponse_httpStatus,

    -- ** BatchGetDeployments
    batchGetDeployments_deploymentIds,
    batchGetDeploymentsResponse_deploymentsInfo,
    batchGetDeploymentsResponse_httpStatus,

    -- ** GetOnPremisesInstance
    getOnPremisesInstance_instanceName,
    getOnPremisesInstanceResponse_instanceInfo,
    getOnPremisesInstanceResponse_httpStatus,

    -- ** RegisterApplicationRevision
    registerApplicationRevision_description,
    registerApplicationRevision_applicationName,
    registerApplicationRevision_revision,

    -- ** ContinueDeployment
    continueDeployment_deploymentId,
    continueDeployment_deploymentWaitType,

    -- ** BatchGetApplications
    batchGetApplications_applicationNames,
    batchGetApplicationsResponse_applicationsInfo,
    batchGetApplicationsResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationName,

    -- ** UpdateApplication
    updateApplication_newApplicationName,
    updateApplication_applicationName,

    -- ** DeleteGitHubAccountToken
    deleteGitHubAccountToken_tokenName,
    deleteGitHubAccountTokenResponse_tokenName,
    deleteGitHubAccountTokenResponse_httpStatus,

    -- ** DeregisterOnPremisesInstance
    deregisterOnPremisesInstance_instanceName,

    -- ** PutLifecycleEventHookExecutionStatus
    putLifecycleEventHookExecutionStatus_status,
    putLifecycleEventHookExecutionStatus_deploymentId,
    putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatusResponse_httpStatus,

    -- ** GetDeploymentTarget
    getDeploymentTarget_targetId,
    getDeploymentTarget_deploymentId,
    getDeploymentTargetResponse_deploymentTarget,
    getDeploymentTargetResponse_httpStatus,

    -- ** CreateApplication
    createApplication_computePlatform,
    createApplication_tags,
    createApplication_applicationName,
    createApplicationResponse_applicationId,
    createApplicationResponse_httpStatus,

    -- ** BatchGetDeploymentTargets
    batchGetDeploymentTargets_deploymentId,
    batchGetDeploymentTargets_targetIds,
    batchGetDeploymentTargetsResponse_deploymentTargets,
    batchGetDeploymentTargetsResponse_httpStatus,

    -- ** StopDeployment
    stopDeployment_autoRollbackEnabled,
    stopDeployment_deploymentId,
    stopDeploymentResponse_status,
    stopDeploymentResponse_statusMessage,
    stopDeploymentResponse_httpStatus,

    -- ** ListGitHubAccountTokenNames
    listGitHubAccountTokenNames_nextToken,
    listGitHubAccountTokenNamesResponse_tokenNameList,
    listGitHubAccountTokenNamesResponse_nextToken,
    listGitHubAccountTokenNamesResponse_httpStatus,

    -- ** GetApplication
    getApplication_applicationName,
    getApplicationResponse_application,
    getApplicationResponse_httpStatus,

    -- ** ListDeploymentGroups
    listDeploymentGroups_nextToken,
    listDeploymentGroups_applicationName,
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_httpStatus,

    -- ** BatchGetOnPremisesInstances
    batchGetOnPremisesInstances_instanceNames,
    batchGetOnPremisesInstancesResponse_instanceInfos,
    batchGetOnPremisesInstancesResponse_httpStatus,

    -- ** RegisterOnPremisesInstance
    registerOnPremisesInstance_iamUserArn,
    registerOnPremisesInstance_iamSessionArn,
    registerOnPremisesInstance_instanceName,

    -- ** CreateDeploymentGroup
    createDeploymentGroup_ec2TagSet,
    createDeploymentGroup_deploymentConfigName,
    createDeploymentGroup_onPremisesTagSet,
    createDeploymentGroup_ec2TagFilters,
    createDeploymentGroup_ecsServices,
    createDeploymentGroup_blueGreenDeploymentConfiguration,
    createDeploymentGroup_loadBalancerInfo,
    createDeploymentGroup_outdatedInstancesStrategy,
    createDeploymentGroup_onPremisesInstanceTagFilters,
    createDeploymentGroup_alarmConfiguration,
    createDeploymentGroup_triggerConfigurations,
    createDeploymentGroup_autoScalingGroups,
    createDeploymentGroup_deploymentStyle,
    createDeploymentGroup_autoRollbackConfiguration,
    createDeploymentGroup_tags,
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

    -- ** GetDeploymentGroup
    getDeploymentGroup_applicationName,
    getDeploymentGroup_deploymentGroupName,
    getDeploymentGroupResponse_deploymentGroupInfo,
    getDeploymentGroupResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_createTimeRange,
    listDeployments_nextToken,
    listDeployments_includeOnlyStatuses,
    listDeployments_applicationName,
    listDeployments_externalId,
    listDeployments_deploymentGroupName,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListApplicationRevisions
    listApplicationRevisions_s3KeyPrefix,
    listApplicationRevisions_deployed,
    listApplicationRevisions_sortOrder,
    listApplicationRevisions_nextToken,
    listApplicationRevisions_s3Bucket,
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

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteResourcesByExternalId
    deleteResourcesByExternalId_externalId,
    deleteResourcesByExternalIdResponse_httpStatus,

    -- ** AddTagsToOnPremisesInstances
    addTagsToOnPremisesInstances_tags,
    addTagsToOnPremisesInstances_instanceNames,

    -- ** ListDeploymentTargets
    listDeploymentTargets_deploymentId,
    listDeploymentTargets_targetFilters,
    listDeploymentTargets_nextToken,
    listDeploymentTargetsResponse_nextToken,
    listDeploymentTargetsResponse_targetIds,
    listDeploymentTargetsResponse_httpStatus,

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
    applicationInfo_linkedToGitHub,
    applicationInfo_computePlatform,
    applicationInfo_applicationId,
    applicationInfo_applicationName,
    applicationInfo_gitHubAccountName,
    applicationInfo_createTime,

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
    cloudFormationTarget_targetId,
    cloudFormationTarget_status,
    cloudFormationTarget_deploymentId,
    cloudFormationTarget_resourceType,
    cloudFormationTarget_lastUpdatedAt,
    cloudFormationTarget_lifecycleEvents,
    cloudFormationTarget_targetVersionWeight,

    -- ** DeploymentConfigInfo
    deploymentConfigInfo_deploymentConfigName,
    deploymentConfigInfo_computePlatform,
    deploymentConfigInfo_minimumHealthyHosts,
    deploymentConfigInfo_trafficRoutingConfig,
    deploymentConfigInfo_deploymentConfigId,
    deploymentConfigInfo_createTime,

    -- ** DeploymentGroupInfo
    deploymentGroupInfo_serviceRoleArn,
    deploymentGroupInfo_ec2TagSet,
    deploymentGroupInfo_deploymentConfigName,
    deploymentGroupInfo_lastAttemptedDeployment,
    deploymentGroupInfo_onPremisesTagSet,
    deploymentGroupInfo_computePlatform,
    deploymentGroupInfo_targetRevision,
    deploymentGroupInfo_ec2TagFilters,
    deploymentGroupInfo_ecsServices,
    deploymentGroupInfo_blueGreenDeploymentConfiguration,
    deploymentGroupInfo_loadBalancerInfo,
    deploymentGroupInfo_outdatedInstancesStrategy,
    deploymentGroupInfo_onPremisesInstanceTagFilters,
    deploymentGroupInfo_lastSuccessfulDeployment,
    deploymentGroupInfo_applicationName,
    deploymentGroupInfo_alarmConfiguration,
    deploymentGroupInfo_triggerConfigurations,
    deploymentGroupInfo_deploymentGroupId,
    deploymentGroupInfo_autoScalingGroups,
    deploymentGroupInfo_deploymentStyle,
    deploymentGroupInfo_autoRollbackConfiguration,
    deploymentGroupInfo_deploymentGroupName,

    -- ** DeploymentInfo
    deploymentInfo_creator,
    deploymentInfo_status,
    deploymentInfo_deploymentId,
    deploymentInfo_deploymentConfigName,
    deploymentInfo_computePlatform,
    deploymentInfo_previousRevision,
    deploymentInfo_instanceTerminationWaitTimeStarted,
    deploymentInfo_deploymentStatusMessages,
    deploymentInfo_relatedDeployments,
    deploymentInfo_startTime,
    deploymentInfo_completeTime,
    deploymentInfo_blueGreenDeploymentConfiguration,
    deploymentInfo_errorInformation,
    deploymentInfo_loadBalancerInfo,
    deploymentInfo_additionalDeploymentStatusInfo,
    deploymentInfo_deploymentOverview,
    deploymentInfo_fileExistsBehavior,
    deploymentInfo_applicationName,
    deploymentInfo_rollbackInfo,
    deploymentInfo_externalId,
    deploymentInfo_targetInstances,
    deploymentInfo_revision,
    deploymentInfo_description,
    deploymentInfo_deploymentStyle,
    deploymentInfo_createTime,
    deploymentInfo_autoRollbackConfiguration,
    deploymentInfo_updateOutdatedInstancesOnly,
    deploymentInfo_deploymentGroupName,
    deploymentInfo_ignoreApplicationStopFailures,

    -- ** DeploymentOverview
    deploymentOverview_pending,
    deploymentOverview_skipped,
    deploymentOverview_inProgress,
    deploymentOverview_succeeded,
    deploymentOverview_ready,
    deploymentOverview_failed,

    -- ** DeploymentReadyOption
    deploymentReadyOption_actionOnTimeout,
    deploymentReadyOption_waitTimeInMinutes,

    -- ** DeploymentStyle
    deploymentStyle_deploymentOption,
    deploymentStyle_deploymentType,

    -- ** DeploymentTarget
    deploymentTarget_instanceTarget,
    deploymentTarget_cloudFormationTarget,
    deploymentTarget_ecsTarget,
    deploymentTarget_deploymentTargetType,
    deploymentTarget_lambdaTarget,

    -- ** Diagnostics
    diagnostics_logTail,
    diagnostics_errorCode,
    diagnostics_scriptName,
    diagnostics_message,

    -- ** EC2TagFilter
    eC2TagFilter_value,
    eC2TagFilter_key,
    eC2TagFilter_type,

    -- ** EC2TagSet
    eC2TagSet_ec2TagSetList,

    -- ** ECSService
    eCSService_serviceName,
    eCSService_clusterName,

    -- ** ECSTarget
    eCSTarget_targetArn,
    eCSTarget_targetId,
    eCSTarget_status,
    eCSTarget_deploymentId,
    eCSTarget_lastUpdatedAt,
    eCSTarget_taskSetsInfo,
    eCSTarget_lifecycleEvents,

    -- ** ECSTaskSet
    eCSTaskSet_runningCount,
    eCSTaskSet_status,
    eCSTaskSet_identifer,
    eCSTaskSet_desiredCount,
    eCSTaskSet_pendingCount,
    eCSTaskSet_trafficWeight,
    eCSTaskSet_targetGroup,
    eCSTaskSet_taskSetLabel,

    -- ** ELBInfo
    eLBInfo_name,

    -- ** ErrorInformation
    errorInformation_code,
    errorInformation_message,

    -- ** GenericRevisionInfo
    genericRevisionInfo_registerTime,
    genericRevisionInfo_firstUsedTime,
    genericRevisionInfo_deploymentGroups,
    genericRevisionInfo_lastUsedTime,
    genericRevisionInfo_description,

    -- ** GitHubLocation
    gitHubLocation_commitId,
    gitHubLocation_repository,

    -- ** GreenFleetProvisioningOption
    greenFleetProvisioningOption_action,

    -- ** InstanceInfo
    instanceInfo_registerTime,
    instanceInfo_instanceArn,
    instanceInfo_deregisterTime,
    instanceInfo_iamUserArn,
    instanceInfo_instanceName,
    instanceInfo_iamSessionArn,
    instanceInfo_tags,

    -- ** InstanceTarget
    instanceTarget_targetArn,
    instanceTarget_targetId,
    instanceTarget_status,
    instanceTarget_deploymentId,
    instanceTarget_instanceLabel,
    instanceTarget_lastUpdatedAt,
    instanceTarget_lifecycleEvents,

    -- ** LambdaFunctionInfo
    lambdaFunctionInfo_currentVersion,
    lambdaFunctionInfo_functionAlias,
    lambdaFunctionInfo_functionName,
    lambdaFunctionInfo_targetVersion,
    lambdaFunctionInfo_targetVersionWeight,

    -- ** LambdaTarget
    lambdaTarget_targetArn,
    lambdaTarget_targetId,
    lambdaTarget_status,
    lambdaTarget_deploymentId,
    lambdaTarget_lastUpdatedAt,
    lambdaTarget_lifecycleEvents,
    lambdaTarget_lambdaFunctionInfo,

    -- ** LastDeploymentInfo
    lastDeploymentInfo_status,
    lastDeploymentInfo_deploymentId,
    lastDeploymentInfo_endTime,
    lastDeploymentInfo_createTime,

    -- ** LifecycleEvent
    lifecycleEvent_status,
    lifecycleEvent_lifecycleEventName,
    lifecycleEvent_startTime,
    lifecycleEvent_diagnostics,
    lifecycleEvent_endTime,

    -- ** LoadBalancerInfo
    loadBalancerInfo_elbInfoList,
    loadBalancerInfo_targetGroupInfoList,
    loadBalancerInfo_targetGroupPairInfoList,

    -- ** MinimumHealthyHosts
    minimumHealthyHosts_value,
    minimumHealthyHosts_type,

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
    revisionLocation_string,
    revisionLocation_revisionType,
    revisionLocation_s3Location,
    revisionLocation_appSpecContent,
    revisionLocation_gitHubLocation,

    -- ** RollbackInfo
    rollbackInfo_rollbackTriggeringDeploymentId,
    rollbackInfo_rollbackMessage,
    rollbackInfo_rollbackDeploymentId,

    -- ** S3Location
    s3Location_bundleType,
    s3Location_eTag,
    s3Location_bucket,
    s3Location_key,
    s3Location_version,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TagFilter
    tagFilter_value,
    tagFilter_key,
    tagFilter_type,

    -- ** TargetGroupInfo
    targetGroupInfo_name,

    -- ** TargetGroupPairInfo
    targetGroupPairInfo_prodTrafficRoute,
    targetGroupPairInfo_testTrafficRoute,
    targetGroupPairInfo_targetGroups,

    -- ** TargetInstances
    targetInstances_ec2TagSet,
    targetInstances_tagFilters,
    targetInstances_autoScalingGroups,

    -- ** TimeBasedCanary
    timeBasedCanary_canaryInterval,
    timeBasedCanary_canaryPercentage,

    -- ** TimeBasedLinear
    timeBasedLinear_linearInterval,
    timeBasedLinear_linearPercentage,

    -- ** TimeRange
    timeRange_start,
    timeRange_end,

    -- ** TrafficRoute
    trafficRoute_listenerArns,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_timeBasedCanary,
    trafficRoutingConfig_timeBasedLinear,
    trafficRoutingConfig_type,

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
