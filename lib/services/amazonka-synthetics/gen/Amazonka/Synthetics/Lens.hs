{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Synthetics.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Lens
  ( -- * Operations

    -- ** AssociateResource
    associateResource_groupIdentifier,
    associateResource_resourceArn,
    associateResourceResponse_httpStatus,

    -- ** CreateCanary
    createCanary_artifactConfig,
    createCanary_failureRetentionPeriodInDays,
    createCanary_runConfig,
    createCanary_successRetentionPeriodInDays,
    createCanary_tags,
    createCanary_vpcConfig,
    createCanary_name,
    createCanary_code,
    createCanary_artifactS3Location,
    createCanary_executionRoleArn,
    createCanary_schedule,
    createCanary_runtimeVersion,
    createCanaryResponse_canary,
    createCanaryResponse_httpStatus,

    -- ** CreateGroup
    createGroup_tags,
    createGroup_name,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** DeleteCanary
    deleteCanary_deleteLambda,
    deleteCanary_name,
    deleteCanaryResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupIdentifier,
    deleteGroupResponse_httpStatus,

    -- ** DescribeCanaries
    describeCanaries_maxResults,
    describeCanaries_names,
    describeCanaries_nextToken,
    describeCanariesResponse_canaries,
    describeCanariesResponse_nextToken,
    describeCanariesResponse_httpStatus,

    -- ** DescribeCanariesLastRun
    describeCanariesLastRun_maxResults,
    describeCanariesLastRun_names,
    describeCanariesLastRun_nextToken,
    describeCanariesLastRunResponse_canariesLastRun,
    describeCanariesLastRunResponse_nextToken,
    describeCanariesLastRunResponse_httpStatus,

    -- ** DescribeRuntimeVersions
    describeRuntimeVersions_maxResults,
    describeRuntimeVersions_nextToken,
    describeRuntimeVersionsResponse_nextToken,
    describeRuntimeVersionsResponse_runtimeVersions,
    describeRuntimeVersionsResponse_httpStatus,

    -- ** DisassociateResource
    disassociateResource_groupIdentifier,
    disassociateResource_resourceArn,
    disassociateResourceResponse_httpStatus,

    -- ** GetCanary
    getCanary_name,
    getCanaryResponse_canary,
    getCanaryResponse_httpStatus,

    -- ** GetCanaryRuns
    getCanaryRuns_maxResults,
    getCanaryRuns_nextToken,
    getCanaryRuns_name,
    getCanaryRunsResponse_canaryRuns,
    getCanaryRunsResponse_nextToken,
    getCanaryRunsResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupIdentifier,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** ListAssociatedGroups
    listAssociatedGroups_maxResults,
    listAssociatedGroups_nextToken,
    listAssociatedGroups_resourceArn,
    listAssociatedGroupsResponse_groups,
    listAssociatedGroupsResponse_nextToken,
    listAssociatedGroupsResponse_httpStatus,

    -- ** ListGroupResources
    listGroupResources_maxResults,
    listGroupResources_nextToken,
    listGroupResources_groupIdentifier,
    listGroupResourcesResponse_nextToken,
    listGroupResourcesResponse_resources,
    listGroupResourcesResponse_httpStatus,

    -- ** ListGroups
    listGroups_maxResults,
    listGroups_nextToken,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartCanary
    startCanary_name,
    startCanaryResponse_httpStatus,

    -- ** StopCanary
    stopCanary_name,
    stopCanaryResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCanary
    updateCanary_artifactConfig,
    updateCanary_artifactS3Location,
    updateCanary_code,
    updateCanary_executionRoleArn,
    updateCanary_failureRetentionPeriodInDays,
    updateCanary_runConfig,
    updateCanary_runtimeVersion,
    updateCanary_schedule,
    updateCanary_successRetentionPeriodInDays,
    updateCanary_visualReference,
    updateCanary_vpcConfig,
    updateCanary_name,
    updateCanaryResponse_httpStatus,

    -- * Types

    -- ** ArtifactConfigInput
    artifactConfigInput_s3Encryption,

    -- ** ArtifactConfigOutput
    artifactConfigOutput_s3Encryption,

    -- ** BaseScreenshot
    baseScreenshot_ignoreCoordinates,
    baseScreenshot_screenshotName,

    -- ** Canary
    canary_artifactConfig,
    canary_artifactS3Location,
    canary_code,
    canary_engineArn,
    canary_executionRoleArn,
    canary_failureRetentionPeriodInDays,
    canary_id,
    canary_name,
    canary_runConfig,
    canary_runtimeVersion,
    canary_schedule,
    canary_status,
    canary_successRetentionPeriodInDays,
    canary_tags,
    canary_timeline,
    canary_visualReference,
    canary_vpcConfig,

    -- ** CanaryCodeInput
    canaryCodeInput_s3Bucket,
    canaryCodeInput_s3Key,
    canaryCodeInput_s3Version,
    canaryCodeInput_zipFile,
    canaryCodeInput_handler,

    -- ** CanaryCodeOutput
    canaryCodeOutput_handler,
    canaryCodeOutput_sourceLocationArn,

    -- ** CanaryLastRun
    canaryLastRun_canaryName,
    canaryLastRun_lastRun,

    -- ** CanaryRun
    canaryRun_artifactS3Location,
    canaryRun_id,
    canaryRun_name,
    canaryRun_status,
    canaryRun_timeline,

    -- ** CanaryRunConfigInput
    canaryRunConfigInput_activeTracing,
    canaryRunConfigInput_environmentVariables,
    canaryRunConfigInput_memoryInMB,
    canaryRunConfigInput_timeoutInSeconds,

    -- ** CanaryRunConfigOutput
    canaryRunConfigOutput_activeTracing,
    canaryRunConfigOutput_memoryInMB,
    canaryRunConfigOutput_timeoutInSeconds,

    -- ** CanaryRunStatus
    canaryRunStatus_state,
    canaryRunStatus_stateReason,
    canaryRunStatus_stateReasonCode,

    -- ** CanaryRunTimeline
    canaryRunTimeline_completed,
    canaryRunTimeline_started,

    -- ** CanaryScheduleInput
    canaryScheduleInput_durationInSeconds,
    canaryScheduleInput_expression,

    -- ** CanaryScheduleOutput
    canaryScheduleOutput_durationInSeconds,
    canaryScheduleOutput_expression,

    -- ** CanaryStatus
    canaryStatus_state,
    canaryStatus_stateReason,
    canaryStatus_stateReasonCode,

    -- ** CanaryTimeline
    canaryTimeline_created,
    canaryTimeline_lastModified,
    canaryTimeline_lastStarted,
    canaryTimeline_lastStopped,

    -- ** Group
    group_arn,
    group_createdTime,
    group_id,
    group_lastModifiedTime,
    group_name,
    group_tags,

    -- ** GroupSummary
    groupSummary_arn,
    groupSummary_id,
    groupSummary_name,

    -- ** RuntimeVersion
    runtimeVersion_deprecationDate,
    runtimeVersion_description,
    runtimeVersion_releaseDate,
    runtimeVersion_versionName,

    -- ** S3EncryptionConfig
    s3EncryptionConfig_encryptionMode,
    s3EncryptionConfig_kmsKeyArn,

    -- ** VisualReferenceInput
    visualReferenceInput_baseScreenshots,
    visualReferenceInput_baseCanaryRunId,

    -- ** VisualReferenceOutput
    visualReferenceOutput_baseCanaryRunId,
    visualReferenceOutput_baseScreenshots,

    -- ** VpcConfigInput
    vpcConfigInput_securityGroupIds,
    vpcConfigInput_subnetIds,

    -- ** VpcConfigOutput
    vpcConfigOutput_securityGroupIds,
    vpcConfigOutput_subnetIds,
    vpcConfigOutput_vpcId,
  )
where

import Amazonka.Synthetics.AssociateResource
import Amazonka.Synthetics.CreateCanary
import Amazonka.Synthetics.CreateGroup
import Amazonka.Synthetics.DeleteCanary
import Amazonka.Synthetics.DeleteGroup
import Amazonka.Synthetics.DescribeCanaries
import Amazonka.Synthetics.DescribeCanariesLastRun
import Amazonka.Synthetics.DescribeRuntimeVersions
import Amazonka.Synthetics.DisassociateResource
import Amazonka.Synthetics.GetCanary
import Amazonka.Synthetics.GetCanaryRuns
import Amazonka.Synthetics.GetGroup
import Amazonka.Synthetics.ListAssociatedGroups
import Amazonka.Synthetics.ListGroupResources
import Amazonka.Synthetics.ListGroups
import Amazonka.Synthetics.ListTagsForResource
import Amazonka.Synthetics.StartCanary
import Amazonka.Synthetics.StopCanary
import Amazonka.Synthetics.TagResource
import Amazonka.Synthetics.Types.ArtifactConfigInput
import Amazonka.Synthetics.Types.ArtifactConfigOutput
import Amazonka.Synthetics.Types.BaseScreenshot
import Amazonka.Synthetics.Types.Canary
import Amazonka.Synthetics.Types.CanaryCodeInput
import Amazonka.Synthetics.Types.CanaryCodeOutput
import Amazonka.Synthetics.Types.CanaryLastRun
import Amazonka.Synthetics.Types.CanaryRun
import Amazonka.Synthetics.Types.CanaryRunConfigInput
import Amazonka.Synthetics.Types.CanaryRunConfigOutput
import Amazonka.Synthetics.Types.CanaryRunStatus
import Amazonka.Synthetics.Types.CanaryRunTimeline
import Amazonka.Synthetics.Types.CanaryScheduleInput
import Amazonka.Synthetics.Types.CanaryScheduleOutput
import Amazonka.Synthetics.Types.CanaryStatus
import Amazonka.Synthetics.Types.CanaryTimeline
import Amazonka.Synthetics.Types.Group
import Amazonka.Synthetics.Types.GroupSummary
import Amazonka.Synthetics.Types.RuntimeVersion
import Amazonka.Synthetics.Types.S3EncryptionConfig
import Amazonka.Synthetics.Types.VisualReferenceInput
import Amazonka.Synthetics.Types.VisualReferenceOutput
import Amazonka.Synthetics.Types.VpcConfigInput
import Amazonka.Synthetics.Types.VpcConfigOutput
import Amazonka.Synthetics.UntagResource
import Amazonka.Synthetics.UpdateCanary
