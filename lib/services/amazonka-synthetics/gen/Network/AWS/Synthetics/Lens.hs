{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Synthetics.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Synthetics.Lens
  ( -- * Operations

    -- ** UpdateCanary
    updateCanary_successRetentionPeriodInDays,
    updateCanary_schedule,
    updateCanary_artifactS3Location,
    updateCanary_runConfig,
    updateCanary_executionRoleArn,
    updateCanary_runtimeVersion,
    updateCanary_failureRetentionPeriodInDays,
    updateCanary_artifactConfig,
    updateCanary_vpcConfig,
    updateCanary_visualReference,
    updateCanary_code,
    updateCanary_name,
    updateCanaryResponse_httpStatus,

    -- ** DeleteCanary
    deleteCanary_name,
    deleteCanaryResponse_httpStatus,

    -- ** CreateCanary
    createCanary_successRetentionPeriodInDays,
    createCanary_runConfig,
    createCanary_failureRetentionPeriodInDays,
    createCanary_artifactConfig,
    createCanary_vpcConfig,
    createCanary_tags,
    createCanary_name,
    createCanary_code,
    createCanary_artifactS3Location,
    createCanary_executionRoleArn,
    createCanary_schedule,
    createCanary_runtimeVersion,
    createCanaryResponse_canary,
    createCanaryResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetCanaryRuns
    getCanaryRuns_nextToken,
    getCanaryRuns_maxResults,
    getCanaryRuns_name,
    getCanaryRunsResponse_nextToken,
    getCanaryRunsResponse_canaryRuns,
    getCanaryRunsResponse_httpStatus,

    -- ** GetCanary
    getCanary_name,
    getCanaryResponse_canary,
    getCanaryResponse_httpStatus,

    -- ** DescribeRuntimeVersions
    describeRuntimeVersions_nextToken,
    describeRuntimeVersions_maxResults,
    describeRuntimeVersionsResponse_runtimeVersions,
    describeRuntimeVersionsResponse_nextToken,
    describeRuntimeVersionsResponse_httpStatus,

    -- ** DescribeCanariesLastRun
    describeCanariesLastRun_nextToken,
    describeCanariesLastRun_maxResults,
    describeCanariesLastRunResponse_nextToken,
    describeCanariesLastRunResponse_canariesLastRun,
    describeCanariesLastRunResponse_httpStatus,

    -- ** StartCanary
    startCanary_name,
    startCanaryResponse_httpStatus,

    -- ** DescribeCanaries
    describeCanaries_nextToken,
    describeCanaries_maxResults,
    describeCanariesResponse_canaries,
    describeCanariesResponse_nextToken,
    describeCanariesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** StopCanary
    stopCanary_name,
    stopCanaryResponse_httpStatus,

    -- * Types

    -- ** ArtifactConfigInput
    artifactConfigInput_s3Encryption,

    -- ** ArtifactConfigOutput
    artifactConfigOutput_s3Encryption,

    -- ** BaseScreenshot
    baseScreenshot_ignoreCoordinates,
    baseScreenshot_screenshotName,

    -- ** Canary
    canary_status,
    canary_successRetentionPeriodInDays,
    canary_schedule,
    canary_artifactS3Location,
    canary_runConfig,
    canary_executionRoleArn,
    canary_runtimeVersion,
    canary_failureRetentionPeriodInDays,
    canary_artifactConfig,
    canary_vpcConfig,
    canary_visualReference,
    canary_name,
    canary_id,
    canary_code,
    canary_timeline,
    canary_engineArn,
    canary_tags,

    -- ** CanaryCodeInput
    canaryCodeInput_s3Key,
    canaryCodeInput_s3Version,
    canaryCodeInput_zipFile,
    canaryCodeInput_s3Bucket,
    canaryCodeInput_handler,

    -- ** CanaryCodeOutput
    canaryCodeOutput_sourceLocationArn,
    canaryCodeOutput_handler,

    -- ** CanaryLastRun
    canaryLastRun_canaryName,
    canaryLastRun_lastRun,

    -- ** CanaryRun
    canaryRun_status,
    canaryRun_artifactS3Location,
    canaryRun_name,
    canaryRun_id,
    canaryRun_timeline,

    -- ** CanaryRunConfigInput
    canaryRunConfigInput_timeoutInSeconds,
    canaryRunConfigInput_environmentVariables,
    canaryRunConfigInput_activeTracing,
    canaryRunConfigInput_memoryInMB,

    -- ** CanaryRunConfigOutput
    canaryRunConfigOutput_timeoutInSeconds,
    canaryRunConfigOutput_activeTracing,
    canaryRunConfigOutput_memoryInMB,

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
    canaryTimeline_lastStarted,
    canaryTimeline_lastStopped,
    canaryTimeline_lastModified,

    -- ** RuntimeVersion
    runtimeVersion_versionName,
    runtimeVersion_deprecationDate,
    runtimeVersion_releaseDate,
    runtimeVersion_description,

    -- ** S3EncryptionConfig
    s3EncryptionConfig_kmsKeyArn,
    s3EncryptionConfig_encryptionMode,

    -- ** VisualReferenceInput
    visualReferenceInput_baseScreenshots,
    visualReferenceInput_baseCanaryRunId,

    -- ** VisualReferenceOutput
    visualReferenceOutput_baseScreenshots,
    visualReferenceOutput_baseCanaryRunId,

    -- ** VpcConfigInput
    vpcConfigInput_securityGroupIds,
    vpcConfigInput_subnetIds,

    -- ** VpcConfigOutput
    vpcConfigOutput_securityGroupIds,
    vpcConfigOutput_subnetIds,
    vpcConfigOutput_vpcId,
  )
where

import Network.AWS.Synthetics.CreateCanary
import Network.AWS.Synthetics.DeleteCanary
import Network.AWS.Synthetics.DescribeCanaries
import Network.AWS.Synthetics.DescribeCanariesLastRun
import Network.AWS.Synthetics.DescribeRuntimeVersions
import Network.AWS.Synthetics.GetCanary
import Network.AWS.Synthetics.GetCanaryRuns
import Network.AWS.Synthetics.ListTagsForResource
import Network.AWS.Synthetics.StartCanary
import Network.AWS.Synthetics.StopCanary
import Network.AWS.Synthetics.TagResource
import Network.AWS.Synthetics.Types.ArtifactConfigInput
import Network.AWS.Synthetics.Types.ArtifactConfigOutput
import Network.AWS.Synthetics.Types.BaseScreenshot
import Network.AWS.Synthetics.Types.Canary
import Network.AWS.Synthetics.Types.CanaryCodeInput
import Network.AWS.Synthetics.Types.CanaryCodeOutput
import Network.AWS.Synthetics.Types.CanaryLastRun
import Network.AWS.Synthetics.Types.CanaryRun
import Network.AWS.Synthetics.Types.CanaryRunConfigInput
import Network.AWS.Synthetics.Types.CanaryRunConfigOutput
import Network.AWS.Synthetics.Types.CanaryRunStatus
import Network.AWS.Synthetics.Types.CanaryRunTimeline
import Network.AWS.Synthetics.Types.CanaryScheduleInput
import Network.AWS.Synthetics.Types.CanaryScheduleOutput
import Network.AWS.Synthetics.Types.CanaryStatus
import Network.AWS.Synthetics.Types.CanaryTimeline
import Network.AWS.Synthetics.Types.RuntimeVersion
import Network.AWS.Synthetics.Types.S3EncryptionConfig
import Network.AWS.Synthetics.Types.VisualReferenceInput
import Network.AWS.Synthetics.Types.VisualReferenceOutput
import Network.AWS.Synthetics.Types.VpcConfigInput
import Network.AWS.Synthetics.Types.VpcConfigOutput
import Network.AWS.Synthetics.UntagResource
import Network.AWS.Synthetics.UpdateCanary
