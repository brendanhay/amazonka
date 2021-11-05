{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Synthetics.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Lens
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

import Amazonka.Synthetics.CreateCanary
import Amazonka.Synthetics.DeleteCanary
import Amazonka.Synthetics.DescribeCanaries
import Amazonka.Synthetics.DescribeCanariesLastRun
import Amazonka.Synthetics.DescribeRuntimeVersions
import Amazonka.Synthetics.GetCanary
import Amazonka.Synthetics.GetCanaryRuns
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
import Amazonka.Synthetics.Types.RuntimeVersion
import Amazonka.Synthetics.Types.S3EncryptionConfig
import Amazonka.Synthetics.Types.VisualReferenceInput
import Amazonka.Synthetics.Types.VisualReferenceOutput
import Amazonka.Synthetics.Types.VpcConfigInput
import Amazonka.Synthetics.Types.VpcConfigOutput
import Amazonka.Synthetics.UntagResource
import Amazonka.Synthetics.UpdateCanary
