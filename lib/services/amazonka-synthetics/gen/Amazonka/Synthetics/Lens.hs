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

    -- ** CreateCanary
    createCanary_tags,
    createCanary_vpcConfig,
    createCanary_artifactConfig,
    createCanary_failureRetentionPeriodInDays,
    createCanary_successRetentionPeriodInDays,
    createCanary_runConfig,
    createCanary_name,
    createCanary_code,
    createCanary_artifactS3Location,
    createCanary_executionRoleArn,
    createCanary_schedule,
    createCanary_runtimeVersion,
    createCanaryResponse_canary,
    createCanaryResponse_httpStatus,

    -- ** DeleteCanary
    deleteCanary_name,
    deleteCanaryResponse_httpStatus,

    -- ** DescribeCanaries
    describeCanaries_nextToken,
    describeCanaries_maxResults,
    describeCanariesResponse_nextToken,
    describeCanariesResponse_canaries,
    describeCanariesResponse_httpStatus,

    -- ** DescribeCanariesLastRun
    describeCanariesLastRun_nextToken,
    describeCanariesLastRun_maxResults,
    describeCanariesLastRunResponse_nextToken,
    describeCanariesLastRunResponse_canariesLastRun,
    describeCanariesLastRunResponse_httpStatus,

    -- ** DescribeRuntimeVersions
    describeRuntimeVersions_nextToken,
    describeRuntimeVersions_maxResults,
    describeRuntimeVersionsResponse_nextToken,
    describeRuntimeVersionsResponse_runtimeVersions,
    describeRuntimeVersionsResponse_httpStatus,

    -- ** GetCanary
    getCanary_name,
    getCanaryResponse_canary,
    getCanaryResponse_httpStatus,

    -- ** GetCanaryRuns
    getCanaryRuns_nextToken,
    getCanaryRuns_maxResults,
    getCanaryRuns_name,
    getCanaryRunsResponse_nextToken,
    getCanaryRunsResponse_canaryRuns,
    getCanaryRunsResponse_httpStatus,

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
    updateCanary_schedule,
    updateCanary_code,
    updateCanary_vpcConfig,
    updateCanary_visualReference,
    updateCanary_artifactConfig,
    updateCanary_failureRetentionPeriodInDays,
    updateCanary_successRetentionPeriodInDays,
    updateCanary_executionRoleArn,
    updateCanary_runConfig,
    updateCanary_artifactS3Location,
    updateCanary_runtimeVersion,
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
    canary_tags,
    canary_schedule,
    canary_name,
    canary_code,
    canary_vpcConfig,
    canary_timeline,
    canary_visualReference,
    canary_artifactConfig,
    canary_status,
    canary_id,
    canary_failureRetentionPeriodInDays,
    canary_successRetentionPeriodInDays,
    canary_engineArn,
    canary_executionRoleArn,
    canary_runConfig,
    canary_artifactS3Location,
    canary_runtimeVersion,

    -- ** CanaryCodeInput
    canaryCodeInput_s3Bucket,
    canaryCodeInput_s3Version,
    canaryCodeInput_s3Key,
    canaryCodeInput_zipFile,
    canaryCodeInput_handler,

    -- ** CanaryCodeOutput
    canaryCodeOutput_sourceLocationArn,
    canaryCodeOutput_handler,

    -- ** CanaryLastRun
    canaryLastRun_lastRun,
    canaryLastRun_canaryName,

    -- ** CanaryRun
    canaryRun_name,
    canaryRun_timeline,
    canaryRun_status,
    canaryRun_id,
    canaryRun_artifactS3Location,

    -- ** CanaryRunConfigInput
    canaryRunConfigInput_activeTracing,
    canaryRunConfigInput_timeoutInSeconds,
    canaryRunConfigInput_environmentVariables,
    canaryRunConfigInput_memoryInMB,

    -- ** CanaryRunConfigOutput
    canaryRunConfigOutput_activeTracing,
    canaryRunConfigOutput_timeoutInSeconds,
    canaryRunConfigOutput_memoryInMB,

    -- ** CanaryRunStatus
    canaryRunStatus_state,
    canaryRunStatus_stateReasonCode,
    canaryRunStatus_stateReason,

    -- ** CanaryRunTimeline
    canaryRunTimeline_started,
    canaryRunTimeline_completed,

    -- ** CanaryScheduleInput
    canaryScheduleInput_durationInSeconds,
    canaryScheduleInput_expression,

    -- ** CanaryScheduleOutput
    canaryScheduleOutput_expression,
    canaryScheduleOutput_durationInSeconds,

    -- ** CanaryStatus
    canaryStatus_state,
    canaryStatus_stateReasonCode,
    canaryStatus_stateReason,

    -- ** CanaryTimeline
    canaryTimeline_lastStarted,
    canaryTimeline_created,
    canaryTimeline_lastModified,
    canaryTimeline_lastStopped,

    -- ** RuntimeVersion
    runtimeVersion_releaseDate,
    runtimeVersion_deprecationDate,
    runtimeVersion_description,
    runtimeVersion_versionName,

    -- ** S3EncryptionConfig
    s3EncryptionConfig_kmsKeyArn,
    s3EncryptionConfig_encryptionMode,

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
    vpcConfigOutput_vpcId,
    vpcConfigOutput_subnetIds,
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
