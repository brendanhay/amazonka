{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Synthetics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudWatch Synthetics
--
-- You can use Amazon CloudWatch Synthetics to continually monitor your
-- services. You can create and manage /canaries/, which are modular,
-- lightweight scripts that monitor your endpoints and APIs from the
-- outside-in. You can set up your canaries to run 24 hours a day, once per
-- minute. The canaries help you check the availability and latency of your
-- web services and troubleshoot anomalies by investigating load time data,
-- screenshots of the UI, logs, and metrics. The canaries seamlessly
-- integrate with CloudWatch ServiceLens to help you trace the causes of
-- impacted nodes in your applications. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ServiceLens.html Using ServiceLens to Monitor the Health of Your Applications>
-- in the /Amazon CloudWatch User Guide/.
--
-- Before you create and manage canaries, be aware of the security
-- considerations. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/servicelens_canaries_security.html Security Considerations for Synthetics Canaries>.
module Network.AWS.Synthetics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateCanary
    UpdateCanary (UpdateCanary'),
    newUpdateCanary,
    UpdateCanaryResponse (UpdateCanaryResponse'),
    newUpdateCanaryResponse,

    -- ** DeleteCanary
    DeleteCanary (DeleteCanary'),
    newDeleteCanary,
    DeleteCanaryResponse (DeleteCanaryResponse'),
    newDeleteCanaryResponse,

    -- ** CreateCanary
    CreateCanary (CreateCanary'),
    newCreateCanary,
    CreateCanaryResponse (CreateCanaryResponse'),
    newCreateCanaryResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetCanaryRuns
    GetCanaryRuns (GetCanaryRuns'),
    newGetCanaryRuns,
    GetCanaryRunsResponse (GetCanaryRunsResponse'),
    newGetCanaryRunsResponse,

    -- ** GetCanary
    GetCanary (GetCanary'),
    newGetCanary,
    GetCanaryResponse (GetCanaryResponse'),
    newGetCanaryResponse,

    -- ** DescribeRuntimeVersions
    DescribeRuntimeVersions (DescribeRuntimeVersions'),
    newDescribeRuntimeVersions,
    DescribeRuntimeVersionsResponse (DescribeRuntimeVersionsResponse'),
    newDescribeRuntimeVersionsResponse,

    -- ** DescribeCanariesLastRun
    DescribeCanariesLastRun (DescribeCanariesLastRun'),
    newDescribeCanariesLastRun,
    DescribeCanariesLastRunResponse (DescribeCanariesLastRunResponse'),
    newDescribeCanariesLastRunResponse,

    -- ** StartCanary
    StartCanary (StartCanary'),
    newStartCanary,
    StartCanaryResponse (StartCanaryResponse'),
    newStartCanaryResponse,

    -- ** DescribeCanaries
    DescribeCanaries (DescribeCanaries'),
    newDescribeCanaries,
    DescribeCanariesResponse (DescribeCanariesResponse'),
    newDescribeCanariesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** StopCanary
    StopCanary (StopCanary'),
    newStopCanary,
    StopCanaryResponse (StopCanaryResponse'),
    newStopCanaryResponse,

    -- * Types

    -- ** CanaryRunState
    CanaryRunState (..),

    -- ** CanaryRunStateReasonCode
    CanaryRunStateReasonCode (..),

    -- ** CanaryState
    CanaryState (..),

    -- ** CanaryStateReasonCode
    CanaryStateReasonCode (..),

    -- ** EncryptionMode
    EncryptionMode (..),

    -- ** ArtifactConfigInput
    ArtifactConfigInput (ArtifactConfigInput'),
    newArtifactConfigInput,

    -- ** ArtifactConfigOutput
    ArtifactConfigOutput (ArtifactConfigOutput'),
    newArtifactConfigOutput,

    -- ** BaseScreenshot
    BaseScreenshot (BaseScreenshot'),
    newBaseScreenshot,

    -- ** Canary
    Canary (Canary'),
    newCanary,

    -- ** CanaryCodeInput
    CanaryCodeInput (CanaryCodeInput'),
    newCanaryCodeInput,

    -- ** CanaryCodeOutput
    CanaryCodeOutput (CanaryCodeOutput'),
    newCanaryCodeOutput,

    -- ** CanaryLastRun
    CanaryLastRun (CanaryLastRun'),
    newCanaryLastRun,

    -- ** CanaryRun
    CanaryRun (CanaryRun'),
    newCanaryRun,

    -- ** CanaryRunConfigInput
    CanaryRunConfigInput (CanaryRunConfigInput'),
    newCanaryRunConfigInput,

    -- ** CanaryRunConfigOutput
    CanaryRunConfigOutput (CanaryRunConfigOutput'),
    newCanaryRunConfigOutput,

    -- ** CanaryRunStatus
    CanaryRunStatus (CanaryRunStatus'),
    newCanaryRunStatus,

    -- ** CanaryRunTimeline
    CanaryRunTimeline (CanaryRunTimeline'),
    newCanaryRunTimeline,

    -- ** CanaryScheduleInput
    CanaryScheduleInput (CanaryScheduleInput'),
    newCanaryScheduleInput,

    -- ** CanaryScheduleOutput
    CanaryScheduleOutput (CanaryScheduleOutput'),
    newCanaryScheduleOutput,

    -- ** CanaryStatus
    CanaryStatus (CanaryStatus'),
    newCanaryStatus,

    -- ** CanaryTimeline
    CanaryTimeline (CanaryTimeline'),
    newCanaryTimeline,

    -- ** RuntimeVersion
    RuntimeVersion (RuntimeVersion'),
    newRuntimeVersion,

    -- ** S3EncryptionConfig
    S3EncryptionConfig (S3EncryptionConfig'),
    newS3EncryptionConfig,

    -- ** VisualReferenceInput
    VisualReferenceInput (VisualReferenceInput'),
    newVisualReferenceInput,

    -- ** VisualReferenceOutput
    VisualReferenceOutput (VisualReferenceOutput'),
    newVisualReferenceOutput,

    -- ** VpcConfigInput
    VpcConfigInput (VpcConfigInput'),
    newVpcConfigInput,

    -- ** VpcConfigOutput
    VpcConfigOutput (VpcConfigOutput'),
    newVpcConfigOutput,
  )
where

import Network.AWS.Synthetics.CreateCanary
import Network.AWS.Synthetics.DeleteCanary
import Network.AWS.Synthetics.DescribeCanaries
import Network.AWS.Synthetics.DescribeCanariesLastRun
import Network.AWS.Synthetics.DescribeRuntimeVersions
import Network.AWS.Synthetics.GetCanary
import Network.AWS.Synthetics.GetCanaryRuns
import Network.AWS.Synthetics.Lens
import Network.AWS.Synthetics.ListTagsForResource
import Network.AWS.Synthetics.StartCanary
import Network.AWS.Synthetics.StopCanary
import Network.AWS.Synthetics.TagResource
import Network.AWS.Synthetics.Types
import Network.AWS.Synthetics.UntagResource
import Network.AWS.Synthetics.UpdateCanary
import Network.AWS.Synthetics.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Synthetics'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
