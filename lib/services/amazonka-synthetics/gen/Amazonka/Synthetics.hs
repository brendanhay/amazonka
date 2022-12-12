{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Synthetics
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Synthetics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** RequestEntityTooLargeException
    _RequestEntityTooLargeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateResource
    AssociateResource (AssociateResource'),
    newAssociateResource,
    AssociateResourceResponse (AssociateResourceResponse'),
    newAssociateResourceResponse,

    -- ** CreateCanary
    CreateCanary (CreateCanary'),
    newCreateCanary,
    CreateCanaryResponse (CreateCanaryResponse'),
    newCreateCanaryResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** DeleteCanary
    DeleteCanary (DeleteCanary'),
    newDeleteCanary,
    DeleteCanaryResponse (DeleteCanaryResponse'),
    newDeleteCanaryResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DescribeCanaries
    DescribeCanaries (DescribeCanaries'),
    newDescribeCanaries,
    DescribeCanariesResponse (DescribeCanariesResponse'),
    newDescribeCanariesResponse,

    -- ** DescribeCanariesLastRun
    DescribeCanariesLastRun (DescribeCanariesLastRun'),
    newDescribeCanariesLastRun,
    DescribeCanariesLastRunResponse (DescribeCanariesLastRunResponse'),
    newDescribeCanariesLastRunResponse,

    -- ** DescribeRuntimeVersions
    DescribeRuntimeVersions (DescribeRuntimeVersions'),
    newDescribeRuntimeVersions,
    DescribeRuntimeVersionsResponse (DescribeRuntimeVersionsResponse'),
    newDescribeRuntimeVersionsResponse,

    -- ** DisassociateResource
    DisassociateResource (DisassociateResource'),
    newDisassociateResource,
    DisassociateResourceResponse (DisassociateResourceResponse'),
    newDisassociateResourceResponse,

    -- ** GetCanary
    GetCanary (GetCanary'),
    newGetCanary,
    GetCanaryResponse (GetCanaryResponse'),
    newGetCanaryResponse,

    -- ** GetCanaryRuns
    GetCanaryRuns (GetCanaryRuns'),
    newGetCanaryRuns,
    GetCanaryRunsResponse (GetCanaryRunsResponse'),
    newGetCanaryRunsResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** ListAssociatedGroups
    ListAssociatedGroups (ListAssociatedGroups'),
    newListAssociatedGroups,
    ListAssociatedGroupsResponse (ListAssociatedGroupsResponse'),
    newListAssociatedGroupsResponse,

    -- ** ListGroupResources
    ListGroupResources (ListGroupResources'),
    newListGroupResources,
    ListGroupResourcesResponse (ListGroupResourcesResponse'),
    newListGroupResourcesResponse,

    -- ** ListGroups
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartCanary
    StartCanary (StartCanary'),
    newStartCanary,
    StartCanaryResponse (StartCanaryResponse'),
    newStartCanaryResponse,

    -- ** StopCanary
    StopCanary (StopCanary'),
    newStopCanary,
    StopCanaryResponse (StopCanaryResponse'),
    newStopCanaryResponse,

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

    -- ** UpdateCanary
    UpdateCanary (UpdateCanary'),
    newUpdateCanary,
    UpdateCanaryResponse (UpdateCanaryResponse'),
    newUpdateCanaryResponse,

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

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupSummary
    GroupSummary (GroupSummary'),
    newGroupSummary,

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
import Amazonka.Synthetics.Lens
import Amazonka.Synthetics.ListAssociatedGroups
import Amazonka.Synthetics.ListGroupResources
import Amazonka.Synthetics.ListGroups
import Amazonka.Synthetics.ListTagsForResource
import Amazonka.Synthetics.StartCanary
import Amazonka.Synthetics.StopCanary
import Amazonka.Synthetics.TagResource
import Amazonka.Synthetics.Types
import Amazonka.Synthetics.UntagResource
import Amazonka.Synthetics.UpdateCanary
import Amazonka.Synthetics.Waiters

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
