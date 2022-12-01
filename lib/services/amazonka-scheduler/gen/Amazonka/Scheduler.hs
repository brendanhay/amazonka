{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Scheduler
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-06-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EventBridge Scheduler is a serverless scheduler that allows you
-- to create, run, and manage tasks from one central, managed service.
-- EventBridge Scheduler delivers your tasks reliably, with built-in
-- mechanisms that adjust your schedules based on the availability of
-- downstream targets. The following reference lists the available API
-- actions, and data types for EventBridge Scheduler.
module Amazonka.Scheduler
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateSchedule
    CreateSchedule (CreateSchedule'),
    newCreateSchedule,
    CreateScheduleResponse (CreateScheduleResponse'),
    newCreateScheduleResponse,

    -- ** CreateScheduleGroup
    CreateScheduleGroup (CreateScheduleGroup'),
    newCreateScheduleGroup,
    CreateScheduleGroupResponse (CreateScheduleGroupResponse'),
    newCreateScheduleGroupResponse,

    -- ** DeleteSchedule
    DeleteSchedule (DeleteSchedule'),
    newDeleteSchedule,
    DeleteScheduleResponse (DeleteScheduleResponse'),
    newDeleteScheduleResponse,

    -- ** DeleteScheduleGroup
    DeleteScheduleGroup (DeleteScheduleGroup'),
    newDeleteScheduleGroup,
    DeleteScheduleGroupResponse (DeleteScheduleGroupResponse'),
    newDeleteScheduleGroupResponse,

    -- ** GetSchedule
    GetSchedule (GetSchedule'),
    newGetSchedule,
    GetScheduleResponse (GetScheduleResponse'),
    newGetScheduleResponse,

    -- ** GetScheduleGroup
    GetScheduleGroup (GetScheduleGroup'),
    newGetScheduleGroup,
    GetScheduleGroupResponse (GetScheduleGroupResponse'),
    newGetScheduleGroupResponse,

    -- ** ListScheduleGroups (Paginated)
    ListScheduleGroups (ListScheduleGroups'),
    newListScheduleGroups,
    ListScheduleGroupsResponse (ListScheduleGroupsResponse'),
    newListScheduleGroupsResponse,

    -- ** ListSchedules (Paginated)
    ListSchedules (ListSchedules'),
    newListSchedules,
    ListSchedulesResponse (ListSchedulesResponse'),
    newListSchedulesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateSchedule
    UpdateSchedule (UpdateSchedule'),
    newUpdateSchedule,
    UpdateScheduleResponse (UpdateScheduleResponse'),
    newUpdateScheduleResponse,

    -- * Types

    -- ** AssignPublicIp
    AssignPublicIp (..),

    -- ** FlexibleTimeWindowMode
    FlexibleTimeWindowMode (..),

    -- ** LaunchType
    LaunchType (..),

    -- ** PlacementConstraintType
    PlacementConstraintType (..),

    -- ** PlacementStrategyType
    PlacementStrategyType (..),

    -- ** PropagateTags
    PropagateTags (..),

    -- ** ScheduleGroupState
    ScheduleGroupState (..),

    -- ** ScheduleState
    ScheduleState (..),

    -- ** AwsVpcConfiguration
    AwsVpcConfiguration (AwsVpcConfiguration'),
    newAwsVpcConfiguration,

    -- ** CapacityProviderStrategyItem
    CapacityProviderStrategyItem (CapacityProviderStrategyItem'),
    newCapacityProviderStrategyItem,

    -- ** DeadLetterConfig
    DeadLetterConfig (DeadLetterConfig'),
    newDeadLetterConfig,

    -- ** EcsParameters
    EcsParameters (EcsParameters'),
    newEcsParameters,

    -- ** EventBridgeParameters
    EventBridgeParameters (EventBridgeParameters'),
    newEventBridgeParameters,

    -- ** FlexibleTimeWindow
    FlexibleTimeWindow (FlexibleTimeWindow'),
    newFlexibleTimeWindow,

    -- ** KinesisParameters
    KinesisParameters (KinesisParameters'),
    newKinesisParameters,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** PlacementConstraint
    PlacementConstraint (PlacementConstraint'),
    newPlacementConstraint,

    -- ** PlacementStrategy
    PlacementStrategy (PlacementStrategy'),
    newPlacementStrategy,

    -- ** RetryPolicy
    RetryPolicy (RetryPolicy'),
    newRetryPolicy,

    -- ** SageMakerPipelineParameter
    SageMakerPipelineParameter (SageMakerPipelineParameter'),
    newSageMakerPipelineParameter,

    -- ** SageMakerPipelineParameters
    SageMakerPipelineParameters (SageMakerPipelineParameters'),
    newSageMakerPipelineParameters,

    -- ** ScheduleGroupSummary
    ScheduleGroupSummary (ScheduleGroupSummary'),
    newScheduleGroupSummary,

    -- ** ScheduleSummary
    ScheduleSummary (ScheduleSummary'),
    newScheduleSummary,

    -- ** SqsParameters
    SqsParameters (SqsParameters'),
    newSqsParameters,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Target
    Target (Target'),
    newTarget,

    -- ** TargetSummary
    TargetSummary (TargetSummary'),
    newTargetSummary,
  )
where

import Amazonka.Scheduler.CreateSchedule
import Amazonka.Scheduler.CreateScheduleGroup
import Amazonka.Scheduler.DeleteSchedule
import Amazonka.Scheduler.DeleteScheduleGroup
import Amazonka.Scheduler.GetSchedule
import Amazonka.Scheduler.GetScheduleGroup
import Amazonka.Scheduler.Lens
import Amazonka.Scheduler.ListScheduleGroups
import Amazonka.Scheduler.ListSchedules
import Amazonka.Scheduler.ListTagsForResource
import Amazonka.Scheduler.TagResource
import Amazonka.Scheduler.Types
import Amazonka.Scheduler.UntagResource
import Amazonka.Scheduler.UpdateSchedule
import Amazonka.Scheduler.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Scheduler'.

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
