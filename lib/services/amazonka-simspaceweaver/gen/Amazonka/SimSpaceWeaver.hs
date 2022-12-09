{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SimSpaceWeaver
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-10-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services SimSpace Weaver (SimSpace Weaver) is a managed
-- service that you can use to build and operate large-scale spatial
-- simulations in the Amazon Web Services Cloud. For example, you can
-- create a digital twin of a city, crowd simulations with millions of
-- people and objects, and massilvely-multiplayer games with hundreds of
-- thousands of connected players. For more information about SimSpace
-- Weaver, see the
-- /<https://docs.aws.amazon.com/simspaceweaver/latest/userguide/ Amazon Web Services SimSpace Weaver User Guide>/
-- .
--
-- This API reference describes the API operations and data types that you
-- can use to communicate directly with SimSpace Weaver.
--
-- SimSpace Weaver also provides the SimSpace Weaver app SDK, which you use
-- for app development. The SimSpace Weaver app SDK API reference is
-- included in the SimSpace Weaver app SDK documentation, which is part of
-- the SimSpace Weaver app SDK distributable package.
module Amazonka.SimSpaceWeaver
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteSimulation
    DeleteSimulation (DeleteSimulation'),
    newDeleteSimulation,
    DeleteSimulationResponse (DeleteSimulationResponse'),
    newDeleteSimulationResponse,

    -- ** DescribeApp
    DescribeApp (DescribeApp'),
    newDescribeApp,
    DescribeAppResponse (DescribeAppResponse'),
    newDescribeAppResponse,

    -- ** DescribeSimulation
    DescribeSimulation (DescribeSimulation'),
    newDescribeSimulation,
    DescribeSimulationResponse (DescribeSimulationResponse'),
    newDescribeSimulationResponse,

    -- ** ListApps
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** ListSimulations
    ListSimulations (ListSimulations'),
    newListSimulations,
    ListSimulationsResponse (ListSimulationsResponse'),
    newListSimulationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartApp
    StartApp (StartApp'),
    newStartApp,
    StartAppResponse (StartAppResponse'),
    newStartAppResponse,

    -- ** StartClock
    StartClock (StartClock'),
    newStartClock,
    StartClockResponse (StartClockResponse'),
    newStartClockResponse,

    -- ** StartSimulation
    StartSimulation (StartSimulation'),
    newStartSimulation,
    StartSimulationResponse (StartSimulationResponse'),
    newStartSimulationResponse,

    -- ** StopApp
    StopApp (StopApp'),
    newStopApp,
    StopAppResponse (StopAppResponse'),
    newStopAppResponse,

    -- ** StopClock
    StopClock (StopClock'),
    newStopClock,
    StopClockResponse (StopClockResponse'),
    newStopClockResponse,

    -- ** StopSimulation
    StopSimulation (StopSimulation'),
    newStopSimulation,
    StopSimulationResponse (StopSimulationResponse'),
    newStopSimulationResponse,

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

    -- * Types

    -- ** ClockStatus
    ClockStatus (..),

    -- ** ClockTargetStatus
    ClockTargetStatus (..),

    -- ** LifecycleManagementStrategy
    LifecycleManagementStrategy (..),

    -- ** SimulationAppStatus
    SimulationAppStatus (..),

    -- ** SimulationAppTargetStatus
    SimulationAppTargetStatus (..),

    -- ** SimulationStatus
    SimulationStatus (..),

    -- ** SimulationTargetStatus
    SimulationTargetStatus (..),

    -- ** CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup (CloudWatchLogsLogGroup'),
    newCloudWatchLogsLogGroup,

    -- ** Domain
    Domain (Domain'),
    newDomain,

    -- ** LaunchOverrides
    LaunchOverrides (LaunchOverrides'),
    newLaunchOverrides,

    -- ** LiveSimulationState
    LiveSimulationState (LiveSimulationState'),
    newLiveSimulationState,

    -- ** LogDestination
    LogDestination (LogDestination'),
    newLogDestination,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SimulationAppEndpointInfo
    SimulationAppEndpointInfo (SimulationAppEndpointInfo'),
    newSimulationAppEndpointInfo,

    -- ** SimulationAppMetadata
    SimulationAppMetadata (SimulationAppMetadata'),
    newSimulationAppMetadata,

    -- ** SimulationAppPortMapping
    SimulationAppPortMapping (SimulationAppPortMapping'),
    newSimulationAppPortMapping,

    -- ** SimulationClock
    SimulationClock (SimulationClock'),
    newSimulationClock,

    -- ** SimulationMetadata
    SimulationMetadata (SimulationMetadata'),
    newSimulationMetadata,
  )
where

import Amazonka.SimSpaceWeaver.DeleteApp
import Amazonka.SimSpaceWeaver.DeleteSimulation
import Amazonka.SimSpaceWeaver.DescribeApp
import Amazonka.SimSpaceWeaver.DescribeSimulation
import Amazonka.SimSpaceWeaver.Lens
import Amazonka.SimSpaceWeaver.ListApps
import Amazonka.SimSpaceWeaver.ListSimulations
import Amazonka.SimSpaceWeaver.ListTagsForResource
import Amazonka.SimSpaceWeaver.StartApp
import Amazonka.SimSpaceWeaver.StartClock
import Amazonka.SimSpaceWeaver.StartSimulation
import Amazonka.SimSpaceWeaver.StopApp
import Amazonka.SimSpaceWeaver.StopClock
import Amazonka.SimSpaceWeaver.StopSimulation
import Amazonka.SimSpaceWeaver.TagResource
import Amazonka.SimSpaceWeaver.Types
import Amazonka.SimSpaceWeaver.UntagResource
import Amazonka.SimSpaceWeaver.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SimSpaceWeaver'.

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
