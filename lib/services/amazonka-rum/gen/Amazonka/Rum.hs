{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Rum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- With Amazon CloudWatch RUM, you can perform real-user monitoring to
-- collect client-side data about your web application performance from
-- actual user sessions in real time. The data collected includes page load
-- times, client-side errors, and user behavior. When you view this data,
-- you can see it all aggregated together and also see breakdowns by the
-- browsers and devices that your customers use.
--
-- You can use the collected data to quickly identify and debug client-side
-- performance issues. CloudWatch RUM helps you visualize anomalies in your
-- application performance and find relevant debugging data such as error
-- messages, stack traces, and user sessions. You can also use RUM to
-- understand the range of end-user impact including the number of users,
-- geolocations, and browsers used.
module Amazonka.Rum
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

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchCreateRumMetricDefinitions
    BatchCreateRumMetricDefinitions (BatchCreateRumMetricDefinitions'),
    newBatchCreateRumMetricDefinitions,
    BatchCreateRumMetricDefinitionsResponse (BatchCreateRumMetricDefinitionsResponse'),
    newBatchCreateRumMetricDefinitionsResponse,

    -- ** BatchDeleteRumMetricDefinitions
    BatchDeleteRumMetricDefinitions (BatchDeleteRumMetricDefinitions'),
    newBatchDeleteRumMetricDefinitions,
    BatchDeleteRumMetricDefinitionsResponse (BatchDeleteRumMetricDefinitionsResponse'),
    newBatchDeleteRumMetricDefinitionsResponse,

    -- ** BatchGetRumMetricDefinitions (Paginated)
    BatchGetRumMetricDefinitions (BatchGetRumMetricDefinitions'),
    newBatchGetRumMetricDefinitions,
    BatchGetRumMetricDefinitionsResponse (BatchGetRumMetricDefinitionsResponse'),
    newBatchGetRumMetricDefinitionsResponse,

    -- ** CreateAppMonitor
    CreateAppMonitor (CreateAppMonitor'),
    newCreateAppMonitor,
    CreateAppMonitorResponse (CreateAppMonitorResponse'),
    newCreateAppMonitorResponse,

    -- ** DeleteAppMonitor
    DeleteAppMonitor (DeleteAppMonitor'),
    newDeleteAppMonitor,
    DeleteAppMonitorResponse (DeleteAppMonitorResponse'),
    newDeleteAppMonitorResponse,

    -- ** DeleteRumMetricsDestination
    DeleteRumMetricsDestination (DeleteRumMetricsDestination'),
    newDeleteRumMetricsDestination,
    DeleteRumMetricsDestinationResponse (DeleteRumMetricsDestinationResponse'),
    newDeleteRumMetricsDestinationResponse,

    -- ** GetAppMonitor
    GetAppMonitor (GetAppMonitor'),
    newGetAppMonitor,
    GetAppMonitorResponse (GetAppMonitorResponse'),
    newGetAppMonitorResponse,

    -- ** GetAppMonitorData (Paginated)
    GetAppMonitorData (GetAppMonitorData'),
    newGetAppMonitorData,
    GetAppMonitorDataResponse (GetAppMonitorDataResponse'),
    newGetAppMonitorDataResponse,

    -- ** ListAppMonitors (Paginated)
    ListAppMonitors (ListAppMonitors'),
    newListAppMonitors,
    ListAppMonitorsResponse (ListAppMonitorsResponse'),
    newListAppMonitorsResponse,

    -- ** ListRumMetricsDestinations (Paginated)
    ListRumMetricsDestinations (ListRumMetricsDestinations'),
    newListRumMetricsDestinations,
    ListRumMetricsDestinationsResponse (ListRumMetricsDestinationsResponse'),
    newListRumMetricsDestinationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutRumEvents
    PutRumEvents (PutRumEvents'),
    newPutRumEvents,
    PutRumEventsResponse (PutRumEventsResponse'),
    newPutRumEventsResponse,

    -- ** PutRumMetricsDestination
    PutRumMetricsDestination (PutRumMetricsDestination'),
    newPutRumMetricsDestination,
    PutRumMetricsDestinationResponse (PutRumMetricsDestinationResponse'),
    newPutRumMetricsDestinationResponse,

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

    -- ** UpdateAppMonitor
    UpdateAppMonitor (UpdateAppMonitor'),
    newUpdateAppMonitor,
    UpdateAppMonitorResponse (UpdateAppMonitorResponse'),
    newUpdateAppMonitorResponse,

    -- ** UpdateRumMetricDefinition
    UpdateRumMetricDefinition (UpdateRumMetricDefinition'),
    newUpdateRumMetricDefinition,
    UpdateRumMetricDefinitionResponse (UpdateRumMetricDefinitionResponse'),
    newUpdateRumMetricDefinitionResponse,

    -- * Types

    -- ** CustomEventsStatus
    CustomEventsStatus (..),

    -- ** MetricDestination
    MetricDestination (..),

    -- ** StateEnum
    StateEnum (..),

    -- ** Telemetry
    Telemetry (..),

    -- ** AppMonitor
    AppMonitor (AppMonitor'),
    newAppMonitor,

    -- ** AppMonitorConfiguration
    AppMonitorConfiguration (AppMonitorConfiguration'),
    newAppMonitorConfiguration,

    -- ** AppMonitorDetails
    AppMonitorDetails (AppMonitorDetails'),
    newAppMonitorDetails,

    -- ** AppMonitorSummary
    AppMonitorSummary (AppMonitorSummary'),
    newAppMonitorSummary,

    -- ** BatchCreateRumMetricDefinitionsError
    BatchCreateRumMetricDefinitionsError (BatchCreateRumMetricDefinitionsError'),
    newBatchCreateRumMetricDefinitionsError,

    -- ** BatchDeleteRumMetricDefinitionsError
    BatchDeleteRumMetricDefinitionsError (BatchDeleteRumMetricDefinitionsError'),
    newBatchDeleteRumMetricDefinitionsError,

    -- ** CustomEvents
    CustomEvents (CustomEvents'),
    newCustomEvents,

    -- ** CwLog
    CwLog (CwLog'),
    newCwLog,

    -- ** DataStorage
    DataStorage (DataStorage'),
    newDataStorage,

    -- ** MetricDefinition
    MetricDefinition (MetricDefinition'),
    newMetricDefinition,

    -- ** MetricDefinitionRequest
    MetricDefinitionRequest (MetricDefinitionRequest'),
    newMetricDefinitionRequest,

    -- ** MetricDestinationSummary
    MetricDestinationSummary (MetricDestinationSummary'),
    newMetricDestinationSummary,

    -- ** QueryFilter
    QueryFilter (QueryFilter'),
    newQueryFilter,

    -- ** RumEvent
    RumEvent (RumEvent'),
    newRumEvent,

    -- ** TimeRange
    TimeRange (TimeRange'),
    newTimeRange,

    -- ** UserDetails
    UserDetails (UserDetails'),
    newUserDetails,
  )
where

import Amazonka.Rum.BatchCreateRumMetricDefinitions
import Amazonka.Rum.BatchDeleteRumMetricDefinitions
import Amazonka.Rum.BatchGetRumMetricDefinitions
import Amazonka.Rum.CreateAppMonitor
import Amazonka.Rum.DeleteAppMonitor
import Amazonka.Rum.DeleteRumMetricsDestination
import Amazonka.Rum.GetAppMonitor
import Amazonka.Rum.GetAppMonitorData
import Amazonka.Rum.Lens
import Amazonka.Rum.ListAppMonitors
import Amazonka.Rum.ListRumMetricsDestinations
import Amazonka.Rum.ListTagsForResource
import Amazonka.Rum.PutRumEvents
import Amazonka.Rum.PutRumMetricsDestination
import Amazonka.Rum.TagResource
import Amazonka.Rum.Types
import Amazonka.Rum.UntagResource
import Amazonka.Rum.UpdateAppMonitor
import Amazonka.Rum.UpdateRumMetricDefinition
import Amazonka.Rum.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Rum'.

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
