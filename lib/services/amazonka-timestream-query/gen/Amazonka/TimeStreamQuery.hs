{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.TimeStreamQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Timestream Query
module Amazonka.TimeStreamQuery
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

    -- ** InvalidEndpointException
    _InvalidEndpointException,

    -- ** QueryExecutionException
    _QueryExecutionException,

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

    -- ** CancelQuery
    CancelQuery (CancelQuery'),
    newCancelQuery,
    CancelQueryResponse (CancelQueryResponse'),
    newCancelQueryResponse,

    -- ** CreateScheduledQuery
    CreateScheduledQuery (CreateScheduledQuery'),
    newCreateScheduledQuery,
    CreateScheduledQueryResponse (CreateScheduledQueryResponse'),
    newCreateScheduledQueryResponse,

    -- ** DeleteScheduledQuery
    DeleteScheduledQuery (DeleteScheduledQuery'),
    newDeleteScheduledQuery,
    DeleteScheduledQueryResponse (DeleteScheduledQueryResponse'),
    newDeleteScheduledQueryResponse,

    -- ** DescribeEndpoints
    DescribeEndpoints (DescribeEndpoints'),
    newDescribeEndpoints,
    DescribeEndpointsResponse (DescribeEndpointsResponse'),
    newDescribeEndpointsResponse,

    -- ** DescribeScheduledQuery
    DescribeScheduledQuery (DescribeScheduledQuery'),
    newDescribeScheduledQuery,
    DescribeScheduledQueryResponse (DescribeScheduledQueryResponse'),
    newDescribeScheduledQueryResponse,

    -- ** ExecuteScheduledQuery
    ExecuteScheduledQuery (ExecuteScheduledQuery'),
    newExecuteScheduledQuery,
    ExecuteScheduledQueryResponse (ExecuteScheduledQueryResponse'),
    newExecuteScheduledQueryResponse,

    -- ** ListScheduledQueries (Paginated)
    ListScheduledQueries (ListScheduledQueries'),
    newListScheduledQueries,
    ListScheduledQueriesResponse (ListScheduledQueriesResponse'),
    newListScheduledQueriesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PrepareQuery
    PrepareQuery (PrepareQuery'),
    newPrepareQuery,
    PrepareQueryResponse (PrepareQueryResponse'),
    newPrepareQueryResponse,

    -- ** Query (Paginated)
    Query (Query'),
    newQuery,
    QueryResponse (QueryResponse'),
    newQueryResponse,

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

    -- ** UpdateScheduledQuery
    UpdateScheduledQuery (UpdateScheduledQuery'),
    newUpdateScheduledQuery,
    UpdateScheduledQueryResponse (UpdateScheduledQueryResponse'),
    newUpdateScheduledQueryResponse,

    -- * Types

    -- ** DimensionValueType
    DimensionValueType (..),

    -- ** MeasureValueType
    MeasureValueType (..),

    -- ** S3EncryptionOption
    S3EncryptionOption (..),

    -- ** ScalarMeasureValueType
    ScalarMeasureValueType (..),

    -- ** ScalarType
    ScalarType (..),

    -- ** ScheduledQueryRunStatus
    ScheduledQueryRunStatus (..),

    -- ** ScheduledQueryState
    ScheduledQueryState (..),

    -- ** ColumnInfo
    ColumnInfo (ColumnInfo'),
    newColumnInfo,

    -- ** Datum
    Datum (Datum'),
    newDatum,

    -- ** DimensionMapping
    DimensionMapping (DimensionMapping'),
    newDimensionMapping,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** ErrorReportConfiguration
    ErrorReportConfiguration (ErrorReportConfiguration'),
    newErrorReportConfiguration,

    -- ** ErrorReportLocation
    ErrorReportLocation (ErrorReportLocation'),
    newErrorReportLocation,

    -- ** ExecutionStats
    ExecutionStats (ExecutionStats'),
    newExecutionStats,

    -- ** MixedMeasureMapping
    MixedMeasureMapping (MixedMeasureMapping'),
    newMixedMeasureMapping,

    -- ** MultiMeasureAttributeMapping
    MultiMeasureAttributeMapping (MultiMeasureAttributeMapping'),
    newMultiMeasureAttributeMapping,

    -- ** MultiMeasureMappings
    MultiMeasureMappings (MultiMeasureMappings'),
    newMultiMeasureMappings,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** ParameterMapping
    ParameterMapping (ParameterMapping'),
    newParameterMapping,

    -- ** QueryStatus
    QueryStatus (QueryStatus'),
    newQueryStatus,

    -- ** Row
    Row (Row'),
    newRow,

    -- ** S3Configuration
    S3Configuration (S3Configuration'),
    newS3Configuration,

    -- ** S3ReportLocation
    S3ReportLocation (S3ReportLocation'),
    newS3ReportLocation,

    -- ** ScheduleConfiguration
    ScheduleConfiguration (ScheduleConfiguration'),
    newScheduleConfiguration,

    -- ** ScheduledQuery
    ScheduledQuery (ScheduledQuery'),
    newScheduledQuery,

    -- ** ScheduledQueryDescription
    ScheduledQueryDescription (ScheduledQueryDescription'),
    newScheduledQueryDescription,

    -- ** ScheduledQueryRunSummary
    ScheduledQueryRunSummary (ScheduledQueryRunSummary'),
    newScheduledQueryRunSummary,

    -- ** SelectColumn
    SelectColumn (SelectColumn'),
    newSelectColumn,

    -- ** SnsConfiguration
    SnsConfiguration (SnsConfiguration'),
    newSnsConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TargetConfiguration
    TargetConfiguration (TargetConfiguration'),
    newTargetConfiguration,

    -- ** TargetDestination
    TargetDestination (TargetDestination'),
    newTargetDestination,

    -- ** TimeSeriesDataPoint
    TimeSeriesDataPoint (TimeSeriesDataPoint'),
    newTimeSeriesDataPoint,

    -- ** TimestreamConfiguration
    TimestreamConfiguration (TimestreamConfiguration'),
    newTimestreamConfiguration,

    -- ** TimestreamDestination
    TimestreamDestination (TimestreamDestination'),
    newTimestreamDestination,

    -- ** Type
    Type (Type'),
    newType,
  )
where

import Amazonka.TimeStreamQuery.CancelQuery
import Amazonka.TimeStreamQuery.CreateScheduledQuery
import Amazonka.TimeStreamQuery.DeleteScheduledQuery
import Amazonka.TimeStreamQuery.DescribeEndpoints
import Amazonka.TimeStreamQuery.DescribeScheduledQuery
import Amazonka.TimeStreamQuery.ExecuteScheduledQuery
import Amazonka.TimeStreamQuery.Lens
import Amazonka.TimeStreamQuery.ListScheduledQueries
import Amazonka.TimeStreamQuery.ListTagsForResource
import Amazonka.TimeStreamQuery.PrepareQuery
import Amazonka.TimeStreamQuery.Query
import Amazonka.TimeStreamQuery.TagResource
import Amazonka.TimeStreamQuery.Types
import Amazonka.TimeStreamQuery.UntagResource
import Amazonka.TimeStreamQuery.UpdateScheduledQuery
import Amazonka.TimeStreamQuery.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'TimeStreamQuery'.

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
