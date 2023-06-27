{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MwAA
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Managed Workflows for Apache Airflow
--
-- This section contains the Amazon Managed Workflows for Apache Airflow
-- (MWAA) API reference documentation. For more information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/what-is-mwaa.html What is Amazon MWAA?>.
--
-- __Endpoints__
--
-- -   @api.airflow.{region}.amazonaws.com@ - This endpoint is used for
--     environment management.
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_CreateEnvironment.html CreateEnvironment>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_DeleteEnvironment.html DeleteEnvironment>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_GetEnvironment.html GetEnvironment>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_ListEnvironments.html ListEnvironments>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_ListTagsForResource.html ListTagsForResource>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_TagResource.html TagResource>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_UntagResource.html UntagResource>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_UpdateEnvironment.html UpdateEnvironment>
--
-- -   @env.airflow.{region}.amazonaws.com@ - This endpoint is used to
--     operate the Airflow environment.
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_CreateCliToken.html%20 CreateCliToken>
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_CreateWebLoginToken.html CreateWebLoginToken>
--
-- -   @ops.airflow.{region}.amazonaws.com@ - This endpoint is used to push
--     environment metrics that track environment health.
--
--     -   <https://docs.aws.amazon.com/mwaa/latest/API/API_PublishMetrics.html%20 PublishMetrics>
--
-- __Regions__
--
-- For a list of regions that Amazon MWAA supports, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/what-is-mwaa.html#regions-mwaa Region availability>
-- in the /Amazon MWAA User Guide/.
module Amazonka.MwAA
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateCliToken
    CreateCliToken (CreateCliToken'),
    newCreateCliToken,
    CreateCliTokenResponse (CreateCliTokenResponse'),
    newCreateCliTokenResponse,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    CreateEnvironmentResponse (CreateEnvironmentResponse'),
    newCreateEnvironmentResponse,

    -- ** CreateWebLoginToken
    CreateWebLoginToken (CreateWebLoginToken'),
    newCreateWebLoginToken,
    CreateWebLoginTokenResponse (CreateWebLoginTokenResponse'),
    newCreateWebLoginTokenResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** GetEnvironment
    GetEnvironment (GetEnvironment'),
    newGetEnvironment,
    GetEnvironmentResponse (GetEnvironmentResponse'),
    newGetEnvironmentResponse,

    -- ** ListEnvironments (Paginated)
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PublishMetrics
    PublishMetrics (PublishMetrics'),
    newPublishMetrics,
    PublishMetricsResponse (PublishMetricsResponse'),
    newPublishMetricsResponse,

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

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    UpdateEnvironmentResponse (UpdateEnvironmentResponse'),
    newUpdateEnvironmentResponse,

    -- * Types

    -- ** EnvironmentStatus
    EnvironmentStatus (..),

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** Unit
    Unit (..),

    -- ** UpdateStatus
    UpdateStatus (..),

    -- ** WebserverAccessMode
    WebserverAccessMode (..),

    -- ** Dimension
    Dimension (Dimension'),
    newDimension,

    -- ** Environment
    Environment (Environment'),
    newEnvironment,

    -- ** LastUpdate
    LastUpdate (LastUpdate'),
    newLastUpdate,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** LoggingConfigurationInput
    LoggingConfigurationInput (LoggingConfigurationInput'),
    newLoggingConfigurationInput,

    -- ** MetricDatum
    MetricDatum (MetricDatum'),
    newMetricDatum,

    -- ** ModuleLoggingConfiguration
    ModuleLoggingConfiguration (ModuleLoggingConfiguration'),
    newModuleLoggingConfiguration,

    -- ** ModuleLoggingConfigurationInput
    ModuleLoggingConfigurationInput (ModuleLoggingConfigurationInput'),
    newModuleLoggingConfigurationInput,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** StatisticSet
    StatisticSet (StatisticSet'),
    newStatisticSet,

    -- ** UpdateError
    UpdateError (UpdateError'),
    newUpdateError,

    -- ** UpdateNetworkConfigurationInput
    UpdateNetworkConfigurationInput (UpdateNetworkConfigurationInput'),
    newUpdateNetworkConfigurationInput,
  )
where

import Amazonka.MwAA.CreateCliToken
import Amazonka.MwAA.CreateEnvironment
import Amazonka.MwAA.CreateWebLoginToken
import Amazonka.MwAA.DeleteEnvironment
import Amazonka.MwAA.GetEnvironment
import Amazonka.MwAA.Lens
import Amazonka.MwAA.ListEnvironments
import Amazonka.MwAA.ListTagsForResource
import Amazonka.MwAA.PublishMetrics
import Amazonka.MwAA.TagResource
import Amazonka.MwAA.Types
import Amazonka.MwAA.UntagResource
import Amazonka.MwAA.UpdateEnvironment
import Amazonka.MwAA.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MwAA'.

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
