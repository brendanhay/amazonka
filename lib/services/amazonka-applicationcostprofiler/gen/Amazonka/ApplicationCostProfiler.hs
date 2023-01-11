{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ApplicationCostProfiler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-09-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This reference provides descriptions of the AWS Application Cost
-- Profiler API.
--
-- The AWS Application Cost Profiler API provides programmatic access to
-- view, create, update, and delete application cost report definitions, as
-- well as to import your usage data into the Application Cost Profiler
-- service.
--
-- For more information about using this service, see the
-- <https://docs.aws.amazon.com/application-cost-profiler/latest/userguide/introduction.html AWS Application Cost Profiler User Guide>.
module Amazonka.ApplicationCostProfiler
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

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

    -- ** DeleteReportDefinition
    DeleteReportDefinition (DeleteReportDefinition'),
    newDeleteReportDefinition,
    DeleteReportDefinitionResponse (DeleteReportDefinitionResponse'),
    newDeleteReportDefinitionResponse,

    -- ** GetReportDefinition
    GetReportDefinition (GetReportDefinition'),
    newGetReportDefinition,
    GetReportDefinitionResponse (GetReportDefinitionResponse'),
    newGetReportDefinitionResponse,

    -- ** ImportApplicationUsage
    ImportApplicationUsage (ImportApplicationUsage'),
    newImportApplicationUsage,
    ImportApplicationUsageResponse (ImportApplicationUsageResponse'),
    newImportApplicationUsageResponse,

    -- ** ListReportDefinitions (Paginated)
    ListReportDefinitions (ListReportDefinitions'),
    newListReportDefinitions,
    ListReportDefinitionsResponse (ListReportDefinitionsResponse'),
    newListReportDefinitionsResponse,

    -- ** PutReportDefinition
    PutReportDefinition (PutReportDefinition'),
    newPutReportDefinition,
    PutReportDefinitionResponse (PutReportDefinitionResponse'),
    newPutReportDefinitionResponse,

    -- ** UpdateReportDefinition
    UpdateReportDefinition (UpdateReportDefinition'),
    newUpdateReportDefinition,
    UpdateReportDefinitionResponse (UpdateReportDefinitionResponse'),
    newUpdateReportDefinitionResponse,

    -- * Types

    -- ** Format
    Format (..),

    -- ** ReportFrequency
    ReportFrequency (..),

    -- ** S3BucketRegion
    S3BucketRegion (..),

    -- ** ReportDefinition
    ReportDefinition (ReportDefinition'),
    newReportDefinition,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SourceS3Location
    SourceS3Location (SourceS3Location'),
    newSourceS3Location,
  )
where

import Amazonka.ApplicationCostProfiler.DeleteReportDefinition
import Amazonka.ApplicationCostProfiler.GetReportDefinition
import Amazonka.ApplicationCostProfiler.ImportApplicationUsage
import Amazonka.ApplicationCostProfiler.Lens
import Amazonka.ApplicationCostProfiler.ListReportDefinitions
import Amazonka.ApplicationCostProfiler.PutReportDefinition
import Amazonka.ApplicationCostProfiler.Types
import Amazonka.ApplicationCostProfiler.UpdateReportDefinition
import Amazonka.ApplicationCostProfiler.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ApplicationCostProfiler'.

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
