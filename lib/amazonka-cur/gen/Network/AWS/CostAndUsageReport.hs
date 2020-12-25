{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The AWS Cost and Usage Report API enables you to programmatically create, query, and delete AWS Cost and Usage report definitions.
--
-- AWS Cost and Usage reports track the monthly AWS costs and usage associated with your AWS account. The report contains line items for each unique combination of AWS product, usage type, and operation that your AWS account uses. You can configure the AWS Cost and Usage report to show only the data that you want, using the AWS Cost and Usage API.
-- Service Endpoint
-- The AWS Cost and Usage Report API provides the following endpoint:
--
--     * cur.us-east-1.amazonaws.com
module Network.AWS.CostAndUsageReport
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** DuplicateReportNameException
    _DuplicateReportNameException,

    -- ** ReportLimitReachedException
    _ReportLimitReachedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutReportDefinition
    module Network.AWS.CostAndUsageReport.PutReportDefinition,

    -- ** DeleteReportDefinition
    module Network.AWS.CostAndUsageReport.DeleteReportDefinition,

    -- ** ModifyReportDefinition
    module Network.AWS.CostAndUsageReport.ModifyReportDefinition,

    -- ** DescribeReportDefinitions (Paginated)
    module Network.AWS.CostAndUsageReport.DescribeReportDefinitions,

    -- * Types

    -- ** ReportVersioning
    ReportVersioning (..),

    -- ** TimeUnit
    TimeUnit (..),

    -- ** ReportName
    ReportName (..),

    -- ** GenericString
    GenericString (..),

    -- ** SchemaElement
    SchemaElement (..),

    -- ** AdditionalArtifact
    AdditionalArtifact (..),

    -- ** CompressionFormat
    CompressionFormat (..),

    -- ** AWSRegion
    AWSRegion (..),

    -- ** ReportFormat
    ReportFormat (..),

    -- ** S3Prefix
    S3Prefix (..),

    -- ** ReportDefinition
    ReportDefinition (..),
    mkReportDefinition,
    rdReportName,
    rdTimeUnit,
    rdFormat,
    rdCompression,
    rdAdditionalSchemaElements,
    rdS3Bucket,
    rdS3Prefix,
    rdS3Region,
    rdAdditionalArtifacts,
    rdRefreshClosedReports,
    rdReportVersioning,

    -- ** S3Bucket
    S3Bucket (..),

    -- ** NextToken
    NextToken (..),

    -- ** ResponseMessage
    ResponseMessage (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.CostAndUsageReport.DeleteReportDefinition
import Network.AWS.CostAndUsageReport.DescribeReportDefinitions
import Network.AWS.CostAndUsageReport.ModifyReportDefinition
import Network.AWS.CostAndUsageReport.PutReportDefinition
import Network.AWS.CostAndUsageReport.Types
import Network.AWS.CostAndUsageReport.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CostAndUsageReport'.

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
