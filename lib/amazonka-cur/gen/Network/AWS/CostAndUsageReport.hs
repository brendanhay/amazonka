{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- All public APIs for AWS Cost and Usage Report service
module Network.AWS.CostAndUsageReport
    (
    -- * Service Configuration
      costAndUsageReport

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** DuplicateReportNameException
    , _DuplicateReportNameException

    -- ** ReportLimitReachedException
    , _ReportLimitReachedException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutReportDefinition
    , module Network.AWS.CostAndUsageReport.PutReportDefinition

    -- ** DeleteReportDefinition
    , module Network.AWS.CostAndUsageReport.DeleteReportDefinition

    -- ** DescribeReportDefinitions (Paginated)
    , module Network.AWS.CostAndUsageReport.DescribeReportDefinitions

    -- * Types

    -- ** AWSRegion
    , AWSRegion (..)

    -- ** AdditionalArtifact
    , AdditionalArtifact (..)

    -- ** CompressionFormat
    , CompressionFormat (..)

    -- ** ReportFormat
    , ReportFormat (..)

    -- ** SchemaElement
    , SchemaElement (..)

    -- ** TimeUnit
    , TimeUnit (..)

    -- ** ReportDefinition
    , ReportDefinition
    , reportDefinition
    , rdAdditionalArtifacts
    , rdReportName
    , rdTimeUnit
    , rdFormat
    , rdCompression
    , rdAdditionalSchemaElements
    , rdS3Bucket
    , rdS3Prefix
    , rdS3Region
    ) where

import Network.AWS.CostAndUsageReport.DeleteReportDefinition
import Network.AWS.CostAndUsageReport.DescribeReportDefinitions
import Network.AWS.CostAndUsageReport.PutReportDefinition
import Network.AWS.CostAndUsageReport.Types
import Network.AWS.CostAndUsageReport.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CostAndUsageReport'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
