{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Service Catalog
--
-- __Overview__
--
-- <https://aws.amazon.com/servicecatalog/ AWS Service Catalog> allows organizations to create and manage catalogs of IT services that are approved for use on AWS. This documentation provides reference material for the AWS Service Catalog end user API. To get the most out of this documentation, you need to be familiar with the terminology discussed in <http://docs.aws.amazon.com/servicecatalog/latest/userguide/what-is_concepts.html AWS Service Catalog Concepts>.
--
-- /Additional Resources/
--
-- -   <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html AWS Service Catalog Administrator Guide>
--
-- -   <http://docs.aws.amazon.com/servicecatalog/latest/userguide/introduction.html AWS Service Catalog User Guide>
--
module Network.AWS.ServiceCatalog
    (
    -- * Service Configuration
      serviceCatalog

    -- * Errors
    -- $errors

    -- ** InvalidParametersException
    , _InvalidParametersException

    -- ** DuplicateResourceException
    , _DuplicateResourceException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** TerminateProvisionedProduct
    , module Network.AWS.ServiceCatalog.TerminateProvisionedProduct

    -- ** UpdateProvisionedProduct
    , module Network.AWS.ServiceCatalog.UpdateProvisionedProduct

    -- ** ListRecordHistory
    , module Network.AWS.ServiceCatalog.ListRecordHistory

    -- ** DescribeRecord
    , module Network.AWS.ServiceCatalog.DescribeRecord

    -- ** DescribeProductView
    , module Network.AWS.ServiceCatalog.DescribeProductView

    -- ** SearchProducts
    , module Network.AWS.ServiceCatalog.SearchProducts

    -- ** DescribeProduct
    , module Network.AWS.ServiceCatalog.DescribeProduct

    -- ** ScanProvisionedProducts
    , module Network.AWS.ServiceCatalog.ScanProvisionedProducts

    -- ** ProvisionProduct
    , module Network.AWS.ServiceCatalog.ProvisionProduct

    -- ** DescribeProvisioningParameters
    , module Network.AWS.ServiceCatalog.DescribeProvisioningParameters

    -- ** ListLaunchPaths
    , module Network.AWS.ServiceCatalog.ListLaunchPaths

    -- * Types

    -- ** ProductViewFilterBy
    , ProductViewFilterBy (..)

    -- ** ProductViewSortBy
    , ProductViewSortBy (..)

    -- ** RecordStatus
    , RecordStatus (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** ConstraintSummary
    , ConstraintSummary
    , constraintSummary
    , csType
    , csDescription

    -- ** LaunchPathSummary
    , LaunchPathSummary
    , launchPathSummary
    , lpsConstraintSummaries
    , lpsName
    , lpsId
    , lpsTags

    -- ** ListRecordHistorySearchFilter
    , ListRecordHistorySearchFilter
    , listRecordHistorySearchFilter
    , lrhsfValue
    , lrhsfKey

    -- ** ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcAllowedValues

    -- ** ProductViewAggregationValue
    , ProductViewAggregationValue
    , productViewAggregationValue
    , pvavValue
    , pvavApproximateCount

    -- ** ProductViewSummary
    , ProductViewSummary
    , productViewSummary
    , pvsOwner
    , pvsSupportURL
    , pvsShortDescription
    , pvsHasDefaultPath
    , pvsDistributor
    , pvsName
    , pvsId
    , pvsType
    , pvsSupportEmail
    , pvsProductId
    , pvsSupportDescription

    -- ** ProvisionedProductDetail
    , ProvisionedProductDetail
    , provisionedProductDetail
    , ppdIdempotencyToken
    , ppdStatus
    , ppdARN
    , ppdCreatedTime
    , ppdStatusMessage
    , ppdName
    , ppdLastRecordId
    , ppdId
    , ppdType

    -- ** ProvisioningArtifact
    , ProvisioningArtifact
    , provisioningArtifact
    , paCreatedTime
    , paName
    , paId
    , paDescription

    -- ** ProvisioningArtifactParameter
    , ProvisioningArtifactParameter
    , provisioningArtifactParameter
    , papIsNoEcho
    , papParameterKey
    , papParameterType
    , papParameterConstraints
    , papDefaultValue
    , papDescription

    -- ** ProvisioningParameter
    , ProvisioningParameter
    , provisioningParameter
    , ppValue
    , ppKey

    -- ** RecordDetail
    , RecordDetail
    , recordDetail
    , rdStatus
    , rdRecordTags
    , rdProvisionedProductName
    , rdProvisioningArtifactId
    , rdCreatedTime
    , rdRecordType
    , rdRecordId
    , rdProvisionedProductType
    , rdUpdatedTime
    , rdPathId
    , rdProvisionedProductId
    , rdRecordErrors
    , rdProductId

    -- ** RecordError
    , RecordError
    , recordError
    , reCode
    , reDescription

    -- ** RecordOutput
    , RecordOutput
    , recordOutput
    , roOutputValue
    , roOutputKey
    , roDescription

    -- ** RecordTag
    , RecordTag
    , recordTag
    , rtValue
    , rtKey

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** UpdateProvisioningParameter
    , UpdateProvisioningParameter
    , updateProvisioningParameter
    , uppValue
    , uppKey
    , uppUsePreviousValue

    -- ** UsageInstruction
    , UsageInstruction
    , usageInstruction
    , uiValue
    , uiType
    ) where

import           Network.AWS.ServiceCatalog.DescribeProduct
import           Network.AWS.ServiceCatalog.DescribeProductView
import           Network.AWS.ServiceCatalog.DescribeProvisioningParameters
import           Network.AWS.ServiceCatalog.DescribeRecord
import           Network.AWS.ServiceCatalog.ListLaunchPaths
import           Network.AWS.ServiceCatalog.ListRecordHistory
import           Network.AWS.ServiceCatalog.ProvisionProduct
import           Network.AWS.ServiceCatalog.ScanProvisionedProducts
import           Network.AWS.ServiceCatalog.SearchProducts
import           Network.AWS.ServiceCatalog.TerminateProvisionedProduct
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product
import           Network.AWS.ServiceCatalog.UpdateProvisionedProduct
import           Network.AWS.ServiceCatalog.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ServiceCatalog'.
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
