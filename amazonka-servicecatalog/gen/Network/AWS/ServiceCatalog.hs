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
-- __AWS Service Catalog__
--
-- __Overview__
--
-- <https://aws.amazon.com/servicecatalog/ AWS Service Catalog> allows organizations to create and manage catalogs of IT services that are approved for use on AWS. This documentation provides reference material for the AWS Service Catalog end user API. To get the most out of this documentation, be familiar with the terminology discussed in <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html AWS Service Catalog Concepts> .
--
-- /Additional Resources/
--
--     * <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html AWS Service Catalog Administrator Guide>
--
--     * <http://docs.aws.amazon.com/servicecatalog/latest/userguide/introduction.html AWS Service Catalog User Guide>
--
--
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

    -- ** TagOptionNotMigratedException
    , _TagOptionNotMigratedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidStateException
    , _InvalidStateException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteConstraint
    , module Network.AWS.ServiceCatalog.DeleteConstraint

    -- ** UpdateConstraint
    , module Network.AWS.ServiceCatalog.UpdateConstraint

    -- ** CreateProduct
    , module Network.AWS.ServiceCatalog.CreateProduct

    -- ** TerminateProvisionedProduct
    , module Network.AWS.ServiceCatalog.TerminateProvisionedProduct

    -- ** UpdateProvisionedProduct
    , module Network.AWS.ServiceCatalog.UpdateProvisionedProduct

    -- ** DescribeProvisioningArtifact
    , module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact

    -- ** ListRecordHistory
    , module Network.AWS.ServiceCatalog.ListRecordHistory

    -- ** AssociateTagOptionWithResource
    , module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource

    -- ** CreateTagOption
    , module Network.AWS.ServiceCatalog.CreateTagOption

    -- ** DisassociateProductFromPortfolio
    , module Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio

    -- ** ListConstraintsForPortfolio
    , module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio

    -- ** DescribeRecord
    , module Network.AWS.ServiceCatalog.DescribeRecord

    -- ** DescribeConstraint
    , module Network.AWS.ServiceCatalog.DescribeConstraint

    -- ** CreateProvisioningArtifact
    , module Network.AWS.ServiceCatalog.CreateProvisioningArtifact

    -- ** ListPortfolios
    , module Network.AWS.ServiceCatalog.ListPortfolios

    -- ** DescribeProductView
    , module Network.AWS.ServiceCatalog.DescribeProductView

    -- ** CreatePortfolioShare
    , module Network.AWS.ServiceCatalog.CreatePortfolioShare

    -- ** ListProvisioningArtifacts
    , module Network.AWS.ServiceCatalog.ListProvisioningArtifacts

    -- ** SearchProducts
    , module Network.AWS.ServiceCatalog.SearchProducts

    -- ** DescribeProduct
    , module Network.AWS.ServiceCatalog.DescribeProduct

    -- ** CreateConstraint
    , module Network.AWS.ServiceCatalog.CreateConstraint

    -- ** ListPortfolioAccess
    , module Network.AWS.ServiceCatalog.ListPortfolioAccess

    -- ** DisassociatePrincipalFromPortfolio
    , module Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio

    -- ** DescribeTagOption
    , module Network.AWS.ServiceCatalog.DescribeTagOption

    -- ** DisassociateTagOptionFromResource
    , module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource

    -- ** DescribePortfolio
    , module Network.AWS.ServiceCatalog.DescribePortfolio

    -- ** AssociateProductWithPortfolio
    , module Network.AWS.ServiceCatalog.AssociateProductWithPortfolio

    -- ** ListAcceptedPortfolioShares
    , module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares

    -- ** AcceptPortfolioShare
    , module Network.AWS.ServiceCatalog.AcceptPortfolioShare

    -- ** ScanProvisionedProducts
    , module Network.AWS.ServiceCatalog.ScanProvisionedProducts

    -- ** ListPrincipalsForPortfolio
    , module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio

    -- ** DeleteProduct
    , module Network.AWS.ServiceCatalog.DeleteProduct

    -- ** UpdateProduct
    , module Network.AWS.ServiceCatalog.UpdateProduct

    -- ** ProvisionProduct
    , module Network.AWS.ServiceCatalog.ProvisionProduct

    -- ** RejectPortfolioShare
    , module Network.AWS.ServiceCatalog.RejectPortfolioShare

    -- ** UpdateTagOption
    , module Network.AWS.ServiceCatalog.UpdateTagOption

    -- ** ListTagOptions
    , module Network.AWS.ServiceCatalog.ListTagOptions

    -- ** SearchProductsAsAdmin
    , module Network.AWS.ServiceCatalog.SearchProductsAsAdmin

    -- ** DeletePortfolio
    , module Network.AWS.ServiceCatalog.DeletePortfolio

    -- ** UpdatePortfolio
    , module Network.AWS.ServiceCatalog.UpdatePortfolio

    -- ** ListPortfoliosForProduct
    , module Network.AWS.ServiceCatalog.ListPortfoliosForProduct

    -- ** DescribeProductAsAdmin
    , module Network.AWS.ServiceCatalog.DescribeProductAsAdmin

    -- ** DescribeProvisioningParameters
    , module Network.AWS.ServiceCatalog.DescribeProvisioningParameters

    -- ** AssociatePrincipalWithPortfolio
    , module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio

    -- ** DescribeProvisionedProduct
    , module Network.AWS.ServiceCatalog.DescribeProvisionedProduct

    -- ** UpdateProvisioningArtifact
    , module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact

    -- ** DeletePortfolioShare
    , module Network.AWS.ServiceCatalog.DeletePortfolioShare

    -- ** DeleteProvisioningArtifact
    , module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact

    -- ** CreatePortfolio
    , module Network.AWS.ServiceCatalog.CreatePortfolio

    -- ** ListLaunchPaths
    , module Network.AWS.ServiceCatalog.ListLaunchPaths

    -- ** ListResourcesForTagOption
    , module Network.AWS.ServiceCatalog.ListResourcesForTagOption

    -- * Types

    -- ** AccessLevelFilterKey
    , AccessLevelFilterKey (..)

    -- ** PrincipalType
    , PrincipalType (..)

    -- ** ProductSource
    , ProductSource (..)

    -- ** ProductType
    , ProductType (..)

    -- ** ProductViewFilterBy
    , ProductViewFilterBy (..)

    -- ** ProductViewSortBy
    , ProductViewSortBy (..)

    -- ** ProvisionedProductStatus
    , ProvisionedProductStatus (..)

    -- ** ProvisioningArtifactType
    , ProvisioningArtifactType (..)

    -- ** RecordStatus
    , RecordStatus (..)

    -- ** RequestStatus
    , RequestStatus (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** AccessLevelFilter
    , AccessLevelFilter
    , accessLevelFilter
    , alfValue
    , alfKey

    -- ** ConstraintDetail
    , ConstraintDetail
    , constraintDetail
    , cdConstraintId
    , cdOwner
    , cdType
    , cdDescription

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

    -- ** ListTagOptionsFilters
    , ListTagOptionsFilters
    , listTagOptionsFilters
    , ltofValue
    , ltofActive
    , ltofKey

    -- ** ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcAllowedValues

    -- ** PortfolioDetail
    , PortfolioDetail
    , portfolioDetail
    , pdARN
    , pdCreatedTime
    , pdId
    , pdDisplayName
    , pdDescription
    , pdProviderName

    -- ** Principal
    , Principal
    , principal
    , pPrincipalType
    , pPrincipalARN

    -- ** ProductViewAggregationValue
    , ProductViewAggregationValue
    , productViewAggregationValue
    , pvavValue
    , pvavApproximateCount

    -- ** ProductViewDetail
    , ProductViewDetail
    , productViewDetail
    , pvdStatus
    , pvdProductViewSummary
    , pvdCreatedTime
    , pvdProductARN

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

    -- ** ProvisioningArtifactDetail
    , ProvisioningArtifactDetail
    , provisioningArtifactDetail
    , padCreatedTime
    , padName
    , padId
    , padType
    , padDescription

    -- ** ProvisioningArtifactParameter
    , ProvisioningArtifactParameter
    , provisioningArtifactParameter
    , pIsNoEcho
    , pParameterKey
    , pParameterType
    , pParameterConstraints
    , pDefaultValue
    , pDescription

    -- ** ProvisioningArtifactProperties
    , ProvisioningArtifactProperties
    , provisioningArtifactProperties
    , papName
    , papType
    , papDescription
    , papInfo

    -- ** ProvisioningArtifactSummary
    , ProvisioningArtifactSummary
    , provisioningArtifactSummary
    , pasProvisioningArtifactMetadata
    , pasCreatedTime
    , pasName
    , pasId
    , pasDescription

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

    -- ** ResourceDetail
    , ResourceDetail
    , resourceDetail
    , rARN
    , rCreatedTime
    , rName
    , rId
    , rDescription

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TagOptionDetail
    , TagOptionDetail
    , tagOptionDetail
    , todValue
    , todActive
    , todKey
    , todId

    -- ** TagOptionSummary
    , TagOptionSummary
    , tagOptionSummary
    , tosValues
    , tosKey

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

import           Network.AWS.ServiceCatalog.AcceptPortfolioShare
import           Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
import           Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
import           Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
import           Network.AWS.ServiceCatalog.CreateConstraint
import           Network.AWS.ServiceCatalog.CreatePortfolio
import           Network.AWS.ServiceCatalog.CreatePortfolioShare
import           Network.AWS.ServiceCatalog.CreateProduct
import           Network.AWS.ServiceCatalog.CreateProvisioningArtifact
import           Network.AWS.ServiceCatalog.CreateTagOption
import           Network.AWS.ServiceCatalog.DeleteConstraint
import           Network.AWS.ServiceCatalog.DeletePortfolio
import           Network.AWS.ServiceCatalog.DeletePortfolioShare
import           Network.AWS.ServiceCatalog.DeleteProduct
import           Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
import           Network.AWS.ServiceCatalog.DescribeConstraint
import           Network.AWS.ServiceCatalog.DescribePortfolio
import           Network.AWS.ServiceCatalog.DescribeProduct
import           Network.AWS.ServiceCatalog.DescribeProductAsAdmin
import           Network.AWS.ServiceCatalog.DescribeProductView
import           Network.AWS.ServiceCatalog.DescribeProvisionedProduct
import           Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
import           Network.AWS.ServiceCatalog.DescribeProvisioningParameters
import           Network.AWS.ServiceCatalog.DescribeRecord
import           Network.AWS.ServiceCatalog.DescribeTagOption
import           Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
import           Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
import           Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
import           Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
import           Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
import           Network.AWS.ServiceCatalog.ListLaunchPaths
import           Network.AWS.ServiceCatalog.ListPortfolioAccess
import           Network.AWS.ServiceCatalog.ListPortfolios
import           Network.AWS.ServiceCatalog.ListPortfoliosForProduct
import           Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
import           Network.AWS.ServiceCatalog.ListProvisioningArtifacts
import           Network.AWS.ServiceCatalog.ListRecordHistory
import           Network.AWS.ServiceCatalog.ListResourcesForTagOption
import           Network.AWS.ServiceCatalog.ListTagOptions
import           Network.AWS.ServiceCatalog.ProvisionProduct
import           Network.AWS.ServiceCatalog.RejectPortfolioShare
import           Network.AWS.ServiceCatalog.ScanProvisionedProducts
import           Network.AWS.ServiceCatalog.SearchProducts
import           Network.AWS.ServiceCatalog.SearchProductsAsAdmin
import           Network.AWS.ServiceCatalog.TerminateProvisionedProduct
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.UpdateConstraint
import           Network.AWS.ServiceCatalog.UpdatePortfolio
import           Network.AWS.ServiceCatalog.UpdateProduct
import           Network.AWS.ServiceCatalog.UpdateProvisionedProduct
import           Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
import           Network.AWS.ServiceCatalog.UpdateTagOption
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
