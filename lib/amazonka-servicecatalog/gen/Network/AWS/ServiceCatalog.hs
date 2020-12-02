{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Service Catalog__
--
-- <https://aws.amazon.com/servicecatalog/ AWS Service Catalog> enables organizations to create and manage catalogs of IT services that are approved for use on AWS. To get the most out of this documentation, you should be familiar with the terminology discussed in <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html AWS Service Catalog Concepts> .
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

    -- ** CreateProvisionedProductPlan
    , module Network.AWS.ServiceCatalog.CreateProvisionedProductPlan

    -- ** CreateProduct
    , module Network.AWS.ServiceCatalog.CreateProduct

    -- ** DescribeCopyProductStatus
    , module Network.AWS.ServiceCatalog.DescribeCopyProductStatus

    -- ** TerminateProvisionedProduct
    , module Network.AWS.ServiceCatalog.TerminateProvisionedProduct

    -- ** UpdateProvisionedProduct
    , module Network.AWS.ServiceCatalog.UpdateProvisionedProduct

    -- ** DescribeProvisioningArtifact
    , module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact

    -- ** ListRecordHistory
    , module Network.AWS.ServiceCatalog.ListRecordHistory

    -- ** DescribeProvisionedProductPlan
    , module Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan

    -- ** AssociateTagOptionWithResource
    , module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource

    -- ** CreateTagOption
    , module Network.AWS.ServiceCatalog.CreateTagOption

    -- ** DisassociateProductFromPortfolio
    , module Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio

    -- ** ListConstraintsForPortfolio (Paginated)
    , module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio

    -- ** DescribeRecord
    , module Network.AWS.ServiceCatalog.DescribeRecord

    -- ** DescribeConstraint
    , module Network.AWS.ServiceCatalog.DescribeConstraint

    -- ** CreateProvisioningArtifact
    , module Network.AWS.ServiceCatalog.CreateProvisioningArtifact

    -- ** ListPortfolios (Paginated)
    , module Network.AWS.ServiceCatalog.ListPortfolios

    -- ** DescribeProductView
    , module Network.AWS.ServiceCatalog.DescribeProductView

    -- ** CreatePortfolioShare
    , module Network.AWS.ServiceCatalog.CreatePortfolioShare

    -- ** ListProvisioningArtifacts
    , module Network.AWS.ServiceCatalog.ListProvisioningArtifacts

    -- ** SearchProducts
    , module Network.AWS.ServiceCatalog.SearchProducts

    -- ** SearchProvisionedProducts
    , module Network.AWS.ServiceCatalog.SearchProvisionedProducts

    -- ** DescribeProduct
    , module Network.AWS.ServiceCatalog.DescribeProduct

    -- ** DeleteProvisionedProductPlan
    , module Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan

    -- ** CreateConstraint
    , module Network.AWS.ServiceCatalog.CreateConstraint

    -- ** ListProvisionedProductPlans
    , module Network.AWS.ServiceCatalog.ListProvisionedProductPlans

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

    -- ** ListAcceptedPortfolioShares (Paginated)
    , module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares

    -- ** ExecuteProvisionedProductPlan
    , module Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan

    -- ** AcceptPortfolioShare
    , module Network.AWS.ServiceCatalog.AcceptPortfolioShare

    -- ** ScanProvisionedProducts
    , module Network.AWS.ServiceCatalog.ScanProvisionedProducts

    -- ** ListPrincipalsForPortfolio (Paginated)
    , module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio

    -- ** DeleteProduct
    , module Network.AWS.ServiceCatalog.DeleteProduct

    -- ** UpdateProduct
    , module Network.AWS.ServiceCatalog.UpdateProduct

    -- ** ProvisionProduct
    , module Network.AWS.ServiceCatalog.ProvisionProduct

    -- ** RejectPortfolioShare
    , module Network.AWS.ServiceCatalog.RejectPortfolioShare

    -- ** DeleteTagOption
    , module Network.AWS.ServiceCatalog.DeleteTagOption

    -- ** UpdateTagOption
    , module Network.AWS.ServiceCatalog.UpdateTagOption

    -- ** ListTagOptions (Paginated)
    , module Network.AWS.ServiceCatalog.ListTagOptions

    -- ** SearchProductsAsAdmin (Paginated)
    , module Network.AWS.ServiceCatalog.SearchProductsAsAdmin

    -- ** DeletePortfolio
    , module Network.AWS.ServiceCatalog.DeletePortfolio

    -- ** UpdatePortfolio
    , module Network.AWS.ServiceCatalog.UpdatePortfolio

    -- ** ListPortfoliosForProduct (Paginated)
    , module Network.AWS.ServiceCatalog.ListPortfoliosForProduct

    -- ** DescribeProductAsAdmin
    , module Network.AWS.ServiceCatalog.DescribeProductAsAdmin

    -- ** DescribeProvisioningParameters
    , module Network.AWS.ServiceCatalog.DescribeProvisioningParameters

    -- ** AssociatePrincipalWithPortfolio
    , module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio

    -- ** DescribeProvisionedProduct
    , module Network.AWS.ServiceCatalog.DescribeProvisionedProduct

    -- ** CopyProduct
    , module Network.AWS.ServiceCatalog.CopyProduct

    -- ** UpdateProvisioningArtifact
    , module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact

    -- ** DeletePortfolioShare
    , module Network.AWS.ServiceCatalog.DeletePortfolioShare

    -- ** DeleteProvisioningArtifact
    , module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact

    -- ** CreatePortfolio
    , module Network.AWS.ServiceCatalog.CreatePortfolio

    -- ** ListLaunchPaths (Paginated)
    , module Network.AWS.ServiceCatalog.ListLaunchPaths

    -- ** ListResourcesForTagOption (Paginated)
    , module Network.AWS.ServiceCatalog.ListResourcesForTagOption

    -- * Types

    -- ** AccessLevelFilterKey
    , AccessLevelFilterKey (..)

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** CopyOption
    , CopyOption (..)

    -- ** CopyProductStatus
    , CopyProductStatus (..)

    -- ** EvaluationType
    , EvaluationType (..)

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

    -- ** ProvisionedProductPlanStatus
    , ProvisionedProductPlanStatus (..)

    -- ** ProvisionedProductPlanType
    , ProvisionedProductPlanType (..)

    -- ** ProvisionedProductStatus
    , ProvisionedProductStatus (..)

    -- ** ProvisionedProductViewFilterBy
    , ProvisionedProductViewFilterBy (..)

    -- ** ProvisioningArtifactPropertyName
    , ProvisioningArtifactPropertyName (..)

    -- ** ProvisioningArtifactType
    , ProvisioningArtifactType (..)

    -- ** RecordStatus
    , RecordStatus (..)

    -- ** Replacement
    , Replacement (..)

    -- ** RequestStatus
    , RequestStatus (..)

    -- ** RequiresRecreation
    , RequiresRecreation (..)

    -- ** ResourceAttribute
    , ResourceAttribute (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** AccessLevelFilter
    , AccessLevelFilter
    , accessLevelFilter
    , alfValue
    , alfKey

    -- ** CloudWatchDashboard
    , CloudWatchDashboard
    , cloudWatchDashboard
    , cwdName

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

    -- ** ProvisionedProductAttribute
    , ProvisionedProductAttribute
    , provisionedProductAttribute
    , ppaIdempotencyToken
    , ppaStatus
    , ppaProvisioningArtifactId
    , ppaARN
    , ppaCreatedTime
    , ppaUserARN
    , ppaStatusMessage
    , ppaName
    , ppaLastRecordId
    , ppaUserARNSession
    , ppaId
    , ppaType
    , ppaPhysicalId
    , ppaProductId
    , ppaTags

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

    -- ** ProvisionedProductPlanDetails
    , ProvisionedProductPlanDetails
    , provisionedProductPlanDetails
    , pppdStatus
    , pppdProvisionProductId
    , pppdProvisioningArtifactId
    , pppdProvisionProductName
    , pppdCreatedTime
    , pppdNotificationARNs
    , pppdPlanId
    , pppdPlanName
    , pppdStatusMessage
    , pppdUpdatedTime
    , pppdPathId
    , pppdProvisioningParameters
    , pppdPlanType
    , pppdProductId
    , pppdTags

    -- ** ProvisionedProductPlanSummary
    , ProvisionedProductPlanSummary
    , provisionedProductPlanSummary
    , pppsProvisionProductId
    , pppsProvisioningArtifactId
    , pppsProvisionProductName
    , pppsPlanId
    , pppsPlanName
    , pppsPlanType

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
    , padActive
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

    -- ** ResourceChange
    , ResourceChange
    , resourceChange
    , rcLogicalResourceId
    , rcPhysicalResourceId
    , rcResourceType
    , rcAction
    , rcScope
    , rcDetails
    , rcReplacement

    -- ** ResourceChangeDetail
    , ResourceChangeDetail
    , resourceChangeDetail
    , rcdCausingEntity
    , rcdEvaluation
    , rcdTarget

    -- ** ResourceDetail
    , ResourceDetail
    , resourceDetail
    , rARN
    , rCreatedTime
    , rName
    , rId
    , rDescription

    -- ** ResourceTargetDefinition
    , ResourceTargetDefinition
    , resourceTargetDefinition
    , rtdAttribute
    , rtdRequiresRecreation
    , rtdName

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

import Network.AWS.ServiceCatalog.AcceptPortfolioShare
import Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
import Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
import Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
import Network.AWS.ServiceCatalog.CopyProduct
import Network.AWS.ServiceCatalog.CreateConstraint
import Network.AWS.ServiceCatalog.CreatePortfolio
import Network.AWS.ServiceCatalog.CreatePortfolioShare
import Network.AWS.ServiceCatalog.CreateProduct
import Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
import Network.AWS.ServiceCatalog.CreateProvisioningArtifact
import Network.AWS.ServiceCatalog.CreateTagOption
import Network.AWS.ServiceCatalog.DeleteConstraint
import Network.AWS.ServiceCatalog.DeletePortfolio
import Network.AWS.ServiceCatalog.DeletePortfolioShare
import Network.AWS.ServiceCatalog.DeleteProduct
import Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
import Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
import Network.AWS.ServiceCatalog.DeleteTagOption
import Network.AWS.ServiceCatalog.DescribeConstraint
import Network.AWS.ServiceCatalog.DescribeCopyProductStatus
import Network.AWS.ServiceCatalog.DescribePortfolio
import Network.AWS.ServiceCatalog.DescribeProduct
import Network.AWS.ServiceCatalog.DescribeProductAsAdmin
import Network.AWS.ServiceCatalog.DescribeProductView
import Network.AWS.ServiceCatalog.DescribeProvisionedProduct
import Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
import Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
import Network.AWS.ServiceCatalog.DescribeProvisioningParameters
import Network.AWS.ServiceCatalog.DescribeRecord
import Network.AWS.ServiceCatalog.DescribeTagOption
import Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
import Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
import Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
import Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
import Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
import Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
import Network.AWS.ServiceCatalog.ListLaunchPaths
import Network.AWS.ServiceCatalog.ListPortfolioAccess
import Network.AWS.ServiceCatalog.ListPortfolios
import Network.AWS.ServiceCatalog.ListPortfoliosForProduct
import Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
import Network.AWS.ServiceCatalog.ListProvisionedProductPlans
import Network.AWS.ServiceCatalog.ListProvisioningArtifacts
import Network.AWS.ServiceCatalog.ListRecordHistory
import Network.AWS.ServiceCatalog.ListResourcesForTagOption
import Network.AWS.ServiceCatalog.ListTagOptions
import Network.AWS.ServiceCatalog.ProvisionProduct
import Network.AWS.ServiceCatalog.RejectPortfolioShare
import Network.AWS.ServiceCatalog.ScanProvisionedProducts
import Network.AWS.ServiceCatalog.SearchProducts
import Network.AWS.ServiceCatalog.SearchProductsAsAdmin
import Network.AWS.ServiceCatalog.SearchProvisionedProducts
import Network.AWS.ServiceCatalog.TerminateProvisionedProduct
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.UpdateConstraint
import Network.AWS.ServiceCatalog.UpdatePortfolio
import Network.AWS.ServiceCatalog.UpdateProduct
import Network.AWS.ServiceCatalog.UpdateProvisionedProduct
import Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
import Network.AWS.ServiceCatalog.UpdateTagOption
import Network.AWS.ServiceCatalog.Waiters

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
