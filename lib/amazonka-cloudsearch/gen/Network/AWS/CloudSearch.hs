{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon CloudSearch Configuration Service__
--
-- You use the Amazon CloudSearch configuration service to create, configure, and manage search domains. Configuration service requests are submitted using the AWS Query protocol. AWS Query requests are HTTP or HTTPS requests submitted via HTTP GET or POST with a query parameter named Action.
--
-- The endpoint for configuration service requests is region-specific: cloudsearch./region/ .amazonaws.com. For example, cloudsearch.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region Regions and Endpoints> .
--
module Network.AWS.CloudSearch
    (
    -- * Service Configuration
      cloudSearch

    -- * Errors
    -- $errors

    -- ** BaseException
    , _BaseException

    -- ** DisabledOperationException
    , _DisabledOperationException

    -- ** InternalException
    , _InternalException

    -- ** InvalidTypeException
    , _InvalidTypeException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeAvailabilityOptions
    , module Network.AWS.CloudSearch.DescribeAvailabilityOptions

    -- ** DescribeExpressions
    , module Network.AWS.CloudSearch.DescribeExpressions

    -- ** DefineExpression
    , module Network.AWS.CloudSearch.DefineExpression

    -- ** DescribeScalingParameters
    , module Network.AWS.CloudSearch.DescribeScalingParameters

    -- ** DescribeServiceAccessPolicies
    , module Network.AWS.CloudSearch.DescribeServiceAccessPolicies

    -- ** DescribeSuggesters
    , module Network.AWS.CloudSearch.DescribeSuggesters

    -- ** UpdateAvailabilityOptions
    , module Network.AWS.CloudSearch.UpdateAvailabilityOptions

    -- ** DeleteExpression
    , module Network.AWS.CloudSearch.DeleteExpression

    -- ** ListDomainNames
    , module Network.AWS.CloudSearch.ListDomainNames

    -- ** DefineSuggester
    , module Network.AWS.CloudSearch.DefineSuggester

    -- ** DescribeDomains
    , module Network.AWS.CloudSearch.DescribeDomains

    -- ** DeleteAnalysisScheme
    , module Network.AWS.CloudSearch.DeleteAnalysisScheme

    -- ** DescribeAnalysisSchemes
    , module Network.AWS.CloudSearch.DescribeAnalysisSchemes

    -- ** CreateDomain
    , module Network.AWS.CloudSearch.CreateDomain

    -- ** DescribeIndexFields
    , module Network.AWS.CloudSearch.DescribeIndexFields

    -- ** DeleteSuggester
    , module Network.AWS.CloudSearch.DeleteSuggester

    -- ** DefineAnalysisScheme
    , module Network.AWS.CloudSearch.DefineAnalysisScheme

    -- ** IndexDocuments
    , module Network.AWS.CloudSearch.IndexDocuments

    -- ** DeleteIndexField
    , module Network.AWS.CloudSearch.DeleteIndexField

    -- ** UpdateServiceAccessPolicies
    , module Network.AWS.CloudSearch.UpdateServiceAccessPolicies

    -- ** UpdateScalingParameters
    , module Network.AWS.CloudSearch.UpdateScalingParameters

    -- ** BuildSuggesters
    , module Network.AWS.CloudSearch.BuildSuggesters

    -- ** DeleteDomain
    , module Network.AWS.CloudSearch.DeleteDomain

    -- ** DefineIndexField
    , module Network.AWS.CloudSearch.DefineIndexField

    -- * Types

    -- ** AlgorithmicStemming
    , AlgorithmicStemming (..)

    -- ** AnalysisSchemeLanguage
    , AnalysisSchemeLanguage (..)

    -- ** IndexFieldType
    , IndexFieldType (..)

    -- ** OptionState
    , OptionState (..)

    -- ** PartitionInstanceType
    , PartitionInstanceType (..)

    -- ** SuggesterFuzzyMatching
    , SuggesterFuzzyMatching (..)

    -- ** AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- ** AnalysisOptions
    , AnalysisOptions
    , analysisOptions
    , aoAlgorithmicStemming
    , aoStopwords
    , aoJapaneseTokenizationDictionary
    , aoSynonyms
    , aoStemmingDictionary

    -- ** AnalysisScheme
    , AnalysisScheme
    , analysisScheme
    , asAnalysisOptions
    , asAnalysisSchemeName
    , asAnalysisSchemeLanguage

    -- ** AnalysisSchemeStatus
    , AnalysisSchemeStatus
    , analysisSchemeStatus
    , assOptions
    , assStatus

    -- ** AvailabilityOptionsStatus
    , AvailabilityOptionsStatus
    , availabilityOptionsStatus
    , aosOptions
    , aosStatus

    -- ** DateArrayOptions
    , DateArrayOptions
    , dateArrayOptions
    , daosSourceFields
    , daosReturnEnabled
    , daosFacetEnabled
    , daosSearchEnabled
    , daosDefaultValue

    -- ** DateOptions
    , DateOptions
    , dateOptions
    , doSourceField
    , doReturnEnabled
    , doFacetEnabled
    , doSearchEnabled
    , doSortEnabled
    , doDefaultValue

    -- ** DocumentSuggesterOptions
    , DocumentSuggesterOptions
    , documentSuggesterOptions
    , dsoSortExpression
    , dsoFuzzyMatching
    , dsoSourceField

    -- ** DomainStatus
    , DomainStatus
    , domainStatus
    , dsSearchInstanceCount
    , dsSearchInstanceType
    , dsDocService
    , dsARN
    , dsCreated
    , dsSearchService
    , dsLimits
    , dsSearchPartitionCount
    , dsDeleted
    , dsProcessing
    , dsDomainId
    , dsDomainName
    , dsRequiresIndexDocuments

    -- ** DoubleArrayOptions
    , DoubleArrayOptions
    , doubleArrayOptions
    , daoSourceFields
    , daoReturnEnabled
    , daoFacetEnabled
    , daoSearchEnabled
    , daoDefaultValue

    -- ** DoubleOptions
    , DoubleOptions
    , doubleOptions
    , dSourceField
    , dReturnEnabled
    , dFacetEnabled
    , dSearchEnabled
    , dSortEnabled
    , dDefaultValue

    -- ** Expression
    , Expression
    , expression
    , eExpressionName
    , eExpressionValue

    -- ** ExpressionStatus
    , ExpressionStatus
    , expressionStatus
    , esOptions
    , esStatus

    -- ** IndexField
    , IndexField
    , indexField
    , ifDoubleArrayOptions
    , ifDateOptions
    , ifTextArrayOptions
    , ifDoubleOptions
    , ifTextOptions
    , ifLatLonOptions
    , ifLiteralArrayOptions
    , ifIntArrayOptions
    , ifDateArrayOptions
    , ifIntOptions
    , ifLiteralOptions
    , ifIndexFieldName
    , ifIndexFieldType

    -- ** IndexFieldStatus
    , IndexFieldStatus
    , indexFieldStatus
    , ifsOptions
    , ifsStatus

    -- ** IntArrayOptions
    , IntArrayOptions
    , intArrayOptions
    , iaoSourceFields
    , iaoReturnEnabled
    , iaoFacetEnabled
    , iaoSearchEnabled
    , iaoDefaultValue

    -- ** IntOptions
    , IntOptions
    , intOptions
    , ioSourceField
    , ioReturnEnabled
    , ioFacetEnabled
    , ioSearchEnabled
    , ioSortEnabled
    , ioDefaultValue

    -- ** LatLonOptions
    , LatLonOptions
    , latLonOptions
    , lloSourceField
    , lloReturnEnabled
    , lloFacetEnabled
    , lloSearchEnabled
    , lloSortEnabled
    , lloDefaultValue

    -- ** Limits
    , Limits
    , limits
    , lMaximumReplicationCount
    , lMaximumPartitionCount

    -- ** LiteralArrayOptions
    , LiteralArrayOptions
    , literalArrayOptions
    , laoSourceFields
    , laoReturnEnabled
    , laoFacetEnabled
    , laoSearchEnabled
    , laoDefaultValue

    -- ** LiteralOptions
    , LiteralOptions
    , literalOptions
    , loSourceField
    , loReturnEnabled
    , loFacetEnabled
    , loSearchEnabled
    , loSortEnabled
    , loDefaultValue

    -- ** OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- ** ScalingParameters
    , ScalingParameters
    , scalingParameters
    , spDesiredInstanceType
    , spDesiredReplicationCount
    , spDesiredPartitionCount

    -- ** ScalingParametersStatus
    , ScalingParametersStatus
    , scalingParametersStatus
    , spsOptions
    , spsStatus

    -- ** ServiceEndpoint
    , ServiceEndpoint
    , serviceEndpoint
    , seEndpoint

    -- ** Suggester
    , Suggester
    , suggester
    , sSuggesterName
    , sDocumentSuggesterOptions

    -- ** SuggesterStatus
    , SuggesterStatus
    , suggesterStatus
    , ssOptions
    , ssStatus

    -- ** TextArrayOptions
    , TextArrayOptions
    , textArrayOptions
    , taoSourceFields
    , taoReturnEnabled
    , taoAnalysisScheme
    , taoHighlightEnabled
    , taoDefaultValue

    -- ** TextOptions
    , TextOptions
    , textOptions
    , toSourceField
    , toReturnEnabled
    , toAnalysisScheme
    , toHighlightEnabled
    , toSortEnabled
    , toDefaultValue
    ) where

import Network.AWS.CloudSearch.BuildSuggesters
import Network.AWS.CloudSearch.CreateDomain
import Network.AWS.CloudSearch.DefineAnalysisScheme
import Network.AWS.CloudSearch.DefineExpression
import Network.AWS.CloudSearch.DefineIndexField
import Network.AWS.CloudSearch.DefineSuggester
import Network.AWS.CloudSearch.DeleteAnalysisScheme
import Network.AWS.CloudSearch.DeleteDomain
import Network.AWS.CloudSearch.DeleteExpression
import Network.AWS.CloudSearch.DeleteIndexField
import Network.AWS.CloudSearch.DeleteSuggester
import Network.AWS.CloudSearch.DescribeAnalysisSchemes
import Network.AWS.CloudSearch.DescribeAvailabilityOptions
import Network.AWS.CloudSearch.DescribeDomains
import Network.AWS.CloudSearch.DescribeExpressions
import Network.AWS.CloudSearch.DescribeIndexFields
import Network.AWS.CloudSearch.DescribeScalingParameters
import Network.AWS.CloudSearch.DescribeServiceAccessPolicies
import Network.AWS.CloudSearch.DescribeSuggesters
import Network.AWS.CloudSearch.IndexDocuments
import Network.AWS.CloudSearch.ListDomainNames
import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.UpdateAvailabilityOptions
import Network.AWS.CloudSearch.UpdateScalingParameters
import Network.AWS.CloudSearch.UpdateServiceAccessPolicies
import Network.AWS.CloudSearch.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudSearch'.
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
