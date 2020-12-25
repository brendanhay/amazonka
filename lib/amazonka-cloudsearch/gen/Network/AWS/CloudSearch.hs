{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon CloudSearch Configuration Service__
--
-- You use the Amazon CloudSearch configuration service to create, configure, and manage search domains. Configuration service requests are submitted using the AWS Query protocol. AWS Query requests are HTTP or HTTPS requests submitted via HTTP GET or POST with a query parameter named Action.
-- The endpoint for configuration service requests is region-specific: cloudsearch./region/ .amazonaws.com. For example, cloudsearch.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region Regions and Endpoints> .
module Network.AWS.CloudSearch
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** BaseException
    _BaseException,

    -- ** DisabledOperationException
    _DisabledOperationException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeAvailabilityOptions
    module Network.AWS.CloudSearch.DescribeAvailabilityOptions,

    -- ** DescribeExpressions
    module Network.AWS.CloudSearch.DescribeExpressions,

    -- ** DefineExpression
    module Network.AWS.CloudSearch.DefineExpression,

    -- ** DescribeScalingParameters
    module Network.AWS.CloudSearch.DescribeScalingParameters,

    -- ** DescribeServiceAccessPolicies
    module Network.AWS.CloudSearch.DescribeServiceAccessPolicies,

    -- ** DescribeSuggesters
    module Network.AWS.CloudSearch.DescribeSuggesters,

    -- ** UpdateAvailabilityOptions
    module Network.AWS.CloudSearch.UpdateAvailabilityOptions,

    -- ** DeleteExpression
    module Network.AWS.CloudSearch.DeleteExpression,

    -- ** ListDomainNames
    module Network.AWS.CloudSearch.ListDomainNames,

    -- ** DefineSuggester
    module Network.AWS.CloudSearch.DefineSuggester,

    -- ** DescribeDomains
    module Network.AWS.CloudSearch.DescribeDomains,

    -- ** DeleteAnalysisScheme
    module Network.AWS.CloudSearch.DeleteAnalysisScheme,

    -- ** DescribeDomainEndpointOptions
    module Network.AWS.CloudSearch.DescribeDomainEndpointOptions,

    -- ** DescribeAnalysisSchemes
    module Network.AWS.CloudSearch.DescribeAnalysisSchemes,

    -- ** CreateDomain
    module Network.AWS.CloudSearch.CreateDomain,

    -- ** UpdateDomainEndpointOptions
    module Network.AWS.CloudSearch.UpdateDomainEndpointOptions,

    -- ** DescribeIndexFields
    module Network.AWS.CloudSearch.DescribeIndexFields,

    -- ** DeleteSuggester
    module Network.AWS.CloudSearch.DeleteSuggester,

    -- ** DefineAnalysisScheme
    module Network.AWS.CloudSearch.DefineAnalysisScheme,

    -- ** IndexDocuments
    module Network.AWS.CloudSearch.IndexDocuments,

    -- ** DeleteIndexField
    module Network.AWS.CloudSearch.DeleteIndexField,

    -- ** UpdateServiceAccessPolicies
    module Network.AWS.CloudSearch.UpdateServiceAccessPolicies,

    -- ** UpdateScalingParameters
    module Network.AWS.CloudSearch.UpdateScalingParameters,

    -- ** BuildSuggesters
    module Network.AWS.CloudSearch.BuildSuggesters,

    -- ** DeleteDomain
    module Network.AWS.CloudSearch.DeleteDomain,

    -- ** DefineIndexField
    module Network.AWS.CloudSearch.DefineIndexField,

    -- * Types

    -- ** DomainStatus
    DomainStatus (..),
    mkDomainStatus,
    dsDomainId,
    dsDomainName,
    dsRequiresIndexDocuments,
    dsARN,
    dsCreated,
    dsDeleted,
    dsDocService,
    dsLimits,
    dsProcessing,
    dsSearchInstanceCount,
    dsSearchInstanceType,
    dsSearchPartitionCount,
    dsSearchService,

    -- ** DocumentSuggesterOptions
    DocumentSuggesterOptions (..),
    mkDocumentSuggesterOptions,
    dsoSourceField,
    dsoFuzzyMatching,
    dsoSortExpression,

    -- ** DoubleArrayOptions
    DoubleArrayOptions (..),
    mkDoubleArrayOptions,
    daoDefaultValue,
    daoFacetEnabled,
    daoReturnEnabled,
    daoSearchEnabled,
    daoSourceFields,

    -- ** IndexField
    IndexField (..),
    mkIndexField,
    ifIndexFieldName,
    ifIndexFieldType,
    ifDateArrayOptions,
    ifDateOptions,
    ifDoubleArrayOptions,
    ifDoubleOptions,
    ifIntArrayOptions,
    ifIntOptions,
    ifLatLonOptions,
    ifLiteralArrayOptions,
    ifLiteralOptions,
    ifTextArrayOptions,
    ifTextOptions,

    -- ** DateOptions
    DateOptions (..),
    mkDateOptions,
    doDefaultValue,
    doFacetEnabled,
    doReturnEnabled,
    doSearchEnabled,
    doSortEnabled,
    doSourceField,

    -- ** OptionState
    OptionState (..),

    -- ** TextArrayOptions
    TextArrayOptions (..),
    mkTextArrayOptions,
    taoAnalysisScheme,
    taoDefaultValue,
    taoHighlightEnabled,
    taoReturnEnabled,
    taoSourceFields,

    -- ** SearchInstanceType
    SearchInstanceType (..),

    -- ** PolicyDocument
    PolicyDocument (..),

    -- ** AlgorithmicStemming
    AlgorithmicStemming (..),

    -- ** AnalysisScheme
    AnalysisScheme (..),
    mkAnalysisScheme,
    asAnalysisSchemeName,
    asAnalysisSchemeLanguage,
    asAnalysisOptions,

    -- ** ScalingParameters
    ScalingParameters (..),
    mkScalingParameters,
    spDesiredInstanceType,
    spDesiredPartitionCount,
    spDesiredReplicationCount,

    -- ** FieldNameCommaList
    FieldNameCommaList (..),

    -- ** APIVersion
    APIVersion (..),

    -- ** AnalysisOptions
    AnalysisOptions (..),
    mkAnalysisOptions,
    aoAlgorithmicStemming,
    aoJapaneseTokenizationDictionary,
    aoStemmingDictionary,
    aoStopwords,
    aoSynonyms,

    -- ** DoubleOptions
    DoubleOptions (..),
    mkDoubleOptions,
    dosDefaultValue,
    dosFacetEnabled,
    dosReturnEnabled,
    dosSearchEnabled,
    dosSortEnabled,
    dosSourceField,

    -- ** ARN
    ARN (..),

    -- ** TextOptions
    TextOptions (..),
    mkTextOptions,
    toAnalysisScheme,
    toDefaultValue,
    toHighlightEnabled,
    toReturnEnabled,
    toSortEnabled,
    toSourceField,

    -- ** AvailabilityOptionsStatus
    AvailabilityOptionsStatus (..),
    mkAvailabilityOptionsStatus,
    aosOptions,
    aosStatus,

    -- ** DynamicFieldName
    DynamicFieldName (..),

    -- ** IndexFieldStatus
    IndexFieldStatus (..),
    mkIndexFieldStatus,
    ifsOptions,
    ifsStatus,

    -- ** ScalingParametersStatus
    ScalingParametersStatus (..),
    mkScalingParametersStatus,
    spsOptions,
    spsStatus,

    -- ** AnalysisSchemeStatus
    AnalysisSchemeStatus (..),
    mkAnalysisSchemeStatus,
    assOptions,
    assStatus,

    -- ** ServiceEndpoint
    ServiceEndpoint (..),
    mkServiceEndpoint,
    seEndpoint,

    -- ** Limits
    Limits (..),
    mkLimits,
    lMaximumReplicationCount,
    lMaximumPartitionCount,

    -- ** ExpressionStatus
    ExpressionStatus (..),
    mkExpressionStatus,
    esOptions,
    esStatus,

    -- ** FieldValue
    FieldValue (..),

    -- ** IndexFieldType
    IndexFieldType (..),

    -- ** StandardName
    StandardName (..),

    -- ** LatLonOptions
    LatLonOptions (..),
    mkLatLonOptions,
    lloDefaultValue,
    lloFacetEnabled,
    lloReturnEnabled,
    lloSearchEnabled,
    lloSortEnabled,
    lloSourceField,

    -- ** SuggesterStatus
    SuggesterStatus (..),
    mkSuggesterStatus,
    ssOptions,
    ssStatus,

    -- ** DomainName
    DomainName (..),

    -- ** OptionStatus
    OptionStatus (..),
    mkOptionStatus,
    osCreationDate,
    osUpdateDate,
    osState,
    osPendingDeletion,
    osUpdateVersion,

    -- ** DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    mkDomainEndpointOptionsStatus,
    deosOptions,
    deosStatus,

    -- ** LiteralArrayOptions
    LiteralArrayOptions (..),
    mkLiteralArrayOptions,
    laoDefaultValue,
    laoFacetEnabled,
    laoReturnEnabled,
    laoSearchEnabled,
    laoSourceFields,

    -- ** IntArrayOptions
    IntArrayOptions (..),
    mkIntArrayOptions,
    iaoDefaultValue,
    iaoFacetEnabled,
    iaoReturnEnabled,
    iaoSearchEnabled,
    iaoSourceFields,

    -- ** Expression
    Expression (..),
    mkExpression,
    eExpressionName,
    eExpressionValue,

    -- ** SuggesterFuzzyMatching
    SuggesterFuzzyMatching (..),

    -- ** FieldName
    FieldName (..),

    -- ** TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- ** DateArrayOptions
    DateArrayOptions (..),
    mkDateArrayOptions,
    dDefaultValue,
    dFacetEnabled,
    dReturnEnabled,
    dSearchEnabled,
    dSourceFields,

    -- ** DomainId
    DomainId (..),

    -- ** AnalysisSchemeLanguage
    AnalysisSchemeLanguage (..),

    -- ** PartitionInstanceType
    PartitionInstanceType (..),

    -- ** Suggester
    Suggester (..),
    mkSuggester,
    sSuggesterName,
    sDocumentSuggesterOptions,

    -- ** IntOptions
    IntOptions (..),
    mkIntOptions,
    ioDefaultValue,
    ioFacetEnabled,
    ioReturnEnabled,
    ioSearchEnabled,
    ioSortEnabled,
    ioSourceField,

    -- ** LiteralOptions
    LiteralOptions (..),
    mkLiteralOptions,
    loDefaultValue,
    loFacetEnabled,
    loReturnEnabled,
    loSearchEnabled,
    loSortEnabled,
    loSourceField,

    -- ** DomainEndpointOptions
    DomainEndpointOptions (..),
    mkDomainEndpointOptions,
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,

    -- ** Word
    Word (..),

    -- ** AccessPoliciesStatus
    AccessPoliciesStatus (..),
    mkAccessPoliciesStatus,
    apsOptions,
    apsStatus,

    -- ** ExpressionValue
    ExpressionValue (..),

    -- ** SourceField
    SourceField (..),

    -- ** SortExpression
    SortExpression (..),

    -- ** SourceFields
    SourceFields (..),

    -- ** IndexFieldName
    IndexFieldName (..),

    -- ** DefaultValue
    DefaultValue (..),

    -- ** AnalysisSchemeName
    AnalysisSchemeName (..),

    -- ** ExpressionName
    ExpressionName (..),

    -- ** JapaneseTokenizationDictionary
    JapaneseTokenizationDictionary (..),

    -- ** StemmingDictionary
    StemmingDictionary (..),

    -- ** Stopwords
    Stopwords (..),

    -- ** Synonyms
    Synonyms (..),

    -- ** SuggesterName
    SuggesterName (..),

    -- ** Endpoint
    Endpoint (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

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
import Network.AWS.CloudSearch.DescribeDomainEndpointOptions
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
import Network.AWS.CloudSearch.UpdateDomainEndpointOptions
import Network.AWS.CloudSearch.UpdateScalingParameters
import Network.AWS.CloudSearch.UpdateServiceAccessPolicies
import Network.AWS.CloudSearch.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudSearch'.

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
