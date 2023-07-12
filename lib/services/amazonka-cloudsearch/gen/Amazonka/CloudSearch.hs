{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudSearch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudSearch Configuration Service
--
-- You use the Amazon CloudSearch configuration service to create,
-- configure, and manage search domains. Configuration service requests are
-- submitted using the AWS Query protocol. AWS Query requests are HTTP or
-- HTTPS requests submitted via HTTP GET or POST with a query parameter
-- named Action.
--
-- The endpoint for configuration service requests is region-specific:
-- cloudsearch./region/.amazonaws.com. For example,
-- cloudsearch.us-east-1.amazonaws.com. For a current list of supported
-- regions and endpoints, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region Regions and Endpoints>.
module Amazonka.CloudSearch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BaseException
    _BaseException,

    -- ** DisabledOperationException
    _DisabledOperationException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BuildSuggesters
    BuildSuggesters (BuildSuggesters'),
    newBuildSuggesters,
    BuildSuggestersResponse (BuildSuggestersResponse'),
    newBuildSuggestersResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** DefineAnalysisScheme
    DefineAnalysisScheme (DefineAnalysisScheme'),
    newDefineAnalysisScheme,
    DefineAnalysisSchemeResponse (DefineAnalysisSchemeResponse'),
    newDefineAnalysisSchemeResponse,

    -- ** DefineExpression
    DefineExpression (DefineExpression'),
    newDefineExpression,
    DefineExpressionResponse (DefineExpressionResponse'),
    newDefineExpressionResponse,

    -- ** DefineIndexField
    DefineIndexField (DefineIndexField'),
    newDefineIndexField,
    DefineIndexFieldResponse (DefineIndexFieldResponse'),
    newDefineIndexFieldResponse,

    -- ** DefineSuggester
    DefineSuggester (DefineSuggester'),
    newDefineSuggester,
    DefineSuggesterResponse (DefineSuggesterResponse'),
    newDefineSuggesterResponse,

    -- ** DeleteAnalysisScheme
    DeleteAnalysisScheme (DeleteAnalysisScheme'),
    newDeleteAnalysisScheme,
    DeleteAnalysisSchemeResponse (DeleteAnalysisSchemeResponse'),
    newDeleteAnalysisSchemeResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteExpression
    DeleteExpression (DeleteExpression'),
    newDeleteExpression,
    DeleteExpressionResponse (DeleteExpressionResponse'),
    newDeleteExpressionResponse,

    -- ** DeleteIndexField
    DeleteIndexField (DeleteIndexField'),
    newDeleteIndexField,
    DeleteIndexFieldResponse (DeleteIndexFieldResponse'),
    newDeleteIndexFieldResponse,

    -- ** DeleteSuggester
    DeleteSuggester (DeleteSuggester'),
    newDeleteSuggester,
    DeleteSuggesterResponse (DeleteSuggesterResponse'),
    newDeleteSuggesterResponse,

    -- ** DescribeAnalysisSchemes
    DescribeAnalysisSchemes (DescribeAnalysisSchemes'),
    newDescribeAnalysisSchemes,
    DescribeAnalysisSchemesResponse (DescribeAnalysisSchemesResponse'),
    newDescribeAnalysisSchemesResponse,

    -- ** DescribeAvailabilityOptions
    DescribeAvailabilityOptions (DescribeAvailabilityOptions'),
    newDescribeAvailabilityOptions,
    DescribeAvailabilityOptionsResponse (DescribeAvailabilityOptionsResponse'),
    newDescribeAvailabilityOptionsResponse,

    -- ** DescribeDomainEndpointOptions
    DescribeDomainEndpointOptions (DescribeDomainEndpointOptions'),
    newDescribeDomainEndpointOptions,
    DescribeDomainEndpointOptionsResponse (DescribeDomainEndpointOptionsResponse'),
    newDescribeDomainEndpointOptionsResponse,

    -- ** DescribeDomains
    DescribeDomains (DescribeDomains'),
    newDescribeDomains,
    DescribeDomainsResponse (DescribeDomainsResponse'),
    newDescribeDomainsResponse,

    -- ** DescribeExpressions
    DescribeExpressions (DescribeExpressions'),
    newDescribeExpressions,
    DescribeExpressionsResponse (DescribeExpressionsResponse'),
    newDescribeExpressionsResponse,

    -- ** DescribeIndexFields
    DescribeIndexFields (DescribeIndexFields'),
    newDescribeIndexFields,
    DescribeIndexFieldsResponse (DescribeIndexFieldsResponse'),
    newDescribeIndexFieldsResponse,

    -- ** DescribeScalingParameters
    DescribeScalingParameters (DescribeScalingParameters'),
    newDescribeScalingParameters,
    DescribeScalingParametersResponse (DescribeScalingParametersResponse'),
    newDescribeScalingParametersResponse,

    -- ** DescribeServiceAccessPolicies
    DescribeServiceAccessPolicies (DescribeServiceAccessPolicies'),
    newDescribeServiceAccessPolicies,
    DescribeServiceAccessPoliciesResponse (DescribeServiceAccessPoliciesResponse'),
    newDescribeServiceAccessPoliciesResponse,

    -- ** DescribeSuggesters
    DescribeSuggesters (DescribeSuggesters'),
    newDescribeSuggesters,
    DescribeSuggestersResponse (DescribeSuggestersResponse'),
    newDescribeSuggestersResponse,

    -- ** IndexDocuments
    IndexDocuments (IndexDocuments'),
    newIndexDocuments,
    IndexDocumentsResponse (IndexDocumentsResponse'),
    newIndexDocumentsResponse,

    -- ** ListDomainNames
    ListDomainNames (ListDomainNames'),
    newListDomainNames,
    ListDomainNamesResponse (ListDomainNamesResponse'),
    newListDomainNamesResponse,

    -- ** UpdateAvailabilityOptions
    UpdateAvailabilityOptions (UpdateAvailabilityOptions'),
    newUpdateAvailabilityOptions,
    UpdateAvailabilityOptionsResponse (UpdateAvailabilityOptionsResponse'),
    newUpdateAvailabilityOptionsResponse,

    -- ** UpdateDomainEndpointOptions
    UpdateDomainEndpointOptions (UpdateDomainEndpointOptions'),
    newUpdateDomainEndpointOptions,
    UpdateDomainEndpointOptionsResponse (UpdateDomainEndpointOptionsResponse'),
    newUpdateDomainEndpointOptionsResponse,

    -- ** UpdateScalingParameters
    UpdateScalingParameters (UpdateScalingParameters'),
    newUpdateScalingParameters,
    UpdateScalingParametersResponse (UpdateScalingParametersResponse'),
    newUpdateScalingParametersResponse,

    -- ** UpdateServiceAccessPolicies
    UpdateServiceAccessPolicies (UpdateServiceAccessPolicies'),
    newUpdateServiceAccessPolicies,
    UpdateServiceAccessPoliciesResponse (UpdateServiceAccessPoliciesResponse'),
    newUpdateServiceAccessPoliciesResponse,

    -- * Types

    -- ** AlgorithmicStemming
    AlgorithmicStemming (..),

    -- ** AnalysisSchemeLanguage
    AnalysisSchemeLanguage (..),

    -- ** IndexFieldType
    IndexFieldType (..),

    -- ** OptionState
    OptionState (..),

    -- ** PartitionInstanceType
    PartitionInstanceType (..),

    -- ** SuggesterFuzzyMatching
    SuggesterFuzzyMatching (..),

    -- ** TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- ** AccessPoliciesStatus
    AccessPoliciesStatus (AccessPoliciesStatus'),
    newAccessPoliciesStatus,

    -- ** AnalysisOptions
    AnalysisOptions (AnalysisOptions'),
    newAnalysisOptions,

    -- ** AnalysisScheme
    AnalysisScheme (AnalysisScheme'),
    newAnalysisScheme,

    -- ** AnalysisSchemeStatus
    AnalysisSchemeStatus (AnalysisSchemeStatus'),
    newAnalysisSchemeStatus,

    -- ** AvailabilityOptionsStatus
    AvailabilityOptionsStatus (AvailabilityOptionsStatus'),
    newAvailabilityOptionsStatus,

    -- ** DateArrayOptions
    DateArrayOptions (DateArrayOptions'),
    newDateArrayOptions,

    -- ** DateOptions
    DateOptions (DateOptions'),
    newDateOptions,

    -- ** DocumentSuggesterOptions
    DocumentSuggesterOptions (DocumentSuggesterOptions'),
    newDocumentSuggesterOptions,

    -- ** DomainEndpointOptions
    DomainEndpointOptions (DomainEndpointOptions'),
    newDomainEndpointOptions,

    -- ** DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (DomainEndpointOptionsStatus'),
    newDomainEndpointOptionsStatus,

    -- ** DomainStatus
    DomainStatus (DomainStatus'),
    newDomainStatus,

    -- ** DoubleArrayOptions
    DoubleArrayOptions (DoubleArrayOptions'),
    newDoubleArrayOptions,

    -- ** DoubleOptions
    DoubleOptions (DoubleOptions'),
    newDoubleOptions,

    -- ** Expression
    Expression (Expression'),
    newExpression,

    -- ** ExpressionStatus
    ExpressionStatus (ExpressionStatus'),
    newExpressionStatus,

    -- ** IndexField
    IndexField (IndexField'),
    newIndexField,

    -- ** IndexFieldStatus
    IndexFieldStatus (IndexFieldStatus'),
    newIndexFieldStatus,

    -- ** IntArrayOptions
    IntArrayOptions (IntArrayOptions'),
    newIntArrayOptions,

    -- ** IntOptions
    IntOptions (IntOptions'),
    newIntOptions,

    -- ** LatLonOptions
    LatLonOptions (LatLonOptions'),
    newLatLonOptions,

    -- ** Limits
    Limits (Limits'),
    newLimits,

    -- ** LiteralArrayOptions
    LiteralArrayOptions (LiteralArrayOptions'),
    newLiteralArrayOptions,

    -- ** LiteralOptions
    LiteralOptions (LiteralOptions'),
    newLiteralOptions,

    -- ** OptionStatus
    OptionStatus (OptionStatus'),
    newOptionStatus,

    -- ** ScalingParameters
    ScalingParameters (ScalingParameters'),
    newScalingParameters,

    -- ** ScalingParametersStatus
    ScalingParametersStatus (ScalingParametersStatus'),
    newScalingParametersStatus,

    -- ** ServiceEndpoint
    ServiceEndpoint (ServiceEndpoint'),
    newServiceEndpoint,

    -- ** Suggester
    Suggester (Suggester'),
    newSuggester,

    -- ** SuggesterStatus
    SuggesterStatus (SuggesterStatus'),
    newSuggesterStatus,

    -- ** TextArrayOptions
    TextArrayOptions (TextArrayOptions'),
    newTextArrayOptions,

    -- ** TextOptions
    TextOptions (TextOptions'),
    newTextOptions,
  )
where

import Amazonka.CloudSearch.BuildSuggesters
import Amazonka.CloudSearch.CreateDomain
import Amazonka.CloudSearch.DefineAnalysisScheme
import Amazonka.CloudSearch.DefineExpression
import Amazonka.CloudSearch.DefineIndexField
import Amazonka.CloudSearch.DefineSuggester
import Amazonka.CloudSearch.DeleteAnalysisScheme
import Amazonka.CloudSearch.DeleteDomain
import Amazonka.CloudSearch.DeleteExpression
import Amazonka.CloudSearch.DeleteIndexField
import Amazonka.CloudSearch.DeleteSuggester
import Amazonka.CloudSearch.DescribeAnalysisSchemes
import Amazonka.CloudSearch.DescribeAvailabilityOptions
import Amazonka.CloudSearch.DescribeDomainEndpointOptions
import Amazonka.CloudSearch.DescribeDomains
import Amazonka.CloudSearch.DescribeExpressions
import Amazonka.CloudSearch.DescribeIndexFields
import Amazonka.CloudSearch.DescribeScalingParameters
import Amazonka.CloudSearch.DescribeServiceAccessPolicies
import Amazonka.CloudSearch.DescribeSuggesters
import Amazonka.CloudSearch.IndexDocuments
import Amazonka.CloudSearch.Lens
import Amazonka.CloudSearch.ListDomainNames
import Amazonka.CloudSearch.Types
import Amazonka.CloudSearch.UpdateAvailabilityOptions
import Amazonka.CloudSearch.UpdateDomainEndpointOptions
import Amazonka.CloudSearch.UpdateScalingParameters
import Amazonka.CloudSearch.UpdateServiceAccessPolicies
import Amazonka.CloudSearch.Waiters

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
