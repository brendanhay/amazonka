{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Lens
  ( -- * Operations

    -- ** DefineExpression
    defineExpression_domainName,
    defineExpression_expression,
    defineExpressionResponse_httpStatus,
    defineExpressionResponse_expression,

    -- ** ListDomainNames
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** DefineSuggester
    defineSuggester_domainName,
    defineSuggester_suggester,
    defineSuggesterResponse_httpStatus,
    defineSuggesterResponse_suggester,

    -- ** DescribeDomains
    describeDomains_domainNames,
    describeDomainsResponse_httpStatus,
    describeDomainsResponse_domainStatusList,

    -- ** DescribeDomainEndpointOptions
    describeDomainEndpointOptions_deployed,
    describeDomainEndpointOptions_domainName,
    describeDomainEndpointOptionsResponse_domainEndpointOptions,
    describeDomainEndpointOptionsResponse_httpStatus,

    -- ** DeleteExpression
    deleteExpression_domainName,
    deleteExpression_expressionName,
    deleteExpressionResponse_httpStatus,
    deleteExpressionResponse_expression,

    -- ** DeleteIndexField
    deleteIndexField_domainName,
    deleteIndexField_indexFieldName,
    deleteIndexFieldResponse_httpStatus,
    deleteIndexFieldResponse_indexField,

    -- ** DeleteSuggester
    deleteSuggester_domainName,
    deleteSuggester_suggesterName,
    deleteSuggesterResponse_httpStatus,
    deleteSuggesterResponse_suggester,

    -- ** UpdateDomainEndpointOptions
    updateDomainEndpointOptions_domainName,
    updateDomainEndpointOptions_domainEndpointOptions,
    updateDomainEndpointOptionsResponse_domainEndpointOptions,
    updateDomainEndpointOptionsResponse_httpStatus,

    -- ** DescribeIndexFields
    describeIndexFields_deployed,
    describeIndexFields_fieldNames,
    describeIndexFields_domainName,
    describeIndexFieldsResponse_httpStatus,
    describeIndexFieldsResponse_indexFields,

    -- ** CreateDomain
    createDomain_domainName,
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,

    -- ** DescribeExpressions
    describeExpressions_deployed,
    describeExpressions_expressionNames,
    describeExpressions_domainName,
    describeExpressionsResponse_httpStatus,
    describeExpressionsResponse_expressions,

    -- ** DescribeAvailabilityOptions
    describeAvailabilityOptions_deployed,
    describeAvailabilityOptions_domainName,
    describeAvailabilityOptionsResponse_availabilityOptions,
    describeAvailabilityOptionsResponse_httpStatus,

    -- ** DefineIndexField
    defineIndexField_domainName,
    defineIndexField_indexField,
    defineIndexFieldResponse_httpStatus,
    defineIndexFieldResponse_indexField,

    -- ** DescribeAnalysisSchemes
    describeAnalysisSchemes_deployed,
    describeAnalysisSchemes_analysisSchemeNames,
    describeAnalysisSchemes_domainName,
    describeAnalysisSchemesResponse_httpStatus,
    describeAnalysisSchemesResponse_analysisSchemes,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_domainStatus,
    deleteDomainResponse_httpStatus,

    -- ** DeleteAnalysisScheme
    deleteAnalysisScheme_domainName,
    deleteAnalysisScheme_analysisSchemeName,
    deleteAnalysisSchemeResponse_httpStatus,
    deleteAnalysisSchemeResponse_analysisScheme,

    -- ** UpdateScalingParameters
    updateScalingParameters_domainName,
    updateScalingParameters_scalingParameters,
    updateScalingParametersResponse_httpStatus,
    updateScalingParametersResponse_scalingParameters,

    -- ** BuildSuggesters
    buildSuggesters_domainName,
    buildSuggestersResponse_fieldNames,
    buildSuggestersResponse_httpStatus,

    -- ** UpdateServiceAccessPolicies
    updateServiceAccessPolicies_domainName,
    updateServiceAccessPolicies_accessPolicies,
    updateServiceAccessPoliciesResponse_httpStatus,
    updateServiceAccessPoliciesResponse_accessPolicies,

    -- ** UpdateAvailabilityOptions
    updateAvailabilityOptions_domainName,
    updateAvailabilityOptions_multiAZ,
    updateAvailabilityOptionsResponse_availabilityOptions,
    updateAvailabilityOptionsResponse_httpStatus,

    -- ** DescribeSuggesters
    describeSuggesters_deployed,
    describeSuggesters_suggesterNames,
    describeSuggesters_domainName,
    describeSuggestersResponse_httpStatus,
    describeSuggestersResponse_suggesters,

    -- ** DescribeServiceAccessPolicies
    describeServiceAccessPolicies_deployed,
    describeServiceAccessPolicies_domainName,
    describeServiceAccessPoliciesResponse_httpStatus,
    describeServiceAccessPoliciesResponse_accessPolicies,

    -- ** DefineAnalysisScheme
    defineAnalysisScheme_domainName,
    defineAnalysisScheme_analysisScheme,
    defineAnalysisSchemeResponse_httpStatus,
    defineAnalysisSchemeResponse_analysisScheme,

    -- ** IndexDocuments
    indexDocuments_domainName,
    indexDocumentsResponse_fieldNames,
    indexDocumentsResponse_httpStatus,

    -- ** DescribeScalingParameters
    describeScalingParameters_domainName,
    describeScalingParametersResponse_httpStatus,
    describeScalingParametersResponse_scalingParameters,

    -- * Types

    -- ** AccessPoliciesStatus
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- ** AnalysisOptions
    analysisOptions_stopwords,
    analysisOptions_algorithmicStemming,
    analysisOptions_stemmingDictionary,
    analysisOptions_japaneseTokenizationDictionary,
    analysisOptions_synonyms,

    -- ** AnalysisScheme
    analysisScheme_analysisOptions,
    analysisScheme_analysisSchemeName,
    analysisScheme_analysisSchemeLanguage,

    -- ** AnalysisSchemeStatus
    analysisSchemeStatus_options,
    analysisSchemeStatus_status,

    -- ** AvailabilityOptionsStatus
    availabilityOptionsStatus_options,
    availabilityOptionsStatus_status,

    -- ** DateArrayOptions
    dateArrayOptions_sourceFields,
    dateArrayOptions_facetEnabled,
    dateArrayOptions_returnEnabled,
    dateArrayOptions_searchEnabled,
    dateArrayOptions_defaultValue,

    -- ** DateOptions
    dateOptions_sortEnabled,
    dateOptions_facetEnabled,
    dateOptions_returnEnabled,
    dateOptions_sourceField,
    dateOptions_searchEnabled,
    dateOptions_defaultValue,

    -- ** DocumentSuggesterOptions
    documentSuggesterOptions_fuzzyMatching,
    documentSuggesterOptions_sortExpression,
    documentSuggesterOptions_sourceField,

    -- ** DomainEndpointOptions
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,

    -- ** DomainEndpointOptionsStatus
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- ** DomainStatus
    domainStatus_searchInstanceType,
    domainStatus_arn,
    domainStatus_searchPartitionCount,
    domainStatus_searchInstanceCount,
    domainStatus_limits,
    domainStatus_searchService,
    domainStatus_processing,
    domainStatus_created,
    domainStatus_deleted,
    domainStatus_docService,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_requiresIndexDocuments,

    -- ** DoubleArrayOptions
    doubleArrayOptions_sourceFields,
    doubleArrayOptions_facetEnabled,
    doubleArrayOptions_returnEnabled,
    doubleArrayOptions_searchEnabled,
    doubleArrayOptions_defaultValue,

    -- ** DoubleOptions
    doubleOptions_sortEnabled,
    doubleOptions_facetEnabled,
    doubleOptions_returnEnabled,
    doubleOptions_sourceField,
    doubleOptions_searchEnabled,
    doubleOptions_defaultValue,

    -- ** Expression
    expression_expressionName,
    expression_expressionValue,

    -- ** ExpressionStatus
    expressionStatus_options,
    expressionStatus_status,

    -- ** IndexField
    indexField_doubleArrayOptions,
    indexField_latLonOptions,
    indexField_textArrayOptions,
    indexField_dateArrayOptions,
    indexField_doubleOptions,
    indexField_textOptions,
    indexField_intArrayOptions,
    indexField_literalArrayOptions,
    indexField_dateOptions,
    indexField_intOptions,
    indexField_literalOptions,
    indexField_indexFieldName,
    indexField_indexFieldType,

    -- ** IndexFieldStatus
    indexFieldStatus_options,
    indexFieldStatus_status,

    -- ** IntArrayOptions
    intArrayOptions_sourceFields,
    intArrayOptions_facetEnabled,
    intArrayOptions_returnEnabled,
    intArrayOptions_searchEnabled,
    intArrayOptions_defaultValue,

    -- ** IntOptions
    intOptions_sortEnabled,
    intOptions_facetEnabled,
    intOptions_returnEnabled,
    intOptions_sourceField,
    intOptions_searchEnabled,
    intOptions_defaultValue,

    -- ** LatLonOptions
    latLonOptions_sortEnabled,
    latLonOptions_facetEnabled,
    latLonOptions_returnEnabled,
    latLonOptions_sourceField,
    latLonOptions_searchEnabled,
    latLonOptions_defaultValue,

    -- ** Limits
    limits_maximumReplicationCount,
    limits_maximumPartitionCount,

    -- ** LiteralArrayOptions
    literalArrayOptions_sourceFields,
    literalArrayOptions_facetEnabled,
    literalArrayOptions_returnEnabled,
    literalArrayOptions_searchEnabled,
    literalArrayOptions_defaultValue,

    -- ** LiteralOptions
    literalOptions_sortEnabled,
    literalOptions_facetEnabled,
    literalOptions_returnEnabled,
    literalOptions_sourceField,
    literalOptions_searchEnabled,
    literalOptions_defaultValue,

    -- ** OptionStatus
    optionStatus_updateVersion,
    optionStatus_pendingDeletion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- ** ScalingParameters
    scalingParameters_desiredReplicationCount,
    scalingParameters_desiredPartitionCount,
    scalingParameters_desiredInstanceType,

    -- ** ScalingParametersStatus
    scalingParametersStatus_options,
    scalingParametersStatus_status,

    -- ** ServiceEndpoint
    serviceEndpoint_endpoint,

    -- ** Suggester
    suggester_suggesterName,
    suggester_documentSuggesterOptions,

    -- ** SuggesterStatus
    suggesterStatus_options,
    suggesterStatus_status,

    -- ** TextArrayOptions
    textArrayOptions_analysisScheme,
    textArrayOptions_sourceFields,
    textArrayOptions_returnEnabled,
    textArrayOptions_defaultValue,
    textArrayOptions_highlightEnabled,

    -- ** TextOptions
    textOptions_sortEnabled,
    textOptions_analysisScheme,
    textOptions_returnEnabled,
    textOptions_sourceField,
    textOptions_defaultValue,
    textOptions_highlightEnabled,
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
import Network.AWS.CloudSearch.Types.AccessPoliciesStatus
import Network.AWS.CloudSearch.Types.AnalysisOptions
import Network.AWS.CloudSearch.Types.AnalysisScheme
import Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
import Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
import Network.AWS.CloudSearch.Types.DateArrayOptions
import Network.AWS.CloudSearch.Types.DateOptions
import Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
import Network.AWS.CloudSearch.Types.DomainEndpointOptions
import Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.CloudSearch.Types.DomainStatus
import Network.AWS.CloudSearch.Types.DoubleArrayOptions
import Network.AWS.CloudSearch.Types.DoubleOptions
import Network.AWS.CloudSearch.Types.Expression
import Network.AWS.CloudSearch.Types.ExpressionStatus
import Network.AWS.CloudSearch.Types.IndexField
import Network.AWS.CloudSearch.Types.IndexFieldStatus
import Network.AWS.CloudSearch.Types.IntArrayOptions
import Network.AWS.CloudSearch.Types.IntOptions
import Network.AWS.CloudSearch.Types.LatLonOptions
import Network.AWS.CloudSearch.Types.Limits
import Network.AWS.CloudSearch.Types.LiteralArrayOptions
import Network.AWS.CloudSearch.Types.LiteralOptions
import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.CloudSearch.Types.ScalingParameters
import Network.AWS.CloudSearch.Types.ScalingParametersStatus
import Network.AWS.CloudSearch.Types.ServiceEndpoint
import Network.AWS.CloudSearch.Types.Suggester
import Network.AWS.CloudSearch.Types.SuggesterStatus
import Network.AWS.CloudSearch.Types.TextArrayOptions
import Network.AWS.CloudSearch.Types.TextOptions
import Network.AWS.CloudSearch.UpdateAvailabilityOptions
import Network.AWS.CloudSearch.UpdateDomainEndpointOptions
import Network.AWS.CloudSearch.UpdateScalingParameters
import Network.AWS.CloudSearch.UpdateServiceAccessPolicies
