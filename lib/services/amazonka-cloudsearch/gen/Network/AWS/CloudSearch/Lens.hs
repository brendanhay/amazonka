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

    -- ** DescribeAvailabilityOptions
    describeAvailabilityOptions_deployed,
    describeAvailabilityOptions_domainName,
    describeAvailabilityOptionsResponse_availabilityOptions,
    describeAvailabilityOptionsResponse_httpStatus,

    -- ** DescribeExpressions
    describeExpressions_deployed,
    describeExpressions_expressionNames,
    describeExpressions_domainName,
    describeExpressionsResponse_httpStatus,
    describeExpressionsResponse_expressions,

    -- ** DefineExpression
    defineExpression_domainName,
    defineExpression_expression,
    defineExpressionResponse_httpStatus,
    defineExpressionResponse_expression,

    -- ** DescribeScalingParameters
    describeScalingParameters_domainName,
    describeScalingParametersResponse_httpStatus,
    describeScalingParametersResponse_scalingParameters,

    -- ** DescribeServiceAccessPolicies
    describeServiceAccessPolicies_deployed,
    describeServiceAccessPolicies_domainName,
    describeServiceAccessPoliciesResponse_httpStatus,
    describeServiceAccessPoliciesResponse_accessPolicies,

    -- ** DescribeSuggesters
    describeSuggesters_deployed,
    describeSuggesters_suggesterNames,
    describeSuggesters_domainName,
    describeSuggestersResponse_httpStatus,
    describeSuggestersResponse_suggesters,

    -- ** UpdateAvailabilityOptions
    updateAvailabilityOptions_domainName,
    updateAvailabilityOptions_multiAZ,
    updateAvailabilityOptionsResponse_availabilityOptions,
    updateAvailabilityOptionsResponse_httpStatus,

    -- ** DeleteExpression
    deleteExpression_domainName,
    deleteExpression_expressionName,
    deleteExpressionResponse_httpStatus,
    deleteExpressionResponse_expression,

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

    -- ** DeleteAnalysisScheme
    deleteAnalysisScheme_domainName,
    deleteAnalysisScheme_analysisSchemeName,
    deleteAnalysisSchemeResponse_httpStatus,
    deleteAnalysisSchemeResponse_analysisScheme,

    -- ** DescribeDomainEndpointOptions
    describeDomainEndpointOptions_deployed,
    describeDomainEndpointOptions_domainName,
    describeDomainEndpointOptionsResponse_domainEndpointOptions,
    describeDomainEndpointOptionsResponse_httpStatus,

    -- ** DescribeAnalysisSchemes
    describeAnalysisSchemes_deployed,
    describeAnalysisSchemes_analysisSchemeNames,
    describeAnalysisSchemes_domainName,
    describeAnalysisSchemesResponse_httpStatus,
    describeAnalysisSchemesResponse_analysisSchemes,

    -- ** CreateDomain
    createDomain_domainName,
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,

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

    -- ** DeleteSuggester
    deleteSuggester_domainName,
    deleteSuggester_suggesterName,
    deleteSuggesterResponse_httpStatus,
    deleteSuggesterResponse_suggester,

    -- ** DefineAnalysisScheme
    defineAnalysisScheme_domainName,
    defineAnalysisScheme_analysisScheme,
    defineAnalysisSchemeResponse_httpStatus,
    defineAnalysisSchemeResponse_analysisScheme,

    -- ** IndexDocuments
    indexDocuments_domainName,
    indexDocumentsResponse_fieldNames,
    indexDocumentsResponse_httpStatus,

    -- ** DeleteIndexField
    deleteIndexField_domainName,
    deleteIndexField_indexFieldName,
    deleteIndexFieldResponse_httpStatus,
    deleteIndexFieldResponse_indexField,

    -- ** UpdateServiceAccessPolicies
    updateServiceAccessPolicies_domainName,
    updateServiceAccessPolicies_accessPolicies,
    updateServiceAccessPoliciesResponse_httpStatus,
    updateServiceAccessPoliciesResponse_accessPolicies,

    -- ** UpdateScalingParameters
    updateScalingParameters_domainName,
    updateScalingParameters_scalingParameters,
    updateScalingParametersResponse_httpStatus,
    updateScalingParametersResponse_scalingParameters,

    -- ** BuildSuggesters
    buildSuggesters_domainName,
    buildSuggestersResponse_fieldNames,
    buildSuggestersResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_domainStatus,
    deleteDomainResponse_httpStatus,

    -- ** DefineIndexField
    defineIndexField_domainName,
    defineIndexField_indexField,
    defineIndexFieldResponse_httpStatus,
    defineIndexFieldResponse_indexField,

    -- * Types

    -- ** AccessPoliciesStatus
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- ** AnalysisOptions
    analysisOptions_algorithmicStemming,
    analysisOptions_stopwords,
    analysisOptions_japaneseTokenizationDictionary,
    analysisOptions_synonyms,
    analysisOptions_stemmingDictionary,

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
    dateArrayOptions_returnEnabled,
    dateArrayOptions_facetEnabled,
    dateArrayOptions_searchEnabled,
    dateArrayOptions_defaultValue,

    -- ** DateOptions
    dateOptions_sourceField,
    dateOptions_returnEnabled,
    dateOptions_facetEnabled,
    dateOptions_searchEnabled,
    dateOptions_sortEnabled,
    dateOptions_defaultValue,

    -- ** DocumentSuggesterOptions
    documentSuggesterOptions_sortExpression,
    documentSuggesterOptions_fuzzyMatching,
    documentSuggesterOptions_sourceField,

    -- ** DomainEndpointOptions
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,

    -- ** DomainEndpointOptionsStatus
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- ** DomainStatus
    domainStatus_searchInstanceCount,
    domainStatus_searchInstanceType,
    domainStatus_docService,
    domainStatus_arn,
    domainStatus_created,
    domainStatus_searchService,
    domainStatus_limits,
    domainStatus_searchPartitionCount,
    domainStatus_deleted,
    domainStatus_processing,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_requiresIndexDocuments,

    -- ** DoubleArrayOptions
    doubleArrayOptions_sourceFields,
    doubleArrayOptions_returnEnabled,
    doubleArrayOptions_facetEnabled,
    doubleArrayOptions_searchEnabled,
    doubleArrayOptions_defaultValue,

    -- ** DoubleOptions
    doubleOptions_sourceField,
    doubleOptions_returnEnabled,
    doubleOptions_facetEnabled,
    doubleOptions_searchEnabled,
    doubleOptions_sortEnabled,
    doubleOptions_defaultValue,

    -- ** Expression
    expression_expressionName,
    expression_expressionValue,

    -- ** ExpressionStatus
    expressionStatus_options,
    expressionStatus_status,

    -- ** IndexField
    indexField_doubleArrayOptions,
    indexField_dateOptions,
    indexField_textArrayOptions,
    indexField_doubleOptions,
    indexField_textOptions,
    indexField_latLonOptions,
    indexField_literalArrayOptions,
    indexField_intArrayOptions,
    indexField_dateArrayOptions,
    indexField_intOptions,
    indexField_literalOptions,
    indexField_indexFieldName,
    indexField_indexFieldType,

    -- ** IndexFieldStatus
    indexFieldStatus_options,
    indexFieldStatus_status,

    -- ** IntArrayOptions
    intArrayOptions_sourceFields,
    intArrayOptions_returnEnabled,
    intArrayOptions_facetEnabled,
    intArrayOptions_searchEnabled,
    intArrayOptions_defaultValue,

    -- ** IntOptions
    intOptions_sourceField,
    intOptions_returnEnabled,
    intOptions_facetEnabled,
    intOptions_searchEnabled,
    intOptions_sortEnabled,
    intOptions_defaultValue,

    -- ** LatLonOptions
    latLonOptions_sourceField,
    latLonOptions_returnEnabled,
    latLonOptions_facetEnabled,
    latLonOptions_searchEnabled,
    latLonOptions_sortEnabled,
    latLonOptions_defaultValue,

    -- ** Limits
    limits_maximumReplicationCount,
    limits_maximumPartitionCount,

    -- ** LiteralArrayOptions
    literalArrayOptions_sourceFields,
    literalArrayOptions_returnEnabled,
    literalArrayOptions_facetEnabled,
    literalArrayOptions_searchEnabled,
    literalArrayOptions_defaultValue,

    -- ** LiteralOptions
    literalOptions_sourceField,
    literalOptions_returnEnabled,
    literalOptions_facetEnabled,
    literalOptions_searchEnabled,
    literalOptions_sortEnabled,
    literalOptions_defaultValue,

    -- ** OptionStatus
    optionStatus_pendingDeletion,
    optionStatus_updateVersion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- ** ScalingParameters
    scalingParameters_desiredInstanceType,
    scalingParameters_desiredReplicationCount,
    scalingParameters_desiredPartitionCount,

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
    textArrayOptions_sourceFields,
    textArrayOptions_returnEnabled,
    textArrayOptions_analysisScheme,
    textArrayOptions_highlightEnabled,
    textArrayOptions_defaultValue,

    -- ** TextOptions
    textOptions_sourceField,
    textOptions_returnEnabled,
    textOptions_analysisScheme,
    textOptions_highlightEnabled,
    textOptions_sortEnabled,
    textOptions_defaultValue,
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
