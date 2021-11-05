{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Lens
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
import Amazonka.CloudSearch.ListDomainNames
import Amazonka.CloudSearch.Types.AccessPoliciesStatus
import Amazonka.CloudSearch.Types.AnalysisOptions
import Amazonka.CloudSearch.Types.AnalysisScheme
import Amazonka.CloudSearch.Types.AnalysisSchemeStatus
import Amazonka.CloudSearch.Types.AvailabilityOptionsStatus
import Amazonka.CloudSearch.Types.DateArrayOptions
import Amazonka.CloudSearch.Types.DateOptions
import Amazonka.CloudSearch.Types.DocumentSuggesterOptions
import Amazonka.CloudSearch.Types.DomainEndpointOptions
import Amazonka.CloudSearch.Types.DomainEndpointOptionsStatus
import Amazonka.CloudSearch.Types.DomainStatus
import Amazonka.CloudSearch.Types.DoubleArrayOptions
import Amazonka.CloudSearch.Types.DoubleOptions
import Amazonka.CloudSearch.Types.Expression
import Amazonka.CloudSearch.Types.ExpressionStatus
import Amazonka.CloudSearch.Types.IndexField
import Amazonka.CloudSearch.Types.IndexFieldStatus
import Amazonka.CloudSearch.Types.IntArrayOptions
import Amazonka.CloudSearch.Types.IntOptions
import Amazonka.CloudSearch.Types.LatLonOptions
import Amazonka.CloudSearch.Types.Limits
import Amazonka.CloudSearch.Types.LiteralArrayOptions
import Amazonka.CloudSearch.Types.LiteralOptions
import Amazonka.CloudSearch.Types.OptionStatus
import Amazonka.CloudSearch.Types.ScalingParameters
import Amazonka.CloudSearch.Types.ScalingParametersStatus
import Amazonka.CloudSearch.Types.ServiceEndpoint
import Amazonka.CloudSearch.Types.Suggester
import Amazonka.CloudSearch.Types.SuggesterStatus
import Amazonka.CloudSearch.Types.TextArrayOptions
import Amazonka.CloudSearch.Types.TextOptions
import Amazonka.CloudSearch.UpdateAvailabilityOptions
import Amazonka.CloudSearch.UpdateDomainEndpointOptions
import Amazonka.CloudSearch.UpdateScalingParameters
import Amazonka.CloudSearch.UpdateServiceAccessPolicies
