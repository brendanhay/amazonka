{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearch.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Lens
  ( -- * Operations

    -- ** BuildSuggesters
    buildSuggesters_domainName,
    buildSuggestersResponse_fieldNames,
    buildSuggestersResponse_httpStatus,

    -- ** CreateDomain
    createDomain_domainName,
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,

    -- ** DefineAnalysisScheme
    defineAnalysisScheme_domainName,
    defineAnalysisScheme_analysisScheme,
    defineAnalysisSchemeResponse_httpStatus,
    defineAnalysisSchemeResponse_analysisScheme,

    -- ** DefineExpression
    defineExpression_domainName,
    defineExpression_expression,
    defineExpressionResponse_httpStatus,
    defineExpressionResponse_expression,

    -- ** DefineIndexField
    defineIndexField_domainName,
    defineIndexField_indexField,
    defineIndexFieldResponse_httpStatus,
    defineIndexFieldResponse_indexField,

    -- ** DefineSuggester
    defineSuggester_domainName,
    defineSuggester_suggester,
    defineSuggesterResponse_httpStatus,
    defineSuggesterResponse_suggester,

    -- ** DeleteAnalysisScheme
    deleteAnalysisScheme_domainName,
    deleteAnalysisScheme_analysisSchemeName,
    deleteAnalysisSchemeResponse_httpStatus,
    deleteAnalysisSchemeResponse_analysisScheme,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_domainStatus,
    deleteDomainResponse_httpStatus,

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

    -- ** DescribeAnalysisSchemes
    describeAnalysisSchemes_deployed,
    describeAnalysisSchemes_analysisSchemeNames,
    describeAnalysisSchemes_domainName,
    describeAnalysisSchemesResponse_httpStatus,
    describeAnalysisSchemesResponse_analysisSchemes,

    -- ** DescribeAvailabilityOptions
    describeAvailabilityOptions_deployed,
    describeAvailabilityOptions_domainName,
    describeAvailabilityOptionsResponse_availabilityOptions,
    describeAvailabilityOptionsResponse_httpStatus,

    -- ** DescribeDomainEndpointOptions
    describeDomainEndpointOptions_deployed,
    describeDomainEndpointOptions_domainName,
    describeDomainEndpointOptionsResponse_domainEndpointOptions,
    describeDomainEndpointOptionsResponse_httpStatus,

    -- ** DescribeDomains
    describeDomains_domainNames,
    describeDomainsResponse_httpStatus,
    describeDomainsResponse_domainStatusList,

    -- ** DescribeExpressions
    describeExpressions_expressionNames,
    describeExpressions_deployed,
    describeExpressions_domainName,
    describeExpressionsResponse_httpStatus,
    describeExpressionsResponse_expressions,

    -- ** DescribeIndexFields
    describeIndexFields_deployed,
    describeIndexFields_fieldNames,
    describeIndexFields_domainName,
    describeIndexFieldsResponse_httpStatus,
    describeIndexFieldsResponse_indexFields,

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

    -- ** IndexDocuments
    indexDocuments_domainName,
    indexDocumentsResponse_fieldNames,
    indexDocumentsResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** UpdateAvailabilityOptions
    updateAvailabilityOptions_domainName,
    updateAvailabilityOptions_multiAZ,
    updateAvailabilityOptionsResponse_availabilityOptions,
    updateAvailabilityOptionsResponse_httpStatus,

    -- ** UpdateDomainEndpointOptions
    updateDomainEndpointOptions_domainName,
    updateDomainEndpointOptions_domainEndpointOptions,
    updateDomainEndpointOptionsResponse_domainEndpointOptions,
    updateDomainEndpointOptionsResponse_httpStatus,

    -- ** UpdateScalingParameters
    updateScalingParameters_domainName,
    updateScalingParameters_scalingParameters,
    updateScalingParametersResponse_httpStatus,
    updateScalingParametersResponse_scalingParameters,

    -- ** UpdateServiceAccessPolicies
    updateServiceAccessPolicies_domainName,
    updateServiceAccessPolicies_accessPolicies,
    updateServiceAccessPoliciesResponse_httpStatus,
    updateServiceAccessPoliciesResponse_accessPolicies,

    -- * Types

    -- ** AccessPoliciesStatus
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- ** AnalysisOptions
    analysisOptions_stemmingDictionary,
    analysisOptions_algorithmicStemming,
    analysisOptions_japaneseTokenizationDictionary,
    analysisOptions_stopwords,
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
    dateArrayOptions_facetEnabled,
    dateArrayOptions_searchEnabled,
    dateArrayOptions_sourceFields,
    dateArrayOptions_defaultValue,
    dateArrayOptions_returnEnabled,

    -- ** DateOptions
    dateOptions_sourceField,
    dateOptions_facetEnabled,
    dateOptions_searchEnabled,
    dateOptions_sortEnabled,
    dateOptions_defaultValue,
    dateOptions_returnEnabled,

    -- ** DocumentSuggesterOptions
    documentSuggesterOptions_sortExpression,
    documentSuggesterOptions_fuzzyMatching,
    documentSuggesterOptions_sourceField,

    -- ** DomainEndpointOptions
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_enforceHTTPS,

    -- ** DomainEndpointOptionsStatus
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- ** DomainStatus
    domainStatus_limits,
    domainStatus_deleted,
    domainStatus_created,
    domainStatus_arn,
    domainStatus_processing,
    domainStatus_searchPartitionCount,
    domainStatus_searchService,
    domainStatus_searchInstanceType,
    domainStatus_searchInstanceCount,
    domainStatus_docService,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_requiresIndexDocuments,

    -- ** DoubleArrayOptions
    doubleArrayOptions_facetEnabled,
    doubleArrayOptions_searchEnabled,
    doubleArrayOptions_sourceFields,
    doubleArrayOptions_defaultValue,
    doubleArrayOptions_returnEnabled,

    -- ** DoubleOptions
    doubleOptions_sourceField,
    doubleOptions_facetEnabled,
    doubleOptions_searchEnabled,
    doubleOptions_sortEnabled,
    doubleOptions_defaultValue,
    doubleOptions_returnEnabled,

    -- ** Expression
    expression_expressionName,
    expression_expressionValue,

    -- ** ExpressionStatus
    expressionStatus_options,
    expressionStatus_status,

    -- ** IndexField
    indexField_dateOptions,
    indexField_literalArrayOptions,
    indexField_textOptions,
    indexField_doubleArrayOptions,
    indexField_textArrayOptions,
    indexField_dateArrayOptions,
    indexField_doubleOptions,
    indexField_latLonOptions,
    indexField_intArrayOptions,
    indexField_literalOptions,
    indexField_intOptions,
    indexField_indexFieldName,
    indexField_indexFieldType,

    -- ** IndexFieldStatus
    indexFieldStatus_options,
    indexFieldStatus_status,

    -- ** IntArrayOptions
    intArrayOptions_facetEnabled,
    intArrayOptions_searchEnabled,
    intArrayOptions_sourceFields,
    intArrayOptions_defaultValue,
    intArrayOptions_returnEnabled,

    -- ** IntOptions
    intOptions_sourceField,
    intOptions_facetEnabled,
    intOptions_searchEnabled,
    intOptions_sortEnabled,
    intOptions_defaultValue,
    intOptions_returnEnabled,

    -- ** LatLonOptions
    latLonOptions_sourceField,
    latLonOptions_facetEnabled,
    latLonOptions_searchEnabled,
    latLonOptions_sortEnabled,
    latLonOptions_defaultValue,
    latLonOptions_returnEnabled,

    -- ** Limits
    limits_maximumReplicationCount,
    limits_maximumPartitionCount,

    -- ** LiteralArrayOptions
    literalArrayOptions_facetEnabled,
    literalArrayOptions_searchEnabled,
    literalArrayOptions_sourceFields,
    literalArrayOptions_defaultValue,
    literalArrayOptions_returnEnabled,

    -- ** LiteralOptions
    literalOptions_sourceField,
    literalOptions_facetEnabled,
    literalOptions_searchEnabled,
    literalOptions_sortEnabled,
    literalOptions_defaultValue,
    literalOptions_returnEnabled,

    -- ** OptionStatus
    optionStatus_pendingDeletion,
    optionStatus_updateVersion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- ** ScalingParameters
    scalingParameters_desiredPartitionCount,
    scalingParameters_desiredReplicationCount,
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
    textArrayOptions_defaultValue,
    textArrayOptions_returnEnabled,
    textArrayOptions_highlightEnabled,

    -- ** TextOptions
    textOptions_sourceField,
    textOptions_analysisScheme,
    textOptions_sortEnabled,
    textOptions_defaultValue,
    textOptions_returnEnabled,
    textOptions_highlightEnabled,
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
