{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidTypeException,
    _InternalException,
    _BaseException,
    _ValidationException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _DisabledOperationException,

    -- * AlgorithmicStemming
    AlgorithmicStemming (..),

    -- * AnalysisSchemeLanguage
    AnalysisSchemeLanguage (..),

    -- * IndexFieldType
    IndexFieldType (..),

    -- * OptionState
    OptionState (..),

    -- * PartitionInstanceType
    PartitionInstanceType (..),

    -- * SuggesterFuzzyMatching
    SuggesterFuzzyMatching (..),

    -- * TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- * AccessPoliciesStatus
    AccessPoliciesStatus (..),
    newAccessPoliciesStatus,
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- * AnalysisOptions
    AnalysisOptions (..),
    newAnalysisOptions,
    analysisOptions_stopwords,
    analysisOptions_algorithmicStemming,
    analysisOptions_stemmingDictionary,
    analysisOptions_japaneseTokenizationDictionary,
    analysisOptions_synonyms,

    -- * AnalysisScheme
    AnalysisScheme (..),
    newAnalysisScheme,
    analysisScheme_analysisOptions,
    analysisScheme_analysisSchemeName,
    analysisScheme_analysisSchemeLanguage,

    -- * AnalysisSchemeStatus
    AnalysisSchemeStatus (..),
    newAnalysisSchemeStatus,
    analysisSchemeStatus_options,
    analysisSchemeStatus_status,

    -- * AvailabilityOptionsStatus
    AvailabilityOptionsStatus (..),
    newAvailabilityOptionsStatus,
    availabilityOptionsStatus_options,
    availabilityOptionsStatus_status,

    -- * DateArrayOptions
    DateArrayOptions (..),
    newDateArrayOptions,
    dateArrayOptions_sourceFields,
    dateArrayOptions_facetEnabled,
    dateArrayOptions_returnEnabled,
    dateArrayOptions_searchEnabled,
    dateArrayOptions_defaultValue,

    -- * DateOptions
    DateOptions (..),
    newDateOptions,
    dateOptions_sortEnabled,
    dateOptions_facetEnabled,
    dateOptions_returnEnabled,
    dateOptions_sourceField,
    dateOptions_searchEnabled,
    dateOptions_defaultValue,

    -- * DocumentSuggesterOptions
    DocumentSuggesterOptions (..),
    newDocumentSuggesterOptions,
    documentSuggesterOptions_fuzzyMatching,
    documentSuggesterOptions_sortExpression,
    documentSuggesterOptions_sourceField,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    newDomainEndpointOptions,
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    newDomainEndpointOptionsStatus,
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- * DomainStatus
    DomainStatus (..),
    newDomainStatus,
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

    -- * DoubleArrayOptions
    DoubleArrayOptions (..),
    newDoubleArrayOptions,
    doubleArrayOptions_sourceFields,
    doubleArrayOptions_facetEnabled,
    doubleArrayOptions_returnEnabled,
    doubleArrayOptions_searchEnabled,
    doubleArrayOptions_defaultValue,

    -- * DoubleOptions
    DoubleOptions (..),
    newDoubleOptions,
    doubleOptions_sortEnabled,
    doubleOptions_facetEnabled,
    doubleOptions_returnEnabled,
    doubleOptions_sourceField,
    doubleOptions_searchEnabled,
    doubleOptions_defaultValue,

    -- * Expression
    Expression (..),
    newExpression,
    expression_expressionName,
    expression_expressionValue,

    -- * ExpressionStatus
    ExpressionStatus (..),
    newExpressionStatus,
    expressionStatus_options,
    expressionStatus_status,

    -- * IndexField
    IndexField (..),
    newIndexField,
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

    -- * IndexFieldStatus
    IndexFieldStatus (..),
    newIndexFieldStatus,
    indexFieldStatus_options,
    indexFieldStatus_status,

    -- * IntArrayOptions
    IntArrayOptions (..),
    newIntArrayOptions,
    intArrayOptions_sourceFields,
    intArrayOptions_facetEnabled,
    intArrayOptions_returnEnabled,
    intArrayOptions_searchEnabled,
    intArrayOptions_defaultValue,

    -- * IntOptions
    IntOptions (..),
    newIntOptions,
    intOptions_sortEnabled,
    intOptions_facetEnabled,
    intOptions_returnEnabled,
    intOptions_sourceField,
    intOptions_searchEnabled,
    intOptions_defaultValue,

    -- * LatLonOptions
    LatLonOptions (..),
    newLatLonOptions,
    latLonOptions_sortEnabled,
    latLonOptions_facetEnabled,
    latLonOptions_returnEnabled,
    latLonOptions_sourceField,
    latLonOptions_searchEnabled,
    latLonOptions_defaultValue,

    -- * Limits
    Limits (..),
    newLimits,
    limits_maximumReplicationCount,
    limits_maximumPartitionCount,

    -- * LiteralArrayOptions
    LiteralArrayOptions (..),
    newLiteralArrayOptions,
    literalArrayOptions_sourceFields,
    literalArrayOptions_facetEnabled,
    literalArrayOptions_returnEnabled,
    literalArrayOptions_searchEnabled,
    literalArrayOptions_defaultValue,

    -- * LiteralOptions
    LiteralOptions (..),
    newLiteralOptions,
    literalOptions_sortEnabled,
    literalOptions_facetEnabled,
    literalOptions_returnEnabled,
    literalOptions_sourceField,
    literalOptions_searchEnabled,
    literalOptions_defaultValue,

    -- * OptionStatus
    OptionStatus (..),
    newOptionStatus,
    optionStatus_updateVersion,
    optionStatus_pendingDeletion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- * ScalingParameters
    ScalingParameters (..),
    newScalingParameters,
    scalingParameters_desiredReplicationCount,
    scalingParameters_desiredPartitionCount,
    scalingParameters_desiredInstanceType,

    -- * ScalingParametersStatus
    ScalingParametersStatus (..),
    newScalingParametersStatus,
    scalingParametersStatus_options,
    scalingParametersStatus_status,

    -- * ServiceEndpoint
    ServiceEndpoint (..),
    newServiceEndpoint,
    serviceEndpoint_endpoint,

    -- * Suggester
    Suggester (..),
    newSuggester,
    suggester_suggesterName,
    suggester_documentSuggesterOptions,

    -- * SuggesterStatus
    SuggesterStatus (..),
    newSuggesterStatus,
    suggesterStatus_options,
    suggesterStatus_status,

    -- * TextArrayOptions
    TextArrayOptions (..),
    newTextArrayOptions,
    textArrayOptions_analysisScheme,
    textArrayOptions_sourceFields,
    textArrayOptions_returnEnabled,
    textArrayOptions_defaultValue,
    textArrayOptions_highlightEnabled,

    -- * TextOptions
    TextOptions (..),
    newTextOptions,
    textOptions_sortEnabled,
    textOptions_analysisScheme,
    textOptions_returnEnabled,
    textOptions_sourceField,
    textOptions_defaultValue,
    textOptions_highlightEnabled,
  )
where

import Network.AWS.CloudSearch.Types.AccessPoliciesStatus
import Network.AWS.CloudSearch.Types.AlgorithmicStemming
import Network.AWS.CloudSearch.Types.AnalysisOptions
import Network.AWS.CloudSearch.Types.AnalysisScheme
import Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
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
import Network.AWS.CloudSearch.Types.IndexFieldType
import Network.AWS.CloudSearch.Types.IntArrayOptions
import Network.AWS.CloudSearch.Types.IntOptions
import Network.AWS.CloudSearch.Types.LatLonOptions
import Network.AWS.CloudSearch.Types.Limits
import Network.AWS.CloudSearch.Types.LiteralArrayOptions
import Network.AWS.CloudSearch.Types.LiteralOptions
import Network.AWS.CloudSearch.Types.OptionState
import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.CloudSearch.Types.PartitionInstanceType
import Network.AWS.CloudSearch.Types.ScalingParameters
import Network.AWS.CloudSearch.Types.ScalingParametersStatus
import Network.AWS.CloudSearch.Types.ServiceEndpoint
import Network.AWS.CloudSearch.Types.Suggester
import Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
import Network.AWS.CloudSearch.Types.SuggesterStatus
import Network.AWS.CloudSearch.Types.TLSSecurityPolicy
import Network.AWS.CloudSearch.Types.TextArrayOptions
import Network.AWS.CloudSearch.Types.TextOptions
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CloudSearch",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudsearch",
      Core._serviceSigningName = "cloudsearch",
      Core._serviceVersion = "2013-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseXMLError "CloudSearch",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "BandwidthLimitExceeded"
              Core.. Core.hasStatus 509
          )
          e =
        Core.Just "request_limit_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because it specified an invalid type
-- definition.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidType"
    Core.. Core.hasStatus 409

-- | An internal error occurred while processing the request. If this problem
-- persists, report an issue from the
-- <http://status.aws.amazon.com/ Service Health Dashboard>.
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Core.. Core.hasStatus 500

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

-- | The request was rejected because it has invalid parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The request was rejected because a resource limit has already been met.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Core.. Core.hasStatus 409

-- | The request was rejected because it attempted to reference a resource
-- that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Core.. Core.hasStatus 409

-- | The request was rejected because it attempted an operation which is not
-- enabled.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledAction"
    Core.. Core.hasStatus 409
