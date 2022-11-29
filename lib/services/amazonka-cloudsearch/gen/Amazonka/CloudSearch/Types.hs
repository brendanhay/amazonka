{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearch.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _InvalidTypeException,
    _LimitExceededException,
    _DisabledOperationException,
    _InternalException,
    _ValidationException,
    _BaseException,

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
    analysisOptions_stemmingDictionary,
    analysisOptions_algorithmicStemming,
    analysisOptions_japaneseTokenizationDictionary,
    analysisOptions_stopwords,
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
    dateArrayOptions_facetEnabled,
    dateArrayOptions_searchEnabled,
    dateArrayOptions_sourceFields,
    dateArrayOptions_defaultValue,
    dateArrayOptions_returnEnabled,

    -- * DateOptions
    DateOptions (..),
    newDateOptions,
    dateOptions_sourceField,
    dateOptions_facetEnabled,
    dateOptions_searchEnabled,
    dateOptions_sortEnabled,
    dateOptions_defaultValue,
    dateOptions_returnEnabled,

    -- * DocumentSuggesterOptions
    DocumentSuggesterOptions (..),
    newDocumentSuggesterOptions,
    documentSuggesterOptions_sortExpression,
    documentSuggesterOptions_fuzzyMatching,
    documentSuggesterOptions_sourceField,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    newDomainEndpointOptions,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_enforceHTTPS,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    newDomainEndpointOptionsStatus,
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- * DomainStatus
    DomainStatus (..),
    newDomainStatus,
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

    -- * DoubleArrayOptions
    DoubleArrayOptions (..),
    newDoubleArrayOptions,
    doubleArrayOptions_facetEnabled,
    doubleArrayOptions_searchEnabled,
    doubleArrayOptions_sourceFields,
    doubleArrayOptions_defaultValue,
    doubleArrayOptions_returnEnabled,

    -- * DoubleOptions
    DoubleOptions (..),
    newDoubleOptions,
    doubleOptions_sourceField,
    doubleOptions_facetEnabled,
    doubleOptions_searchEnabled,
    doubleOptions_sortEnabled,
    doubleOptions_defaultValue,
    doubleOptions_returnEnabled,

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

    -- * IndexFieldStatus
    IndexFieldStatus (..),
    newIndexFieldStatus,
    indexFieldStatus_options,
    indexFieldStatus_status,

    -- * IntArrayOptions
    IntArrayOptions (..),
    newIntArrayOptions,
    intArrayOptions_facetEnabled,
    intArrayOptions_searchEnabled,
    intArrayOptions_sourceFields,
    intArrayOptions_defaultValue,
    intArrayOptions_returnEnabled,

    -- * IntOptions
    IntOptions (..),
    newIntOptions,
    intOptions_sourceField,
    intOptions_facetEnabled,
    intOptions_searchEnabled,
    intOptions_sortEnabled,
    intOptions_defaultValue,
    intOptions_returnEnabled,

    -- * LatLonOptions
    LatLonOptions (..),
    newLatLonOptions,
    latLonOptions_sourceField,
    latLonOptions_facetEnabled,
    latLonOptions_searchEnabled,
    latLonOptions_sortEnabled,
    latLonOptions_defaultValue,
    latLonOptions_returnEnabled,

    -- * Limits
    Limits (..),
    newLimits,
    limits_maximumReplicationCount,
    limits_maximumPartitionCount,

    -- * LiteralArrayOptions
    LiteralArrayOptions (..),
    newLiteralArrayOptions,
    literalArrayOptions_facetEnabled,
    literalArrayOptions_searchEnabled,
    literalArrayOptions_sourceFields,
    literalArrayOptions_defaultValue,
    literalArrayOptions_returnEnabled,

    -- * LiteralOptions
    LiteralOptions (..),
    newLiteralOptions,
    literalOptions_sourceField,
    literalOptions_facetEnabled,
    literalOptions_searchEnabled,
    literalOptions_sortEnabled,
    literalOptions_defaultValue,
    literalOptions_returnEnabled,

    -- * OptionStatus
    OptionStatus (..),
    newOptionStatus,
    optionStatus_pendingDeletion,
    optionStatus_updateVersion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- * ScalingParameters
    ScalingParameters (..),
    newScalingParameters,
    scalingParameters_desiredPartitionCount,
    scalingParameters_desiredReplicationCount,
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
    textArrayOptions_defaultValue,
    textArrayOptions_returnEnabled,
    textArrayOptions_highlightEnabled,

    -- * TextOptions
    TextOptions (..),
    newTextOptions,
    textOptions_sourceField,
    textOptions_analysisScheme,
    textOptions_sortEnabled,
    textOptions_defaultValue,
    textOptions_returnEnabled,
    textOptions_highlightEnabled,
  )
where

import Amazonka.CloudSearch.Types.AccessPoliciesStatus
import Amazonka.CloudSearch.Types.AlgorithmicStemming
import Amazonka.CloudSearch.Types.AnalysisOptions
import Amazonka.CloudSearch.Types.AnalysisScheme
import Amazonka.CloudSearch.Types.AnalysisSchemeLanguage
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
import Amazonka.CloudSearch.Types.IndexFieldType
import Amazonka.CloudSearch.Types.IntArrayOptions
import Amazonka.CloudSearch.Types.IntOptions
import Amazonka.CloudSearch.Types.LatLonOptions
import Amazonka.CloudSearch.Types.Limits
import Amazonka.CloudSearch.Types.LiteralArrayOptions
import Amazonka.CloudSearch.Types.LiteralOptions
import Amazonka.CloudSearch.Types.OptionState
import Amazonka.CloudSearch.Types.OptionStatus
import Amazonka.CloudSearch.Types.PartitionInstanceType
import Amazonka.CloudSearch.Types.ScalingParameters
import Amazonka.CloudSearch.Types.ScalingParametersStatus
import Amazonka.CloudSearch.Types.ServiceEndpoint
import Amazonka.CloudSearch.Types.Suggester
import Amazonka.CloudSearch.Types.SuggesterFuzzyMatching
import Amazonka.CloudSearch.Types.SuggesterStatus
import Amazonka.CloudSearch.Types.TLSSecurityPolicy
import Amazonka.CloudSearch.Types.TextArrayOptions
import Amazonka.CloudSearch.Types.TextOptions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudSearch",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloudsearch",
      Core.signingName = "cloudsearch",
      Core.version = "2013-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "CloudSearch",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "BandwidthLimitExceeded"
              Prelude.. Core.hasStatus 509
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because it attempted to reference a resource
-- that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because it specified an invalid type
-- definition.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidType"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because a resource limit has already been met.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because it attempted an operation which is not
-- enabled.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledAction"
    Prelude.. Core.hasStatus 409

-- | An internal error occurred while processing the request. If this problem
-- persists, report an issue from the
-- <http://status.aws.amazon.com/ Service Health Dashboard>.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | The request was rejected because it has invalid parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"
