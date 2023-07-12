{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearch.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BaseException,
    _DisabledOperationException,
    _InternalException,
    _InvalidTypeException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ValidationException,

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
    analysisOptions_algorithmicStemming,
    analysisOptions_japaneseTokenizationDictionary,
    analysisOptions_stemmingDictionary,
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
    dateArrayOptions_defaultValue,
    dateArrayOptions_facetEnabled,
    dateArrayOptions_returnEnabled,
    dateArrayOptions_searchEnabled,
    dateArrayOptions_sourceFields,

    -- * DateOptions
    DateOptions (..),
    newDateOptions,
    dateOptions_defaultValue,
    dateOptions_facetEnabled,
    dateOptions_returnEnabled,
    dateOptions_searchEnabled,
    dateOptions_sortEnabled,
    dateOptions_sourceField,

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
    domainStatus_arn,
    domainStatus_created,
    domainStatus_deleted,
    domainStatus_docService,
    domainStatus_limits,
    domainStatus_processing,
    domainStatus_searchInstanceCount,
    domainStatus_searchInstanceType,
    domainStatus_searchPartitionCount,
    domainStatus_searchService,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_requiresIndexDocuments,

    -- * DoubleArrayOptions
    DoubleArrayOptions (..),
    newDoubleArrayOptions,
    doubleArrayOptions_defaultValue,
    doubleArrayOptions_facetEnabled,
    doubleArrayOptions_returnEnabled,
    doubleArrayOptions_searchEnabled,
    doubleArrayOptions_sourceFields,

    -- * DoubleOptions
    DoubleOptions (..),
    newDoubleOptions,
    doubleOptions_defaultValue,
    doubleOptions_facetEnabled,
    doubleOptions_returnEnabled,
    doubleOptions_searchEnabled,
    doubleOptions_sortEnabled,
    doubleOptions_sourceField,

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
    indexField_dateArrayOptions,
    indexField_dateOptions,
    indexField_doubleArrayOptions,
    indexField_doubleOptions,
    indexField_intArrayOptions,
    indexField_intOptions,
    indexField_latLonOptions,
    indexField_literalArrayOptions,
    indexField_literalOptions,
    indexField_textArrayOptions,
    indexField_textOptions,
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
    intArrayOptions_defaultValue,
    intArrayOptions_facetEnabled,
    intArrayOptions_returnEnabled,
    intArrayOptions_searchEnabled,
    intArrayOptions_sourceFields,

    -- * IntOptions
    IntOptions (..),
    newIntOptions,
    intOptions_defaultValue,
    intOptions_facetEnabled,
    intOptions_returnEnabled,
    intOptions_searchEnabled,
    intOptions_sortEnabled,
    intOptions_sourceField,

    -- * LatLonOptions
    LatLonOptions (..),
    newLatLonOptions,
    latLonOptions_defaultValue,
    latLonOptions_facetEnabled,
    latLonOptions_returnEnabled,
    latLonOptions_searchEnabled,
    latLonOptions_sortEnabled,
    latLonOptions_sourceField,

    -- * Limits
    Limits (..),
    newLimits,
    limits_maximumReplicationCount,
    limits_maximumPartitionCount,

    -- * LiteralArrayOptions
    LiteralArrayOptions (..),
    newLiteralArrayOptions,
    literalArrayOptions_defaultValue,
    literalArrayOptions_facetEnabled,
    literalArrayOptions_returnEnabled,
    literalArrayOptions_searchEnabled,
    literalArrayOptions_sourceFields,

    -- * LiteralOptions
    LiteralOptions (..),
    newLiteralOptions,
    literalOptions_defaultValue,
    literalOptions_facetEnabled,
    literalOptions_returnEnabled,
    literalOptions_searchEnabled,
    literalOptions_sortEnabled,
    literalOptions_sourceField,

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
    scalingParameters_desiredInstanceType,
    scalingParameters_desiredPartitionCount,
    scalingParameters_desiredReplicationCount,

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
    textArrayOptions_defaultValue,
    textArrayOptions_highlightEnabled,
    textArrayOptions_returnEnabled,
    textArrayOptions_sourceFields,

    -- * TextOptions
    TextOptions (..),
    newTextOptions,
    textOptions_analysisScheme,
    textOptions_defaultValue,
    textOptions_highlightEnabled,
    textOptions_returnEnabled,
    textOptions_sortEnabled,
    textOptions_sourceField,
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "BandwidthLimitExceeded"
              Prelude.. Core.hasStatus 509
          )
          e =
          Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An error occurred while processing the request.
_BaseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

-- | The request was rejected because it attempted an operation which is not
-- enabled.
_DisabledOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledAction"
    Prelude.. Core.hasStatus 409

-- | An internal error occurred while processing the request. If this problem
-- persists, report an issue from the
-- <http://status.aws.amazon.com/ Service Health Dashboard>.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | The request was rejected because it specified an invalid type
-- definition.
_InvalidTypeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidType"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because a resource limit has already been met.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because it attempted to reference a resource
-- that does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 409

-- | The request was rejected because it has invalid parameters.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
