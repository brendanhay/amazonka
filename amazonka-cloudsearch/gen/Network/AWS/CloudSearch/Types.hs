{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types
    (
    -- * Service Configuration
      cloudSearch

    -- * Errors
    , _BaseException
    , _DisabledOperationException
    , _InternalException
    , _InvalidTypeException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * AlgorithmicStemming
    , AlgorithmicStemming (..)

    -- * AnalysisSchemeLanguage
    , AnalysisSchemeLanguage (..)

    -- * IndexFieldType
    , IndexFieldType (..)

    -- * OptionState
    , OptionState (..)

    -- * PartitionInstanceType
    , PartitionInstanceType (..)

    -- * SuggesterFuzzyMatching
    , SuggesterFuzzyMatching (..)

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * AnalysisOptions
    , AnalysisOptions
    , analysisOptions
    , aoAlgorithmicStemming
    , aoStopwords
    , aoJapaneseTokenizationDictionary
    , aoSynonyms
    , aoStemmingDictionary

    -- * AnalysisScheme
    , AnalysisScheme
    , analysisScheme
    , asAnalysisOptions
    , asAnalysisSchemeName
    , asAnalysisSchemeLanguage

    -- * AnalysisSchemeStatus
    , AnalysisSchemeStatus
    , analysisSchemeStatus
    , assOptions
    , assStatus

    -- * AvailabilityOptionsStatus
    , AvailabilityOptionsStatus
    , availabilityOptionsStatus
    , aosOptions
    , aosStatus

    -- * DateArrayOptions
    , DateArrayOptions
    , dateArrayOptions
    , daosSourceFields
    , daosReturnEnabled
    , daosFacetEnabled
    , daosSearchEnabled
    , daosDefaultValue

    -- * DateOptions
    , DateOptions
    , dateOptions
    , doSourceField
    , doReturnEnabled
    , doFacetEnabled
    , doSearchEnabled
    , doSortEnabled
    , doDefaultValue

    -- * DocumentSuggesterOptions
    , DocumentSuggesterOptions
    , documentSuggesterOptions
    , dsoSortExpression
    , dsoFuzzyMatching
    , dsoSourceField

    -- * DomainStatus
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

    -- * DoubleArrayOptions
    , DoubleArrayOptions
    , doubleArrayOptions
    , daoSourceFields
    , daoReturnEnabled
    , daoFacetEnabled
    , daoSearchEnabled
    , daoDefaultValue

    -- * DoubleOptions
    , DoubleOptions
    , doubleOptions
    , dSourceField
    , dReturnEnabled
    , dFacetEnabled
    , dSearchEnabled
    , dSortEnabled
    , dDefaultValue

    -- * Expression
    , Expression
    , expression
    , eExpressionName
    , eExpressionValue

    -- * ExpressionStatus
    , ExpressionStatus
    , expressionStatus
    , esOptions
    , esStatus

    -- * IndexField
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

    -- * IndexFieldStatus
    , IndexFieldStatus
    , indexFieldStatus
    , ifsOptions
    , ifsStatus

    -- * IntArrayOptions
    , IntArrayOptions
    , intArrayOptions
    , iaoSourceFields
    , iaoReturnEnabled
    , iaoFacetEnabled
    , iaoSearchEnabled
    , iaoDefaultValue

    -- * IntOptions
    , IntOptions
    , intOptions
    , ioSourceField
    , ioReturnEnabled
    , ioFacetEnabled
    , ioSearchEnabled
    , ioSortEnabled
    , ioDefaultValue

    -- * LatLonOptions
    , LatLonOptions
    , latLonOptions
    , lloSourceField
    , lloReturnEnabled
    , lloFacetEnabled
    , lloSearchEnabled
    , lloSortEnabled
    , lloDefaultValue

    -- * Limits
    , Limits
    , limits
    , lMaximumReplicationCount
    , lMaximumPartitionCount

    -- * LiteralArrayOptions
    , LiteralArrayOptions
    , literalArrayOptions
    , laoSourceFields
    , laoReturnEnabled
    , laoFacetEnabled
    , laoSearchEnabled
    , laoDefaultValue

    -- * LiteralOptions
    , LiteralOptions
    , literalOptions
    , loSourceField
    , loReturnEnabled
    , loFacetEnabled
    , loSearchEnabled
    , loSortEnabled
    , loDefaultValue

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- * ScalingParameters
    , ScalingParameters
    , scalingParameters
    , spDesiredInstanceType
    , spDesiredReplicationCount
    , spDesiredPartitionCount

    -- * ScalingParametersStatus
    , ScalingParametersStatus
    , scalingParametersStatus
    , spsOptions
    , spsStatus

    -- * ServiceEndpoint
    , ServiceEndpoint
    , serviceEndpoint
    , seEndpoint

    -- * Suggester
    , Suggester
    , suggester
    , sSuggesterName
    , sDocumentSuggesterOptions

    -- * SuggesterStatus
    , SuggesterStatus
    , suggesterStatus
    , ssOptions
    , ssStatus

    -- * TextArrayOptions
    , TextArrayOptions
    , textArrayOptions
    , taoSourceFields
    , taoReturnEnabled
    , taoAnalysisScheme
    , taoHighlightEnabled
    , taoDefaultValue

    -- * TextOptions
    , TextOptions
    , textOptions
    , toSourceField
    , toReturnEnabled
    , toAnalysisScheme
    , toHighlightEnabled
    , toSortEnabled
    , toDefaultValue
    ) where

import Network.AWS.CloudSearch.Types.Product
import Network.AWS.CloudSearch.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-01-01@ of the Amazon CloudSearch SDK configuration.
cloudSearch :: Service
cloudSearch =
  Service
    { _svcAbbrev = "CloudSearch"
    , _svcSigner = v4
    , _svcPrefix = "cloudsearch"
    , _svcVersion = "2013-01-01"
    , _svcEndpoint = defaultEndpoint cloudSearch
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "CloudSearch"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasCode "BandwidthLimitExceeded" . hasStatus 509) e =
        Just "request_limit_exceeded"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | An error occurred while processing the request.
--
--
_BaseException :: AsError a => Getting (First ServiceError) a ServiceError
_BaseException = _MatchServiceError cloudSearch "BaseException"


-- | The request was rejected because it attempted an operation which is not enabled.
--
--
_DisabledOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_DisabledOperationException =
  _MatchServiceError cloudSearch "DisabledAction" . hasStatus 409


-- | An internal error occurred while processing the request. If this problem persists, report an issue from the <http://status.aws.amazon.com/ Service Health Dashboard> .
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException =
  _MatchServiceError cloudSearch "InternalException" . hasStatus 500


-- | The request was rejected because it specified an invalid type definition.
--
--
_InvalidTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTypeException =
  _MatchServiceError cloudSearch "InvalidType" . hasStatus 409


-- | The request was rejected because it attempted to reference a resource that does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError cloudSearch "ResourceNotFound" . hasStatus 409


-- | The request was rejected because a resource limit has already been met.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError cloudSearch "LimitExceeded" . hasStatus 409

