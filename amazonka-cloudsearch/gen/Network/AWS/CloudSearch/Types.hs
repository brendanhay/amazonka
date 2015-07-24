{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types
    (
    -- * Service
      CloudSearch

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
    , aoStemmingDictionary
    , aoSynonyms
    , aoJapaneseTokenizationDictionary

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
    , dsARN
    , dsDocService
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
    , ifDateOptions
    , ifTextArrayOptions
    , ifDoubleArrayOptions
    , ifDoubleOptions
    , ifTextOptions
    , ifLatLonOptions
    , ifIntArrayOptions
    , ifLiteralArrayOptions
    , ifDateArrayOptions
    , ifLiteralOptions
    , ifIntOptions
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

import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.CloudSearch.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-01-01@ of the Amazon CloudSearch SDK.
data CloudSearch

instance AWSService CloudSearch where
    type Sg CloudSearch = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudSearch"
            , _svcPrefix = "cloudsearch"
            , _svcVersion = "2013-01-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcPreflight = id
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "BandwidthLimitExceeded" . hasStatus 509) e =
              Just "request_limit_exceeded"
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | An error occurred while processing the request.
_BaseException :: AWSError a => Getting (First ServiceError) a ServiceError
_BaseException = _ServiceError . hasCode "BaseException"

-- | The request was rejected because it attempted an operation which is not
-- enabled.
_DisabledOperationException :: AWSError a => Getting (First ServiceError) a ServiceError
_DisabledOperationException =
    _ServiceError . hasStatus 409 . hasCode "DisabledAction"

-- | An internal error occurred while processing the request. If this problem
-- persists, report an issue from the
-- <http://status.aws.amazon.com/ Service Health Dashboard>.
_InternalException :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalException =
    _ServiceError . hasStatus 500 . hasCode "InternalException"

-- | The request was rejected because it specified an invalid type
-- definition.
_InvalidTypeException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidTypeException = _ServiceError . hasStatus 409 . hasCode "InvalidType"

-- | The request was rejected because it attempted to reference a resource
-- that does not exist.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 409 . hasCode "ResourceNotFound"

-- | The request was rejected because a resource limit has already been met.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 409 . hasCode "LimitExceeded"
