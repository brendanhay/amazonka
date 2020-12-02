{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types
  ( -- * Service Configuration
    cloudSearch,

    -- * Errors

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
    AccessPoliciesStatus,
    accessPoliciesStatus,
    apsOptions,
    apsStatus,

    -- * AnalysisOptions
    AnalysisOptions,
    analysisOptions,
    aoAlgorithmicStemming,
    aoStopwords,
    aoJapaneseTokenizationDictionary,
    aoSynonyms,
    aoStemmingDictionary,

    -- * AnalysisScheme
    AnalysisScheme,
    analysisScheme,
    asAnalysisOptions,
    asAnalysisSchemeName,
    asAnalysisSchemeLanguage,

    -- * AnalysisSchemeStatus
    AnalysisSchemeStatus,
    analysisSchemeStatus,
    assOptions,
    assStatus,

    -- * AvailabilityOptionsStatus
    AvailabilityOptionsStatus,
    availabilityOptionsStatus,
    aosOptions,
    aosStatus,

    -- * DateArrayOptions
    DateArrayOptions,
    dateArrayOptions,
    daosSourceFields,
    daosReturnEnabled,
    daosFacetEnabled,
    daosSearchEnabled,
    daosDefaultValue,

    -- * DateOptions
    DateOptions,
    dateOptions,
    doSourceField,
    doReturnEnabled,
    doFacetEnabled,
    doSearchEnabled,
    doSortEnabled,
    doDefaultValue,

    -- * DocumentSuggesterOptions
    DocumentSuggesterOptions,
    documentSuggesterOptions,
    dsoSortExpression,
    dsoFuzzyMatching,
    dsoSourceField,

    -- * DomainEndpointOptions
    DomainEndpointOptions,
    domainEndpointOptions,
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus,
    domainEndpointOptionsStatus,
    deosOptions,
    deosStatus,

    -- * DomainStatus
    DomainStatus,
    domainStatus,
    dsSearchInstanceCount,
    dsSearchInstanceType,
    dsDocService,
    dsARN,
    dsCreated,
    dsSearchService,
    dsLimits,
    dsSearchPartitionCount,
    dsDeleted,
    dsProcessing,
    dsDomainId,
    dsDomainName,
    dsRequiresIndexDocuments,

    -- * DoubleArrayOptions
    DoubleArrayOptions,
    doubleArrayOptions,
    daoSourceFields,
    daoReturnEnabled,
    daoFacetEnabled,
    daoSearchEnabled,
    daoDefaultValue,

    -- * DoubleOptions
    DoubleOptions,
    doubleOptions,
    dSourceField,
    dReturnEnabled,
    dFacetEnabled,
    dSearchEnabled,
    dSortEnabled,
    dDefaultValue,

    -- * Expression
    Expression,
    expression,
    eExpressionName,
    eExpressionValue,

    -- * ExpressionStatus
    ExpressionStatus,
    expressionStatus,
    esOptions,
    esStatus,

    -- * IndexField
    IndexField,
    indexField,
    ifDoubleArrayOptions,
    ifDateOptions,
    ifTextArrayOptions,
    ifDoubleOptions,
    ifTextOptions,
    ifLatLonOptions,
    ifLiteralArrayOptions,
    ifIntArrayOptions,
    ifDateArrayOptions,
    ifIntOptions,
    ifLiteralOptions,
    ifIndexFieldName,
    ifIndexFieldType,

    -- * IndexFieldStatus
    IndexFieldStatus,
    indexFieldStatus,
    ifsOptions,
    ifsStatus,

    -- * IntArrayOptions
    IntArrayOptions,
    intArrayOptions,
    iaoSourceFields,
    iaoReturnEnabled,
    iaoFacetEnabled,
    iaoSearchEnabled,
    iaoDefaultValue,

    -- * IntOptions
    IntOptions,
    intOptions,
    ioSourceField,
    ioReturnEnabled,
    ioFacetEnabled,
    ioSearchEnabled,
    ioSortEnabled,
    ioDefaultValue,

    -- * LatLonOptions
    LatLonOptions,
    latLonOptions,
    lloSourceField,
    lloReturnEnabled,
    lloFacetEnabled,
    lloSearchEnabled,
    lloSortEnabled,
    lloDefaultValue,

    -- * Limits
    Limits,
    limits,
    lMaximumReplicationCount,
    lMaximumPartitionCount,

    -- * LiteralArrayOptions
    LiteralArrayOptions,
    literalArrayOptions,
    laoSourceFields,
    laoReturnEnabled,
    laoFacetEnabled,
    laoSearchEnabled,
    laoDefaultValue,

    -- * LiteralOptions
    LiteralOptions,
    literalOptions,
    loSourceField,
    loReturnEnabled,
    loFacetEnabled,
    loSearchEnabled,
    loSortEnabled,
    loDefaultValue,

    -- * OptionStatus
    OptionStatus,
    optionStatus,
    osPendingDeletion,
    osUpdateVersion,
    osCreationDate,
    osUpdateDate,
    osState,

    -- * ScalingParameters
    ScalingParameters,
    scalingParameters,
    spDesiredInstanceType,
    spDesiredReplicationCount,
    spDesiredPartitionCount,

    -- * ScalingParametersStatus
    ScalingParametersStatus,
    scalingParametersStatus,
    spsOptions,
    spsStatus,

    -- * ServiceEndpoint
    ServiceEndpoint,
    serviceEndpoint,
    seEndpoint,

    -- * Suggester
    Suggester,
    suggester,
    sSuggesterName,
    sDocumentSuggesterOptions,

    -- * SuggesterStatus
    SuggesterStatus,
    suggesterStatus,
    ssOptions,
    ssStatus,

    -- * TextArrayOptions
    TextArrayOptions,
    textArrayOptions,
    taoSourceFields,
    taoReturnEnabled,
    taoAnalysisScheme,
    taoHighlightEnabled,
    taoDefaultValue,

    -- * TextOptions
    TextOptions,
    textOptions,
    toSourceField,
    toReturnEnabled,
    toAnalysisScheme,
    toHighlightEnabled,
    toSortEnabled,
    toDefaultValue,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-01-01@ of the Amazon CloudSearch SDK configuration.
cloudSearch :: Service
cloudSearch =
  Service
    { _svcAbbrev = "CloudSearch",
      _svcSigner = v4,
      _svcPrefix = "cloudsearch",
      _svcVersion = "2013-01-01",
      _svcEndpoint = defaultEndpoint cloudSearch,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "CloudSearch",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
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
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
