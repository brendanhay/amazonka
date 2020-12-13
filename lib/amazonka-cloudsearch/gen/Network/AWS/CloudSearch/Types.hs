-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types
  ( -- * Service configuration
    cloudSearchService,

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
    AccessPoliciesStatus (..),
    mkAccessPoliciesStatus,
    apsStatus,
    apsOptions,

    -- * AnalysisOptions
    AnalysisOptions (..),
    mkAnalysisOptions,
    aoAlgorithmicStemming,
    aoStopwords,
    aoJapaneseTokenizationDictionary,
    aoSynonyms,
    aoStemmingDictionary,

    -- * AnalysisScheme
    AnalysisScheme (..),
    mkAnalysisScheme,
    asAnalysisOptions,
    asAnalysisSchemeLanguage,
    asAnalysisSchemeName,

    -- * AnalysisSchemeStatus
    AnalysisSchemeStatus (..),
    mkAnalysisSchemeStatus,
    assStatus,
    assOptions,

    -- * AvailabilityOptionsStatus
    AvailabilityOptionsStatus (..),
    mkAvailabilityOptionsStatus,
    aosStatus,
    aosOptions,

    -- * DateArrayOptions
    DateArrayOptions (..),
    mkDateArrayOptions,
    daosSourceFields,
    daosReturnEnabled,
    daosFacetEnabled,
    daosSearchEnabled,
    daosDefaultValue,

    -- * DateOptions
    DateOptions (..),
    mkDateOptions,
    doSourceField,
    doReturnEnabled,
    doFacetEnabled,
    doSearchEnabled,
    doSortEnabled,
    doDefaultValue,

    -- * DocumentSuggesterOptions
    DocumentSuggesterOptions (..),
    mkDocumentSuggesterOptions,
    dsoSourceField,
    dsoSortExpression,
    dsoFuzzyMatching,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    mkDomainEndpointOptions,
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    mkDomainEndpointOptionsStatus,
    deosStatus,
    deosOptions,

    -- * DomainStatus
    DomainStatus (..),
    mkDomainStatus,
    dsSearchInstanceCount,
    dsSearchInstanceType,
    dsDocService,
    dsARN,
    dsCreated,
    dsSearchService,
    dsLimits,
    dsRequiresIndexDocuments,
    dsDomainName,
    dsSearchPartitionCount,
    dsDeleted,
    dsDomainId,
    dsProcessing,

    -- * DoubleArrayOptions
    DoubleArrayOptions (..),
    mkDoubleArrayOptions,
    daoSourceFields,
    daoReturnEnabled,
    daoFacetEnabled,
    daoSearchEnabled,
    daoDefaultValue,

    -- * DoubleOptions
    DoubleOptions (..),
    mkDoubleOptions,
    dSourceField,
    dReturnEnabled,
    dFacetEnabled,
    dSearchEnabled,
    dSortEnabled,
    dDefaultValue,

    -- * Expression
    Expression (..),
    mkExpression,
    eExpressionName,
    eExpressionValue,

    -- * ExpressionStatus
    ExpressionStatus (..),
    mkExpressionStatus,
    esStatus,
    esOptions,

    -- * IndexField
    IndexField (..),
    mkIndexField,
    ifDoubleArrayOptions,
    ifDateOptions,
    ifTextArrayOptions,
    ifDoubleOptions,
    ifTextOptions,
    ifIndexFieldType,
    ifLatLonOptions,
    ifLiteralArrayOptions,
    ifIntArrayOptions,
    ifDateArrayOptions,
    ifIntOptions,
    ifLiteralOptions,
    ifIndexFieldName,

    -- * IndexFieldStatus
    IndexFieldStatus (..),
    mkIndexFieldStatus,
    ifsStatus,
    ifsOptions,

    -- * IntArrayOptions
    IntArrayOptions (..),
    mkIntArrayOptions,
    iaoSourceFields,
    iaoReturnEnabled,
    iaoFacetEnabled,
    iaoSearchEnabled,
    iaoDefaultValue,

    -- * IntOptions
    IntOptions (..),
    mkIntOptions,
    ioSourceField,
    ioReturnEnabled,
    ioFacetEnabled,
    ioSearchEnabled,
    ioSortEnabled,
    ioDefaultValue,

    -- * LatLonOptions
    LatLonOptions (..),
    mkLatLonOptions,
    lloSourceField,
    lloReturnEnabled,
    lloFacetEnabled,
    lloSearchEnabled,
    lloSortEnabled,
    lloDefaultValue,

    -- * Limits
    Limits (..),
    mkLimits,
    lMaximumReplicationCount,
    lMaximumPartitionCount,

    -- * LiteralArrayOptions
    LiteralArrayOptions (..),
    mkLiteralArrayOptions,
    laoSourceFields,
    laoReturnEnabled,
    laoFacetEnabled,
    laoSearchEnabled,
    laoDefaultValue,

    -- * LiteralOptions
    LiteralOptions (..),
    mkLiteralOptions,
    loSourceField,
    loReturnEnabled,
    loFacetEnabled,
    loSearchEnabled,
    loSortEnabled,
    loDefaultValue,

    -- * OptionStatus
    OptionStatus (..),
    mkOptionStatus,
    osState,
    osUpdateDate,
    osPendingDeletion,
    osCreationDate,
    osUpdateVersion,

    -- * ScalingParameters
    ScalingParameters (..),
    mkScalingParameters,
    spDesiredInstanceType,
    spDesiredReplicationCount,
    spDesiredPartitionCount,

    -- * ScalingParametersStatus
    ScalingParametersStatus (..),
    mkScalingParametersStatus,
    spsStatus,
    spsOptions,

    -- * ServiceEndpoint
    ServiceEndpoint (..),
    mkServiceEndpoint,
    seEndpoint,

    -- * Suggester
    Suggester (..),
    mkSuggester,
    sDocumentSuggesterOptions,
    sSuggesterName,

    -- * SuggesterStatus
    SuggesterStatus (..),
    mkSuggesterStatus,
    ssStatus,
    ssOptions,

    -- * TextArrayOptions
    TextArrayOptions (..),
    mkTextArrayOptions,
    taoSourceFields,
    taoReturnEnabled,
    taoAnalysisScheme,
    taoHighlightEnabled,
    taoDefaultValue,

    -- * TextOptions
    TextOptions (..),
    mkTextOptions,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch SDK configuration.
cloudSearchService :: Lude.Service
cloudSearchService =
  Lude.Service
    { Lude._svcAbbrev = "CloudSearch",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloudsearch",
      Lude._svcVersion = "2013-01-01",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudSearchService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "CloudSearch",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lens.has
          (Lude.hasCode "BandwidthLimitExceeded" Lude.. Lude.hasStatus 509)
          e =
        Lude.Just "request_limit_exceeded"
      | Lude.otherwise = Lude.Nothing
