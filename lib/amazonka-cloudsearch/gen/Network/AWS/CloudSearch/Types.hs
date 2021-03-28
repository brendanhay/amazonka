-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ValidationException
    , _BaseException
    , _DisabledOperationException
    , _InternalException
    , _InvalidTypeException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * DomainStatus
    , DomainStatus (..)
    , mkDomainStatus
    , dsDomainId
    , dsDomainName
    , dsRequiresIndexDocuments
    , dsARN
    , dsCreated
    , dsDeleted
    , dsDocService
    , dsLimits
    , dsProcessing
    , dsSearchInstanceCount
    , dsSearchInstanceType
    , dsSearchPartitionCount
    , dsSearchService

    -- * DocumentSuggesterOptions
    , DocumentSuggesterOptions (..)
    , mkDocumentSuggesterOptions
    , dsoSourceField
    , dsoFuzzyMatching
    , dsoSortExpression

    -- * DoubleArrayOptions
    , DoubleArrayOptions (..)
    , mkDoubleArrayOptions
    , daoDefaultValue
    , daoFacetEnabled
    , daoReturnEnabled
    , daoSearchEnabled
    , daoSourceFields

    -- * IndexField
    , IndexField (..)
    , mkIndexField
    , ifIndexFieldName
    , ifIndexFieldType
    , ifDateArrayOptions
    , ifDateOptions
    , ifDoubleArrayOptions
    , ifDoubleOptions
    , ifIntArrayOptions
    , ifIntOptions
    , ifLatLonOptions
    , ifLiteralArrayOptions
    , ifLiteralOptions
    , ifTextArrayOptions
    , ifTextOptions

    -- * DateOptions
    , DateOptions (..)
    , mkDateOptions
    , doDefaultValue
    , doFacetEnabled
    , doReturnEnabled
    , doSearchEnabled
    , doSortEnabled
    , doSourceField

    -- * OptionState
    , OptionState (..)

    -- * TextArrayOptions
    , TextArrayOptions (..)
    , mkTextArrayOptions
    , taoAnalysisScheme
    , taoDefaultValue
    , taoHighlightEnabled
    , taoReturnEnabled
    , taoSourceFields

    -- * SearchInstanceType
    , SearchInstanceType (..)

    -- * PolicyDocument
    , PolicyDocument (..)

    -- * AlgorithmicStemming
    , AlgorithmicStemming (..)

    -- * AnalysisScheme
    , AnalysisScheme (..)
    , mkAnalysisScheme
    , asAnalysisSchemeName
    , asAnalysisSchemeLanguage
    , asAnalysisOptions

    -- * ScalingParameters
    , ScalingParameters (..)
    , mkScalingParameters
    , spDesiredInstanceType
    , spDesiredPartitionCount
    , spDesiredReplicationCount

    -- * FieldNameCommaList
    , FieldNameCommaList (..)

    -- * APIVersion
    , APIVersion (..)

    -- * AnalysisOptions
    , AnalysisOptions (..)
    , mkAnalysisOptions
    , aoAlgorithmicStemming
    , aoJapaneseTokenizationDictionary
    , aoStemmingDictionary
    , aoStopwords
    , aoSynonyms

    -- * DoubleOptions
    , DoubleOptions (..)
    , mkDoubleOptions
    , dosDefaultValue
    , dosFacetEnabled
    , dosReturnEnabled
    , dosSearchEnabled
    , dosSortEnabled
    , dosSourceField

    -- * ARN
    , ARN (..)

    -- * TextOptions
    , TextOptions (..)
    , mkTextOptions
    , toAnalysisScheme
    , toDefaultValue
    , toHighlightEnabled
    , toReturnEnabled
    , toSortEnabled
    , toSourceField

    -- * AvailabilityOptionsStatus
    , AvailabilityOptionsStatus (..)
    , mkAvailabilityOptionsStatus
    , aosOptions
    , aosStatus

    -- * DynamicFieldName
    , DynamicFieldName (..)

    -- * IndexFieldStatus
    , IndexFieldStatus (..)
    , mkIndexFieldStatus
    , ifsOptions
    , ifsStatus

    -- * ScalingParametersStatus
    , ScalingParametersStatus (..)
    , mkScalingParametersStatus
    , spsOptions
    , spsStatus

    -- * AnalysisSchemeStatus
    , AnalysisSchemeStatus (..)
    , mkAnalysisSchemeStatus
    , assOptions
    , assStatus

    -- * ServiceEndpoint
    , ServiceEndpoint (..)
    , mkServiceEndpoint
    , seEndpoint

    -- * Limits
    , Limits (..)
    , mkLimits
    , lMaximumReplicationCount
    , lMaximumPartitionCount

    -- * ExpressionStatus
    , ExpressionStatus (..)
    , mkExpressionStatus
    , esOptions
    , esStatus

    -- * FieldValue
    , FieldValue (..)

    -- * IndexFieldType
    , IndexFieldType (..)

    -- * StandardName
    , StandardName (..)

    -- * LatLonOptions
    , LatLonOptions (..)
    , mkLatLonOptions
    , lloDefaultValue
    , lloFacetEnabled
    , lloReturnEnabled
    , lloSearchEnabled
    , lloSortEnabled
    , lloSourceField

    -- * SuggesterStatus
    , SuggesterStatus (..)
    , mkSuggesterStatus
    , ssOptions
    , ssStatus

    -- * DomainName
    , DomainName (..)

    -- * OptionStatus
    , OptionStatus (..)
    , mkOptionStatus
    , osCreationDate
    , osUpdateDate
    , osState
    , osPendingDeletion
    , osUpdateVersion

    -- * DomainEndpointOptionsStatus
    , DomainEndpointOptionsStatus (..)
    , mkDomainEndpointOptionsStatus
    , deosOptions
    , deosStatus

    -- * LiteralArrayOptions
    , LiteralArrayOptions (..)
    , mkLiteralArrayOptions
    , laoDefaultValue
    , laoFacetEnabled
    , laoReturnEnabled
    , laoSearchEnabled
    , laoSourceFields

    -- * IntArrayOptions
    , IntArrayOptions (..)
    , mkIntArrayOptions
    , iaoDefaultValue
    , iaoFacetEnabled
    , iaoReturnEnabled
    , iaoSearchEnabled
    , iaoSourceFields

    -- * Expression
    , Expression (..)
    , mkExpression
    , eExpressionName
    , eExpressionValue

    -- * SuggesterFuzzyMatching
    , SuggesterFuzzyMatching (..)

    -- * FieldName
    , FieldName (..)

    -- * TLSSecurityPolicy
    , TLSSecurityPolicy (..)

    -- * DateArrayOptions
    , DateArrayOptions (..)
    , mkDateArrayOptions
    , dDefaultValue
    , dFacetEnabled
    , dReturnEnabled
    , dSearchEnabled
    , dSourceFields

    -- * DomainId
    , DomainId (..)

    -- * AnalysisSchemeLanguage
    , AnalysisSchemeLanguage (..)

    -- * PartitionInstanceType
    , PartitionInstanceType (..)

    -- * Suggester
    , Suggester (..)
    , mkSuggester
    , sSuggesterName
    , sDocumentSuggesterOptions

    -- * IntOptions
    , IntOptions (..)
    , mkIntOptions
    , ioDefaultValue
    , ioFacetEnabled
    , ioReturnEnabled
    , ioSearchEnabled
    , ioSortEnabled
    , ioSourceField

    -- * LiteralOptions
    , LiteralOptions (..)
    , mkLiteralOptions
    , loDefaultValue
    , loFacetEnabled
    , loReturnEnabled
    , loSearchEnabled
    , loSortEnabled
    , loSourceField

    -- * DomainEndpointOptions
    , DomainEndpointOptions (..)
    , mkDomainEndpointOptions
    , deoEnforceHTTPS
    , deoTLSSecurityPolicy

    -- * Word
    , Word (..)

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus (..)
    , mkAccessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * ExpressionValue
    , ExpressionValue (..)

    -- * SourceField
    , SourceField (..)

    -- * SourceFields
    , SourceFields (..)

    -- * IndexFieldName
    , IndexFieldName (..)

    -- * DefaultValue
    , DefaultValue (..)

    -- * AnalysisSchemeName
    , AnalysisSchemeName (..)

    -- * ExpressionName
    , ExpressionName (..)

    -- * SuggesterName
    , SuggesterName (..)

    -- * Endpoint
    , Endpoint (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CloudSearch.Types.DomainStatus
  
import Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
  
import Network.AWS.CloudSearch.Types.DoubleArrayOptions
  
import Network.AWS.CloudSearch.Types.IndexField
  
  
import Network.AWS.CloudSearch.Types.DateOptions
  
import Network.AWS.CloudSearch.Types.OptionState
  
import Network.AWS.CloudSearch.Types.TextArrayOptions
  
import Network.AWS.CloudSearch.Types.SearchInstanceType
  
import Network.AWS.CloudSearch.Types.PolicyDocument
  
import Network.AWS.CloudSearch.Types.AlgorithmicStemming
  
import Network.AWS.CloudSearch.Types.AnalysisScheme
  
import Network.AWS.CloudSearch.Types.ScalingParameters
  
import Network.AWS.CloudSearch.Types.FieldNameCommaList
  
import Network.AWS.CloudSearch.Types.APIVersion
  
import Network.AWS.CloudSearch.Types.AnalysisOptions
  
import Network.AWS.CloudSearch.Types.DoubleOptions
  
import Network.AWS.CloudSearch.Types.ARN
  
import Network.AWS.CloudSearch.Types.TextOptions
  
import Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
  
import Network.AWS.CloudSearch.Types.DynamicFieldName
  
import Network.AWS.CloudSearch.Types.IndexFieldStatus
  
import Network.AWS.CloudSearch.Types.ScalingParametersStatus
  
import Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
  
import Network.AWS.CloudSearch.Types.ServiceEndpoint
  
import Network.AWS.CloudSearch.Types.Limits
  
import Network.AWS.CloudSearch.Types.ExpressionStatus
  
import Network.AWS.CloudSearch.Types.FieldValue
  
import Network.AWS.CloudSearch.Types.IndexFieldType
  
import Network.AWS.CloudSearch.Types.StandardName
  
import Network.AWS.CloudSearch.Types.LatLonOptions
  
import Network.AWS.CloudSearch.Types.SuggesterStatus
  
import Network.AWS.CloudSearch.Types.DomainName
  
import Network.AWS.CloudSearch.Types.OptionStatus
  
import Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
  
import Network.AWS.CloudSearch.Types.LiteralArrayOptions
  
  
import Network.AWS.CloudSearch.Types.IntArrayOptions
  
import Network.AWS.CloudSearch.Types.Expression
  
import Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
  
  
import Network.AWS.CloudSearch.Types.FieldName
  
import Network.AWS.CloudSearch.Types.TLSSecurityPolicy
  
import Network.AWS.CloudSearch.Types.DateArrayOptions
  
import Network.AWS.CloudSearch.Types.DomainId
  
import Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
  
  
import Network.AWS.CloudSearch.Types.PartitionInstanceType
  
import Network.AWS.CloudSearch.Types.Suggester
  
import Network.AWS.CloudSearch.Types.IntOptions
  
import Network.AWS.CloudSearch.Types.LiteralOptions
  
import Network.AWS.CloudSearch.Types.DomainEndpointOptions
  
import Network.AWS.CloudSearch.Types.Word
  
  
import Network.AWS.CloudSearch.Types.AccessPoliciesStatus
  
  
import Network.AWS.CloudSearch.Types.ExpressionValue
  
  
import Network.AWS.CloudSearch.Types.SourceField
  
import Network.AWS.CloudSearch.Types.SourceFields
  
import Network.AWS.CloudSearch.Types.IndexFieldName
  
import Network.AWS.CloudSearch.Types.DefaultValue
  
import Network.AWS.CloudSearch.Types.AnalysisSchemeName
  
import Network.AWS.CloudSearch.Types.ExpressionName
  
import Network.AWS.CloudSearch.Types.SuggesterName
  
import Network.AWS.CloudSearch.Types.Endpoint
  

-- | API version @2013-01-01@ of the Amazon CloudSearch SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CloudSearch",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "cloudsearch",
                 Core._svcVersion = "2013-01-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "CloudSearch",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Lens.has
              (Core.hasCode "BandwidthLimitExceeded" Core.. Core.hasStatus 509)
              e
            = Core.Just "request_limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The request was rejected because it has invalid parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BaseException
  = Core._MatchServiceError mkServiceConfig "BaseException"
{-# INLINEABLE _BaseException #-}
{-# DEPRECATED _BaseException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it attempted an operation which is not enabled.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException
  = Core._MatchServiceError mkServiceConfig "DisabledAction" Core..
      Core.hasStatues 409
{-# INLINEABLE _DisabledOperationException #-}
{-# DEPRECATED _DisabledOperationException "Use generic-lens or generic-optics instead"  #-}

-- | An internal error occurred while processing the request. If this problem persists, report an issue from the <http://status.aws.amazon.com/ Service Health Dashboard> .
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException
  = Core._MatchServiceError mkServiceConfig "InternalException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalException #-}
{-# DEPRECATED _InternalException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it specified an invalid type definition.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException
  = Core._MatchServiceError mkServiceConfig "InvalidType" Core..
      Core.hasStatues 409
{-# INLINEABLE _InvalidTypeException #-}
{-# DEPRECATED _InvalidTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because it attempted to reference a resource that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig "ResourceNotFound" Core..
      Core.hasStatues 409
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because a resource limit has already been met.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceeded" Core..
      Core.hasStatues 409
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
