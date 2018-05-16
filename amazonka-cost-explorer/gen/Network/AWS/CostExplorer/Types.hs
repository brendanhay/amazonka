{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types
    (
    -- * Service Configuration
      costExplorer

    -- * Errors
    , _BillExpirationException
    , _RequestChangedException
    , _InvalidNextTokenException
    , _DataUnavailableException
    , _LimitExceededException

    -- * AccountScope
    , AccountScope (..)

    -- * Context
    , Context (..)

    -- * Dimension
    , Dimension (..)

    -- * Granularity
    , Granularity (..)

    -- * GroupDefinitionType
    , GroupDefinitionType (..)

    -- * LookbackPeriodInDays
    , LookbackPeriodInDays (..)

    -- * OfferingClass
    , OfferingClass (..)

    -- * PaymentOption
    , PaymentOption (..)

    -- * TermInYears
    , TermInYears (..)

    -- * Coverage
    , Coverage
    , coverage
    , cCoverageHours

    -- * CoverageByTime
    , CoverageByTime
    , coverageByTime
    , cbtGroups
    , cbtTimePeriod
    , cbtTotal

    -- * CoverageHours
    , CoverageHours
    , coverageHours
    , chCoverageHoursPercentage
    , chOnDemandHours
    , chTotalRunningHours
    , chReservedHours

    -- * DateInterval
    , DateInterval
    , dateInterval
    , diStart
    , diEnd

    -- * DimensionValues
    , DimensionValues
    , dimensionValues
    , dvValues
    , dvKey

    -- * DimensionValuesWithAttributes
    , DimensionValuesWithAttributes
    , dimensionValuesWithAttributes
    , dvwaValue
    , dvwaAttributes

    -- * EC2InstanceDetails
    , EC2InstanceDetails
    , ec2InstanceDetails
    , eidCurrentGeneration
    , eidPlatform
    , eidFamily
    , eidInstanceType
    , eidAvailabilityZone
    , eidSizeFlexEligible
    , eidTenancy
    , eidRegion

    -- * EC2Specification
    , EC2Specification
    , ec2Specification
    , esOfferingClass

    -- * Expression
    , Expression
    , expression
    , eNot
    , eAnd
    , eOr
    , eDimensions
    , eTags

    -- * Group
    , Group
    , group'
    , gMetrics
    , gKeys

    -- * GroupDefinition
    , GroupDefinition
    , groupDefinition
    , gdKey
    , gdType

    -- * InstanceDetails
    , InstanceDetails
    , instanceDetails
    , idRDSInstanceDetails
    , idEC2InstanceDetails

    -- * MetricValue
    , MetricValue
    , metricValue
    , mvAmount
    , mvUnit

    -- * RDSInstanceDetails
    , RDSInstanceDetails
    , rdsInstanceDetails
    , ridCurrentGeneration
    , ridDeploymentOption
    , ridFamily
    , ridInstanceType
    , ridLicenseModel
    , ridSizeFlexEligible
    , ridRegion
    , ridDatabaseEngine

    -- * ReservationAggregates
    , ReservationAggregates
    , reservationAggregates
    , raPurchasedHours
    , raTotalActualHours
    , raUtilizationPercentage
    , raUnusedHours

    -- * ReservationCoverageGroup
    , ReservationCoverageGroup
    , reservationCoverageGroup
    , rcgCoverage
    , rcgAttributes

    -- * ReservationPurchaseRecommendation
    , ReservationPurchaseRecommendation
    , reservationPurchaseRecommendation
    , rprTermInYears
    , rprRecommendationSummary
    , rprServiceSpecification
    , rprAccountScope
    , rprRecommendationDetails
    , rprLookbackPeriodInDays
    , rprPaymentOption

    -- * ReservationPurchaseRecommendationDetail
    , ReservationPurchaseRecommendationDetail
    , reservationPurchaseRecommendationDetail
    , rprdMaximumNormalizedUnitsUsedPerHour
    , rprdRecurringStandardMonthlyCost
    , rprdAverageNormalizedUnitsUsedPerHour
    , rprdCurrencyCode
    , rprdEstimatedMonthlySavingsPercentage
    , rprdRecommendedNormalizedUnitsToPurchase
    , rprdAverageUtilization
    , rprdEstimatedMonthlySavingsAmount
    , rprdUpfrontCost
    , rprdMinimumNormalizedUnitsUsedPerHour
    , rprdEstimatedMonthlyOnDemandCost
    , rprdRecommendedNumberOfInstancesToPurchase
    , rprdMaximumNumberOfInstancesUsedPerHour
    , rprdEstimatedReservationCostForLookbackPeriod
    , rprdInstanceDetails
    , rprdAverageNumberOfInstancesUsedPerHour
    , rprdMinimumNumberOfInstancesUsedPerHour
    , rprdEstimatedBreakEvenInMonths

    -- * ReservationPurchaseRecommendationMetadata
    , ReservationPurchaseRecommendationMetadata
    , reservationPurchaseRecommendationMetadata
    , rprmRecommendationId
    , rprmGenerationTimestamp

    -- * ReservationPurchaseRecommendationSummary
    , ReservationPurchaseRecommendationSummary
    , reservationPurchaseRecommendationSummary
    , rprsCurrencyCode
    , rprsTotalEstimatedMonthlySavingsPercentage
    , rprsTotalEstimatedMonthlySavingsAmount

    -- * ReservationUtilizationGroup
    , ReservationUtilizationGroup
    , reservationUtilizationGroup
    , rugValue
    , rugKey
    , rugAttributes
    , rugUtilization

    -- * ResultByTime
    , ResultByTime
    , resultByTime
    , rbtGroups
    , rbtTimePeriod
    , rbtTotal
    , rbtEstimated

    -- * ServiceSpecification
    , ServiceSpecification
    , serviceSpecification
    , ssEC2Specification

    -- * TagValues
    , TagValues
    , tagValues
    , tvValues
    , tvKey

    -- * UtilizationByTime
    , UtilizationByTime
    , utilizationByTime
    , ubtGroups
    , ubtTimePeriod
    , ubtTotal
    ) where

import Network.AWS.CostExplorer.Types.Product
import Network.AWS.CostExplorer.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-10-25@ of the Amazon Cost Explorer Service SDK configuration.
costExplorer :: Service
costExplorer =
  Service
    { _svcAbbrev = "CostExplorer"
    , _svcSigner = v4
    , _svcPrefix = "ce"
    , _svcVersion = "2017-10-25"
    , _svcEndpoint = defaultEndpoint costExplorer
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CostExplorer"
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


-- | The requested report expired. Update the date interval and try again.
--
--
_BillExpirationException :: AsError a => Getting (First ServiceError) a ServiceError
_BillExpirationException =
  _MatchServiceError costExplorer "BillExpirationException"


-- | Your request parameters changed between pages. Try again with the old parameters or without a pagination token.
--
--
_RequestChangedException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestChangedException =
  _MatchServiceError costExplorer "RequestChangedException"


-- | The pagination token is invalid. Try again without a pagination token.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError costExplorer "InvalidNextTokenException"


-- | The requested data is unavailable.
--
--
_DataUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_DataUnavailableException =
  _MatchServiceError costExplorer "DataUnavailableException"


-- | You made too many calls in a short period of time. Try again later.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError costExplorer "LimitExceededException"

