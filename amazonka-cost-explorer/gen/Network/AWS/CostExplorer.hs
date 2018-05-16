{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Cost Explorer API allows you to programmatically query your cost and usage data. You can query for aggregated data such as total monthly costs or total daily usage. You can also query for granular data, such as the number of daily write operations for Amazon DynamoDB database tables in your production environment.
--
--
-- Service Endpoint
--
-- The Cost Explorer API provides the following endpoint:
--
--     * https://ce.us-east-1.amazonaws.com
--
--
--
-- For information about costs associated with the Cost Explorer API, see <https://aws.amazon.com/aws-cost-management/pricing/ AWS Cost Management Pricing> .
--
module Network.AWS.CostExplorer
    (
    -- * Service Configuration
      costExplorer

    -- * Errors
    -- $errors

    -- ** BillExpirationException
    , _BillExpirationException

    -- ** RequestChangedException
    , _RequestChangedException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** DataUnavailableException
    , _DataUnavailableException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetReservationUtilization
    , module Network.AWS.CostExplorer.GetReservationUtilization

    -- ** GetTags
    , module Network.AWS.CostExplorer.GetTags

    -- ** GetReservationCoverage
    , module Network.AWS.CostExplorer.GetReservationCoverage

    -- ** GetDimensionValues
    , module Network.AWS.CostExplorer.GetDimensionValues

    -- ** GetReservationPurchaseRecommendation
    , module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation

    -- ** GetCostAndUsage
    , module Network.AWS.CostExplorer.GetCostAndUsage

    -- * Types

    -- ** AccountScope
    , AccountScope (..)

    -- ** Context
    , Context (..)

    -- ** Dimension
    , Dimension (..)

    -- ** Granularity
    , Granularity (..)

    -- ** GroupDefinitionType
    , GroupDefinitionType (..)

    -- ** LookbackPeriodInDays
    , LookbackPeriodInDays (..)

    -- ** OfferingClass
    , OfferingClass (..)

    -- ** PaymentOption
    , PaymentOption (..)

    -- ** TermInYears
    , TermInYears (..)

    -- ** Coverage
    , Coverage
    , coverage
    , cCoverageHours

    -- ** CoverageByTime
    , CoverageByTime
    , coverageByTime
    , cbtGroups
    , cbtTimePeriod
    , cbtTotal

    -- ** CoverageHours
    , CoverageHours
    , coverageHours
    , chCoverageHoursPercentage
    , chOnDemandHours
    , chTotalRunningHours
    , chReservedHours

    -- ** DateInterval
    , DateInterval
    , dateInterval
    , diStart
    , diEnd

    -- ** DimensionValues
    , DimensionValues
    , dimensionValues
    , dvValues
    , dvKey

    -- ** DimensionValuesWithAttributes
    , DimensionValuesWithAttributes
    , dimensionValuesWithAttributes
    , dvwaValue
    , dvwaAttributes

    -- ** EC2InstanceDetails
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

    -- ** EC2Specification
    , EC2Specification
    , ec2Specification
    , esOfferingClass

    -- ** Expression
    , Expression
    , expression
    , eNot
    , eAnd
    , eOr
    , eDimensions
    , eTags

    -- ** Group
    , Group
    , group'
    , gMetrics
    , gKeys

    -- ** GroupDefinition
    , GroupDefinition
    , groupDefinition
    , gdKey
    , gdType

    -- ** InstanceDetails
    , InstanceDetails
    , instanceDetails
    , idRDSInstanceDetails
    , idEC2InstanceDetails

    -- ** MetricValue
    , MetricValue
    , metricValue
    , mvAmount
    , mvUnit

    -- ** RDSInstanceDetails
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

    -- ** ReservationAggregates
    , ReservationAggregates
    , reservationAggregates
    , raPurchasedHours
    , raTotalActualHours
    , raUtilizationPercentage
    , raUnusedHours

    -- ** ReservationCoverageGroup
    , ReservationCoverageGroup
    , reservationCoverageGroup
    , rcgCoverage
    , rcgAttributes

    -- ** ReservationPurchaseRecommendation
    , ReservationPurchaseRecommendation
    , reservationPurchaseRecommendation
    , rprTermInYears
    , rprRecommendationSummary
    , rprServiceSpecification
    , rprAccountScope
    , rprRecommendationDetails
    , rprLookbackPeriodInDays
    , rprPaymentOption

    -- ** ReservationPurchaseRecommendationDetail
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

    -- ** ReservationPurchaseRecommendationMetadata
    , ReservationPurchaseRecommendationMetadata
    , reservationPurchaseRecommendationMetadata
    , rprmRecommendationId
    , rprmGenerationTimestamp

    -- ** ReservationPurchaseRecommendationSummary
    , ReservationPurchaseRecommendationSummary
    , reservationPurchaseRecommendationSummary
    , rprsCurrencyCode
    , rprsTotalEstimatedMonthlySavingsPercentage
    , rprsTotalEstimatedMonthlySavingsAmount

    -- ** ReservationUtilizationGroup
    , ReservationUtilizationGroup
    , reservationUtilizationGroup
    , rugValue
    , rugKey
    , rugAttributes
    , rugUtilization

    -- ** ResultByTime
    , ResultByTime
    , resultByTime
    , rbtGroups
    , rbtTimePeriod
    , rbtTotal
    , rbtEstimated

    -- ** ServiceSpecification
    , ServiceSpecification
    , serviceSpecification
    , ssEC2Specification

    -- ** TagValues
    , TagValues
    , tagValues
    , tvValues
    , tvKey

    -- ** UtilizationByTime
    , UtilizationByTime
    , utilizationByTime
    , ubtGroups
    , ubtTimePeriod
    , ubtTotal
    ) where

import Network.AWS.CostExplorer.GetCostAndUsage
import Network.AWS.CostExplorer.GetDimensionValues
import Network.AWS.CostExplorer.GetReservationCoverage
import Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
import Network.AWS.CostExplorer.GetReservationUtilization
import Network.AWS.CostExplorer.GetTags
import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CostExplorer'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
