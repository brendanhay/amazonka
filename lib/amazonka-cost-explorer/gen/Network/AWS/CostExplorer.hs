{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Cost Explorer API enables you to programmatically query your cost and usage data. You can query for aggregated data such as total monthly costs or total daily usage. You can also query for granular data, such as the number of daily write operations for Amazon DynamoDB database tables in your production environment. 
--
-- Service Endpoint
-- The Cost Explorer API provides the following endpoint:
--
--     * @https://ce.us-east-1.amazonaws.com@ 
--
--
-- For information about costs associated with the Cost Explorer API, see <http://aws.amazon.com/aws-cost-management/pricing/ AWS Cost Management Pricing> .
module Network.AWS.CostExplorer
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** UnknownSubscriptionException
    , _UnknownSubscriptionException

    -- ** BillExpirationException
    , _BillExpirationException

    -- ** RequestChangedException
    , _RequestChangedException

    -- ** UnresolvableUsageUnitException
    , _UnresolvableUsageUnitException

    -- ** ServiceQuotaExceededException
    , _ServiceQuotaExceededException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** DataUnavailableException
    , _DataUnavailableException

    -- ** UnknownMonitorException
    , _UnknownMonitorException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetReservationUtilization 
    , module Network.AWS.CostExplorer.GetReservationUtilization

    -- ** GetSavingsPlansCoverage 
    , module Network.AWS.CostExplorer.GetSavingsPlansCoverage

    -- ** GetTags 
    , module Network.AWS.CostExplorer.GetTags

    -- ** GetRightsizingRecommendation 
    , module Network.AWS.CostExplorer.GetRightsizingRecommendation

    -- ** GetCostAndUsageWithResources 
    , module Network.AWS.CostExplorer.GetCostAndUsageWithResources

    -- ** GetUsageForecast 
    , module Network.AWS.CostExplorer.GetUsageForecast

    -- ** GetReservationCoverage 
    , module Network.AWS.CostExplorer.GetReservationCoverage

    -- ** GetCostForecast 
    , module Network.AWS.CostExplorer.GetCostForecast

    -- ** GetDimensionValues 
    , module Network.AWS.CostExplorer.GetDimensionValues

    -- ** GetAnomalies 
    , module Network.AWS.CostExplorer.GetAnomalies

    -- ** GetReservationPurchaseRecommendation 
    , module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation

    -- ** DeleteAnomalyMonitor 
    , module Network.AWS.CostExplorer.DeleteAnomalyMonitor

    -- ** UpdateAnomalyMonitor 
    , module Network.AWS.CostExplorer.UpdateAnomalyMonitor

    -- ** ListCostCategoryDefinitions 
    , module Network.AWS.CostExplorer.ListCostCategoryDefinitions

    -- ** UpdateCostCategoryDefinition 
    , module Network.AWS.CostExplorer.UpdateCostCategoryDefinition

    -- ** DeleteCostCategoryDefinition 
    , module Network.AWS.CostExplorer.DeleteCostCategoryDefinition

    -- ** GetAnomalySubscriptions 
    , module Network.AWS.CostExplorer.GetAnomalySubscriptions

    -- ** CreateCostCategoryDefinition 
    , module Network.AWS.CostExplorer.CreateCostCategoryDefinition

    -- ** GetAnomalyMonitors 
    , module Network.AWS.CostExplorer.GetAnomalyMonitors

    -- ** DeleteAnomalySubscription 
    , module Network.AWS.CostExplorer.DeleteAnomalySubscription

    -- ** UpdateAnomalySubscription 
    , module Network.AWS.CostExplorer.UpdateAnomalySubscription

    -- ** GetCostAndUsage 
    , module Network.AWS.CostExplorer.GetCostAndUsage

    -- ** GetSavingsPlansPurchaseRecommendation 
    , module Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation

    -- ** ProvideAnomalyFeedback 
    , module Network.AWS.CostExplorer.ProvideAnomalyFeedback

    -- ** GetSavingsPlansUtilization 
    , module Network.AWS.CostExplorer.GetSavingsPlansUtilization

    -- ** DescribeCostCategoryDefinition 
    , module Network.AWS.CostExplorer.DescribeCostCategoryDefinition

    -- ** CreateAnomalySubscription 
    , module Network.AWS.CostExplorer.CreateAnomalySubscription

    -- ** CreateAnomalyMonitor 
    , module Network.AWS.CostExplorer.CreateAnomalyMonitor

    -- ** GetSavingsPlansUtilizationDetails 
    , module Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails

    -- * Types

    -- ** PurchasedHours
    , PurchasedHours (..)

    -- ** EC2ResourceUtilization
    , EC2ResourceUtilization (..)
    , mkEC2ResourceUtilization
    , ecruEBSResourceUtilization
    , ecruMaxCpuUtilizationPercentage
    , ecruMaxMemoryUtilizationPercentage
    , ecruMaxStorageUtilizationPercentage

    -- ** EC2ResourceDetails
    , EC2ResourceDetails (..)
    , mkEC2ResourceDetails
    , ecrdHourlyOnDemandRate
    , ecrdInstanceType
    , ecrdMemory
    , ecrdNetworkPerformance
    , ecrdPlatform
    , ecrdRegion
    , ecrdSku
    , ecrdStorage
    , ecrdVcpu

    -- ** MonitorType
    , MonitorType (..)

    -- ** RightsizingRecommendationMetadata
    , RightsizingRecommendationMetadata (..)
    , mkRightsizingRecommendationMetadata
    , rrmGenerationTimestamp
    , rrmLookbackPeriodInDays
    , rrmRecommendationId

    -- ** NextPageToken
    , NextPageToken (..)

    -- ** TotalActualHours
    , TotalActualHours (..)

    -- ** Context
    , Context (..)

    -- ** CostCategoryStatus
    , CostCategoryStatus (..)

    -- ** Group
    , Group (..)
    , mkGroup
    , gKeys
    , gMetrics

    -- ** SavingsPlansUtilizationAggregates
    , SavingsPlansUtilizationAggregates (..)
    , mkSavingsPlansUtilizationAggregates
    , spuaUtilization
    , spuaAmortizedCommitment
    , spuaSavings

    -- ** ResultByTime
    , ResultByTime (..)
    , mkResultByTime
    , rbtEstimated
    , rbtGroups
    , rbtTimePeriod
    , rbtTotal

    -- ** AttributeValue
    , AttributeValue (..)

    -- ** UtilizationPercentage
    , UtilizationPercentage (..)

    -- ** CoverageNormalizedUnits
    , CoverageNormalizedUnits (..)
    , mkCoverageNormalizedUnits
    , cnuCoverageNormalizedUnitsPercentage
    , cnuOnDemandNormalizedUnits
    , cnuReservedNormalizedUnits
    , cnuTotalRunningNormalizedUnits

    -- ** ForecastResult
    , ForecastResult (..)
    , mkForecastResult
    , frMeanValue
    , frPredictionIntervalLowerBound
    , frPredictionIntervalUpperBound
    , frTimePeriod

    -- ** ReservationAggregates
    , ReservationAggregates (..)
    , mkReservationAggregates
    , raAmortizedRecurringFee
    , raAmortizedUpfrontFee
    , raNetRISavings
    , raOnDemandCostOfRIHoursUsed
    , raPurchasedHours
    , raPurchasedUnits
    , raTotalActualHours
    , raTotalActualUnits
    , raTotalAmortizedFee
    , raTotalPotentialRISavings
    , raUnusedHours
    , raUnusedUnits
    , raUtilizationPercentage
    , raUtilizationPercentageInUnits

    -- ** TagValues
    , TagValues (..)
    , mkTagValues
    , tvKey
    , tvMatchOptions
    , tvValues

    -- ** GroupDefinition
    , GroupDefinition (..)
    , mkGroupDefinition
    , gdKey
    , gdType

    -- ** AnomalyDateInterval
    , AnomalyDateInterval (..)
    , mkAnomalyDateInterval
    , adiStartDate
    , adiEndDate

    -- ** Subscriber
    , Subscriber (..)
    , mkSubscriber
    , sAddress
    , sStatus
    , sType

    -- ** ReservationPurchaseRecommendation
    , ReservationPurchaseRecommendation (..)
    , mkReservationPurchaseRecommendation
    , rprAccountScope
    , rprLookbackPeriodInDays
    , rprPaymentOption
    , rprRecommendationDetails
    , rprRecommendationSummary
    , rprServiceSpecification
    , rprTermInYears

    -- ** CoverageHoursPercentage
    , CoverageHoursPercentage (..)

    -- ** ESInstanceDetails
    , ESInstanceDetails (..)
    , mkESInstanceDetails
    , esidCurrentGeneration
    , esidInstanceClass
    , esidInstanceSize
    , esidRegion
    , esidSizeFlexEligible

    -- ** OnDemandHours
    , OnDemandHours (..)

    -- ** OnDemandCost
    , OnDemandCost (..)

    -- ** TermInYears
    , TermInYears (..)

    -- ** SavingsPlansPurchaseRecommendation
    , SavingsPlansPurchaseRecommendation (..)
    , mkSavingsPlansPurchaseRecommendation
    , spprAccountScope
    , spprLookbackPeriodInDays
    , spprPaymentOption
    , spprSavingsPlansPurchaseRecommendationDetails
    , spprSavingsPlansPurchaseRecommendationSummary
    , spprSavingsPlansType
    , spprTermInYears

    -- ** Arn
    , Arn (..)

    -- ** RightsizingRecommendationSummary
    , RightsizingRecommendationSummary (..)
    , mkRightsizingRecommendationSummary
    , rrsEstimatedTotalMonthlySavingsAmount
    , rrsSavingsCurrencyCode
    , rrsSavingsPercentage
    , rrsTotalRecommendationCount

    -- ** SavingsPlansDetails
    , SavingsPlansDetails (..)
    , mkSavingsPlansDetails
    , spdInstanceFamily
    , spdOfferingId
    , spdRegion

    -- ** SavingsPlansUtilization
    , SavingsPlansUtilization (..)
    , mkSavingsPlansUtilization
    , spuTotalCommitment
    , spuUnusedCommitment
    , spuUsedCommitment
    , spuUtilizationPercentage

    -- ** AnomalyMonitor
    , AnomalyMonitor (..)
    , mkAnomalyMonitor
    , amMonitorName
    , amMonitorType
    , amCreationDate
    , amDimensionalValueCount
    , amLastEvaluatedDate
    , amLastUpdatedDate
    , amMonitorArn
    , amMonitorDimension
    , amMonitorSpecification

    -- ** AnomalyFeedbackType
    , AnomalyFeedbackType (..)

    -- ** ResourceUtilization
    , ResourceUtilization (..)
    , mkResourceUtilization
    , ruEC2ResourceUtilization

    -- ** ResourceDetails
    , ResourceDetails (..)
    , mkResourceDetails
    , rdEC2ResourceDetails

    -- ** ReservationGroupKey
    , ReservationGroupKey (..)

    -- ** DimensionValues
    , DimensionValues (..)
    , mkDimensionValues
    , dvKey
    , dvMatchOptions
    , dvValues

    -- ** TargetInstance
    , TargetInstance (..)
    , mkTargetInstance
    , tiCurrencyCode
    , tiDefaultTargetInstance
    , tiEstimatedMonthlyCost
    , tiEstimatedMonthlySavings
    , tiExpectedResourceUtilization
    , tiResourceDetails

    -- ** ServiceSpecification
    , ServiceSpecification (..)
    , mkServiceSpecification
    , ssEC2Specification

    -- ** SavingsPlansSavings
    , SavingsPlansSavings (..)
    , mkSavingsPlansSavings
    , spsNetSavings
    , spsOnDemandCostEquivalent

    -- ** ReservedNormalizedUnits
    , ReservedNormalizedUnits (..)

    -- ** MatchOption
    , MatchOption (..)

    -- ** Dimension
    , Dimension (..)

    -- ** RecommendationTarget
    , RecommendationTarget (..)

    -- ** SavingsPlansUtilizationByTime
    , SavingsPlansUtilizationByTime (..)
    , mkSavingsPlansUtilizationByTime
    , spubtTimePeriod
    , spubtUtilization
    , spubtAmortizedCommitment
    , spubtSavings

    -- ** MetricName
    , MetricName (..)

    -- ** CostCategoryValues
    , CostCategoryValues (..)
    , mkCostCategoryValues
    , ccvKey
    , ccvMatchOptions
    , ccvValues

    -- ** SearchString
    , SearchString (..)

    -- ** DateInterval
    , DateInterval (..)
    , mkDateInterval
    , diStart
    , diEnd

    -- ** AccountScope
    , AccountScope (..)

    -- ** CostCategory
    , CostCategory (..)
    , mkCostCategory
    , ccCostCategoryArn
    , ccEffectiveStart
    , ccName
    , ccRuleVersion
    , ccRules
    , ccEffectiveEnd
    , ccProcessingStatus

    -- ** Value
    , Value (..)

    -- ** TotalAmortizedFee
    , TotalAmortizedFee (..)

    -- ** AnomalySubscriptionFrequency
    , AnomalySubscriptionFrequency (..)

    -- ** Coverage
    , Coverage (..)
    , mkCoverage
    , cCoverageCost
    , cCoverageHours
    , cCoverageNormalizedUnits

    -- ** RDSInstanceDetails
    , RDSInstanceDetails (..)
    , mkRDSInstanceDetails
    , rdsidCurrentGeneration
    , rdsidDatabaseEdition
    , rdsidDatabaseEngine
    , rdsidDeploymentOption
    , rdsidFamily
    , rdsidInstanceType
    , rdsidLicenseModel
    , rdsidRegion
    , rdsidSizeFlexEligible

    -- ** TotalRunningHours
    , TotalRunningHours (..)

    -- ** TotalRunningNormalizedUnits
    , TotalRunningNormalizedUnits (..)

    -- ** UnusedUnits
    , UnusedUnits (..)

    -- ** ElastiCacheInstanceDetails
    , ElastiCacheInstanceDetails (..)
    , mkElastiCacheInstanceDetails
    , eCurrentGeneration
    , eFamily
    , eNodeType
    , eProductDescription
    , eRegion
    , eSizeFlexEligible

    -- ** SupportedSavingsPlansType
    , SupportedSavingsPlansType (..)

    -- ** Anomaly
    , Anomaly (..)
    , mkAnomaly
    , aAnomalyId
    , aAnomalyScore
    , aImpact
    , aMonitorArn
    , aAnomalyEndDate
    , aAnomalyStartDate
    , aDimensionValue
    , aFeedback
    , aRootCauses

    -- ** CostCategoryName
    , CostCategoryName (..)

    -- ** SavingsPlansPurchaseRecommendationDetail
    , SavingsPlansPurchaseRecommendationDetail (..)
    , mkSavingsPlansPurchaseRecommendationDetail
    , spprdAccountId
    , spprdCurrencyCode
    , spprdCurrentAverageHourlyOnDemandSpend
    , spprdCurrentMaximumHourlyOnDemandSpend
    , spprdCurrentMinimumHourlyOnDemandSpend
    , spprdEstimatedAverageUtilization
    , spprdEstimatedMonthlySavingsAmount
    , spprdEstimatedOnDemandCost
    , spprdEstimatedOnDemandCostWithCurrentCommitment
    , spprdEstimatedROI
    , spprdEstimatedSPCost
    , spprdEstimatedSavingsAmount
    , spprdEstimatedSavingsPercentage
    , spprdHourlyCommitmentToPurchase
    , spprdSavingsPlansDetails
    , spprdUpfrontCost

    -- ** Granularity
    , Granularity (..)

    -- ** UtilizationByTime
    , UtilizationByTime (..)
    , mkUtilizationByTime
    , ubtGroups
    , ubtTimePeriod
    , ubtTotal

    -- ** ReservationGroupValue
    , ReservationGroupValue (..)

    -- ** GenericString
    , GenericString (..)

    -- ** CostCategoryProcessingStatus
    , CostCategoryProcessingStatus (..)
    , mkCostCategoryProcessingStatus
    , ccpsComponent
    , ccpsStatus

    -- ** MonitorDimension
    , MonitorDimension (..)

    -- ** SavingsPlansUtilizationDetail
    , SavingsPlansUtilizationDetail (..)
    , mkSavingsPlansUtilizationDetail
    , spudAmortizedCommitment
    , spudAttributes
    , spudSavings
    , spudSavingsPlanArn
    , spudUtilization

    -- ** Metric
    , Metric (..)

    -- ** AttributeType
    , AttributeType (..)

    -- ** EBSResourceUtilization
    , EBSResourceUtilization (..)
    , mkEBSResourceUtilization
    , ebsruEbsReadBytesPerSecond
    , ebsruEbsReadOpsPerSecond
    , ebsruEbsWriteBytesPerSecond
    , ebsruEbsWriteOpsPerSecond

    -- ** ReservedHours
    , ReservedHours (..)

    -- ** RootCause
    , RootCause (..)
    , mkRootCause
    , rcLinkedAccount
    , rcRegion
    , rcService
    , rcUsageType

    -- ** UnusedHours
    , UnusedHours (..)

    -- ** Key
    , Key (..)

    -- ** CostCategoryRuleVersion
    , CostCategoryRuleVersion (..)

    -- ** EC2InstanceDetails
    , EC2InstanceDetails (..)
    , mkEC2InstanceDetails
    , ecidAvailabilityZone
    , ecidCurrentGeneration
    , ecidFamily
    , ecidInstanceType
    , ecidPlatform
    , ecidRegion
    , ecidSizeFlexEligible
    , ecidTenancy

    -- ** MetricValue
    , MetricValue (..)
    , mkMetricValue
    , mvAmount
    , mvUnit

    -- ** ReservationPurchaseRecommendationMetadata
    , ReservationPurchaseRecommendationMetadata (..)
    , mkReservationPurchaseRecommendationMetadata
    , rprmGenerationTimestamp
    , rprmRecommendationId

    -- ** SubscriberType
    , SubscriberType (..)

    -- ** AnomalySubscription
    , AnomalySubscription (..)
    , mkAnomalySubscription
    , asMonitorArnList
    , asSubscribers
    , asThreshold
    , asFrequency
    , asSubscriptionName
    , asAccountId
    , asSubscriptionArn

    -- ** GroupDefinitionType
    , GroupDefinitionType (..)

    -- ** CoverageNormalizedUnitsPercentage
    , CoverageNormalizedUnitsPercentage (..)

    -- ** Impact
    , Impact (..)
    , mkImpact
    , iMaxImpact
    , iTotalImpact

    -- ** Expression
    , Expression (..)
    , mkExpression
    , eAnd
    , eCostCategories
    , eDimensions
    , eNot
    , eOr
    , eTags

    -- ** PurchasedUnits
    , PurchasedUnits (..)

    -- ** OnDemandNormalizedUnits
    , OnDemandNormalizedUnits (..)

    -- ** CoverageHours
    , CoverageHours (..)
    , mkCoverageHours
    , chCoverageHoursPercentage
    , chOnDemandHours
    , chReservedHours
    , chTotalRunningHours

    -- ** ReservationUtilizationGroup
    , ReservationUtilizationGroup (..)
    , mkReservationUtilizationGroup
    , rugAttributes
    , rugKey
    , rugUtilization
    , rugValue

    -- ** AmortizedUpfrontFee
    , AmortizedUpfrontFee (..)

    -- ** CoverageCost
    , CoverageCost (..)
    , mkCoverageCost
    , ccOnDemandCost

    -- ** CostCategoryRule
    , CostCategoryRule (..)
    , mkCostCategoryRule
    , ccrValue
    , ccrRule

    -- ** DimensionValuesWithAttributes
    , DimensionValuesWithAttributes (..)
    , mkDimensionValuesWithAttributes
    , dvwaAttributes
    , dvwaValue

    -- ** TagKey
    , TagKey (..)

    -- ** ModifyRecommendationDetail
    , ModifyRecommendationDetail (..)
    , mkModifyRecommendationDetail
    , mrdTargetInstances

    -- ** AmortizedRecurringFee
    , AmortizedRecurringFee (..)

    -- ** CostCategoryValue
    , CostCategoryValue (..)

    -- ** CoverageByTime
    , CoverageByTime (..)
    , mkCoverageByTime
    , cbtGroups
    , cbtTimePeriod
    , cbtTotal

    -- ** TotalImpactFilter
    , TotalImpactFilter (..)
    , mkTotalImpactFilter
    , tifNumericOperator
    , tifStartValue
    , tifEndValue

    -- ** CurrentInstance
    , CurrentInstance (..)
    , mkCurrentInstance
    , ciCurrencyCode
    , ciInstanceName
    , ciMonthlyCost
    , ciOnDemandHoursInLookbackPeriod
    , ciReservationCoveredHoursInLookbackPeriod
    , ciResourceDetails
    , ciResourceId
    , ciResourceUtilization
    , ciSavingsPlansCoveredHoursInLookbackPeriod
    , ciTags
    , ciTotalRunningHoursInLookbackPeriod

    -- ** CostCategoryStatusComponent
    , CostCategoryStatusComponent (..)

    -- ** RightsizingType
    , RightsizingType (..)

    -- ** UtilizationPercentageInUnits
    , UtilizationPercentageInUnits (..)

    -- ** RedshiftInstanceDetails
    , RedshiftInstanceDetails (..)
    , mkRedshiftInstanceDetails
    , ridCurrentGeneration
    , ridFamily
    , ridNodeType
    , ridRegion
    , ridSizeFlexEligible

    -- ** SavingsPlansPurchaseRecommendationMetadata
    , SavingsPlansPurchaseRecommendationMetadata (..)
    , mkSavingsPlansPurchaseRecommendationMetadata
    , spprmAdditionalMetadata
    , spprmGenerationTimestamp
    , spprmRecommendationId

    -- ** CostCategoryReference
    , CostCategoryReference (..)
    , mkCostCategoryReference
    , ccrCostCategoryArn
    , ccrEffectiveEnd
    , ccrEffectiveStart
    , ccrName
    , ccrNumberOfRules
    , ccrProcessingStatus
    , ccrValues

    -- ** SavingsPlansAmortizedCommitment
    , SavingsPlansAmortizedCommitment (..)
    , mkSavingsPlansAmortizedCommitment
    , spacAmortizedRecurringCommitment
    , spacAmortizedUpfrontCommitment
    , spacTotalAmortizedCommitment

    -- ** NetRISavings
    , NetRISavings (..)

    -- ** InstanceDetails
    , InstanceDetails (..)
    , mkInstanceDetails
    , idEC2InstanceDetails
    , idESInstanceDetails
    , idElastiCacheInstanceDetails
    , idRDSInstanceDetails
    , idRedshiftInstanceDetails

    -- ** OnDemandCostOfRIHoursUsed
    , OnDemandCostOfRIHoursUsed (..)

    -- ** Entity
    , Entity (..)

    -- ** ReservationPurchaseRecommendationDetail
    , ReservationPurchaseRecommendationDetail (..)
    , mkReservationPurchaseRecommendationDetail
    , rprdAccountId
    , rprdAverageNormalizedUnitsUsedPerHour
    , rprdAverageNumberOfInstancesUsedPerHour
    , rprdAverageUtilization
    , rprdCurrencyCode
    , rprdEstimatedBreakEvenInMonths
    , rprdEstimatedMonthlyOnDemandCost
    , rprdEstimatedMonthlySavingsAmount
    , rprdEstimatedMonthlySavingsPercentage
    , rprdEstimatedReservationCostForLookbackPeriod
    , rprdInstanceDetails
    , rprdMaximumNormalizedUnitsUsedPerHour
    , rprdMaximumNumberOfInstancesUsedPerHour
    , rprdMinimumNormalizedUnitsUsedPerHour
    , rprdMinimumNumberOfInstancesUsedPerHour
    , rprdRecommendedNormalizedUnitsToPurchase
    , rprdRecommendedNumberOfInstancesToPurchase
    , rprdRecurringStandardMonthlyCost
    , rprdUpfrontCost

    -- ** NumericOperator
    , NumericOperator (..)

    -- ** ZonedDateTime
    , ZonedDateTime (..)

    -- ** OfferingClass
    , OfferingClass (..)

    -- ** ReservationPurchaseRecommendationSummary
    , ReservationPurchaseRecommendationSummary (..)
    , mkReservationPurchaseRecommendationSummary
    , rprsCurrencyCode
    , rprsTotalEstimatedMonthlySavingsAmount
    , rprsTotalEstimatedMonthlySavingsPercentage

    -- ** TotalPotentialRISavings
    , TotalPotentialRISavings (..)

    -- ** SubscriberStatus
    , SubscriberStatus (..)

    -- ** AnomalyScore
    , AnomalyScore (..)
    , mkAnomalyScore
    , asMaxScore
    , asCurrentScore

    -- ** RightsizingRecommendationConfiguration
    , RightsizingRecommendationConfiguration (..)
    , mkRightsizingRecommendationConfiguration
    , rrcRecommendationTarget
    , rrcBenefitsConsidered

    -- ** SavingsPlansCoverage
    , SavingsPlansCoverage (..)
    , mkSavingsPlansCoverage
    , spcAttributes
    , spcCoverage
    , spcTimePeriod

    -- ** SavingsPlanArn
    , SavingsPlanArn (..)

    -- ** LookbackPeriodInDays
    , LookbackPeriodInDays (..)

    -- ** SavingsPlansCoverageData
    , SavingsPlansCoverageData (..)
    , mkSavingsPlansCoverageData
    , spcdCoveragePercentage
    , spcdOnDemandCost
    , spcdSpendCoveredBySavingsPlans
    , spcdTotalCost

    -- ** RightsizingRecommendation
    , RightsizingRecommendation (..)
    , mkRightsizingRecommendation
    , rrAccountId
    , rrCurrentInstance
    , rrModifyRecommendationDetail
    , rrRightsizingType
    , rrTerminateRecommendationDetail

    -- ** PaymentOption
    , PaymentOption (..)

    -- ** SavingsPlansPurchaseRecommendationSummary
    , SavingsPlansPurchaseRecommendationSummary (..)
    , mkSavingsPlansPurchaseRecommendationSummary
    , spprsCurrencyCode
    , spprsCurrentOnDemandSpend
    , spprsDailyCommitmentToPurchase
    , spprsEstimatedMonthlySavingsAmount
    , spprsEstimatedOnDemandCostWithCurrentCommitment
    , spprsEstimatedROI
    , spprsEstimatedSavingsAmount
    , spprsEstimatedSavingsPercentage
    , spprsEstimatedTotalCost
    , spprsHourlyCommitmentToPurchase
    , spprsTotalRecommendationCount

    -- ** EC2Specification
    , EC2Specification (..)
    , mkEC2Specification
    , ecsOfferingClass

    -- ** ReservationCoverageGroup
    , ReservationCoverageGroup (..)
    , mkReservationCoverageGroup
    , rcgAttributes
    , rcgCoverage

    -- ** TotalActualUnits
    , TotalActualUnits (..)

    -- ** TerminateRecommendationDetail
    , TerminateRecommendationDetail (..)
    , mkTerminateRecommendationDetail
    , trdCurrencyCode
    , trdEstimatedMonthlySavings

    -- ** MaxCpuUtilizationPercentage
    , MaxCpuUtilizationPercentage (..)

    -- ** MaxMemoryUtilizationPercentage
    , MaxMemoryUtilizationPercentage (..)

    -- ** MaxStorageUtilizationPercentage
    , MaxStorageUtilizationPercentage (..)

    -- ** HourlyOnDemandRate
    , HourlyOnDemandRate (..)

    -- ** InstanceType
    , InstanceType (..)

    -- ** Memory
    , Memory (..)

    -- ** NetworkPerformance
    , NetworkPerformance (..)

    -- ** Platform
    , Platform (..)

    -- ** Region
    , Region (..)

    -- ** Sku
    , Sku (..)

    -- ** Storage
    , Storage (..)

    -- ** Vcpu
    , Vcpu (..)

    -- ** GenerationTimestamp
    , GenerationTimestamp (..)

    -- ** RecommendationId
    , RecommendationId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** MeanValue
    , MeanValue (..)

    -- ** PredictionIntervalLowerBound
    , PredictionIntervalLowerBound (..)

    -- ** PredictionIntervalUpperBound
    , PredictionIntervalUpperBound (..)

    -- ** StartDate
    , StartDate (..)

    -- ** EndDate
    , EndDate (..)

    -- ** Address
    , Address (..)

    -- ** InstanceClass
    , InstanceClass (..)

    -- ** InstanceSize
    , InstanceSize (..)

    -- ** Service
    , Service (..)

    -- ** AccountId
    , AccountId (..)

    -- ** EstimatedTotalMonthlySavingsAmount
    , EstimatedTotalMonthlySavingsAmount (..)

    -- ** SavingsCurrencyCode
    , SavingsCurrencyCode (..)

    -- ** SavingsPercentage
    , SavingsPercentage (..)

    -- ** TotalRecommendationCount
    , TotalRecommendationCount (..)

    -- ** InstanceFamily
    , InstanceFamily (..)

    -- ** OfferingId
    , OfferingId (..)

    -- ** MonitorArn
    , MonitorArn (..)

    -- ** MonitorName
    , MonitorName (..)

    -- ** SubscriptionArn
    , SubscriptionArn (..)

    -- ** TotalCommitment
    , TotalCommitment (..)

    -- ** UnusedCommitment
    , UnusedCommitment (..)

    -- ** UsedCommitment
    , UsedCommitment (..)

    -- ** CreationDate
    , CreationDate (..)

    -- ** LastEvaluatedDate
    , LastEvaluatedDate (..)

    -- ** LastUpdatedDate
    , LastUpdatedDate (..)

    -- ** CurrencyCode
    , CurrencyCode (..)

    -- ** EstimatedMonthlyCost
    , EstimatedMonthlyCost (..)

    -- ** EstimatedMonthlySavings
    , EstimatedMonthlySavings (..)

    -- ** NetSavings
    , NetSavings (..)

    -- ** OnDemandCostEquivalent
    , OnDemandCostEquivalent (..)

    -- ** Start
    , Start (..)

    -- ** End
    , End (..)

    -- ** EffectiveStart
    , EffectiveStart (..)

    -- ** Name
    , Name (..)

    -- ** EffectiveEnd
    , EffectiveEnd (..)

    -- ** EffectiveOn
    , EffectiveOn (..)

    -- ** DatabaseEdition
    , DatabaseEdition (..)

    -- ** DatabaseEngine
    , DatabaseEngine (..)

    -- ** DeploymentOption
    , DeploymentOption (..)

    -- ** Family
    , Family (..)

    -- ** LicenseModel
    , LicenseModel (..)

    -- ** NodeType
    , NodeType (..)

    -- ** ProductDescription
    , ProductDescription (..)

    -- ** AnomalyId
    , AnomalyId (..)

    -- ** AnomalyEndDate
    , AnomalyEndDate (..)

    -- ** AnomalyStartDate
    , AnomalyStartDate (..)

    -- ** DimensionValue
    , DimensionValue (..)

    -- ** CurrentAverageHourlyOnDemandSpend
    , CurrentAverageHourlyOnDemandSpend (..)

    -- ** CurrentMaximumHourlyOnDemandSpend
    , CurrentMaximumHourlyOnDemandSpend (..)

    -- ** CurrentMinimumHourlyOnDemandSpend
    , CurrentMinimumHourlyOnDemandSpend (..)

    -- ** EstimatedAverageUtilization
    , EstimatedAverageUtilization (..)

    -- ** EstimatedMonthlySavingsAmount
    , EstimatedMonthlySavingsAmount (..)

    -- ** EstimatedOnDemandCost
    , EstimatedOnDemandCost (..)

    -- ** EstimatedOnDemandCostWithCurrentCommitment
    , EstimatedOnDemandCostWithCurrentCommitment (..)

    -- ** EstimatedROI
    , EstimatedROI (..)

    -- ** EstimatedSPCost
    , EstimatedSPCost (..)

    -- ** EstimatedSavingsAmount
    , EstimatedSavingsAmount (..)

    -- ** EstimatedSavingsPercentage
    , EstimatedSavingsPercentage (..)

    -- ** HourlyCommitmentToPurchase
    , HourlyCommitmentToPurchase (..)

    -- ** UpfrontCost
    , UpfrontCost (..)

    -- ** Amount
    , Amount (..)

    -- ** Unit
    , Unit (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Waiters
import Network.AWS.CostExplorer.GetReservationUtilization
import Network.AWS.CostExplorer.GetSavingsPlansCoverage
import Network.AWS.CostExplorer.GetTags
import Network.AWS.CostExplorer.GetRightsizingRecommendation
import Network.AWS.CostExplorer.GetCostAndUsageWithResources
import Network.AWS.CostExplorer.GetUsageForecast
import Network.AWS.CostExplorer.GetReservationCoverage
import Network.AWS.CostExplorer.GetCostForecast
import Network.AWS.CostExplorer.GetDimensionValues
import Network.AWS.CostExplorer.GetAnomalies
import Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
import Network.AWS.CostExplorer.DeleteAnomalyMonitor
import Network.AWS.CostExplorer.UpdateAnomalyMonitor
import Network.AWS.CostExplorer.ListCostCategoryDefinitions
import Network.AWS.CostExplorer.UpdateCostCategoryDefinition
import Network.AWS.CostExplorer.DeleteCostCategoryDefinition
import Network.AWS.CostExplorer.GetAnomalySubscriptions
import Network.AWS.CostExplorer.CreateCostCategoryDefinition
import Network.AWS.CostExplorer.GetAnomalyMonitors
import Network.AWS.CostExplorer.DeleteAnomalySubscription
import Network.AWS.CostExplorer.UpdateAnomalySubscription
import Network.AWS.CostExplorer.GetCostAndUsage
import Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
import Network.AWS.CostExplorer.ProvideAnomalyFeedback
import Network.AWS.CostExplorer.GetSavingsPlansUtilization
import Network.AWS.CostExplorer.DescribeCostCategoryDefinition
import Network.AWS.CostExplorer.CreateAnomalySubscription
import Network.AWS.CostExplorer.CreateAnomalyMonitor
import Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
import qualified Network.AWS.Prelude as Lude

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
