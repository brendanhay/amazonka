{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- Service Endpoint
--
-- The Cost Explorer API provides the following endpoint:
--
--     * @https://ce.us-east-1.amazonaws.com@
--
--
--
-- For information about costs associated with the Cost Explorer API, see <http://aws.amazon.com/aws-cost-management/pricing/ AWS Cost Management Pricing> .
module Network.AWS.CostExplorer
  ( -- * Service Configuration
    costExplorer,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetReservationUtilization
    module Network.AWS.CostExplorer.GetReservationUtilization,

    -- ** GetSavingsPlansCoverage
    module Network.AWS.CostExplorer.GetSavingsPlansCoverage,

    -- ** GetTags
    module Network.AWS.CostExplorer.GetTags,

    -- ** GetRightsizingRecommendation
    module Network.AWS.CostExplorer.GetRightsizingRecommendation,

    -- ** GetCostAndUsageWithResources
    module Network.AWS.CostExplorer.GetCostAndUsageWithResources,

    -- ** GetUsageForecast
    module Network.AWS.CostExplorer.GetUsageForecast,

    -- ** GetReservationCoverage
    module Network.AWS.CostExplorer.GetReservationCoverage,

    -- ** GetCostForecast
    module Network.AWS.CostExplorer.GetCostForecast,

    -- ** GetDimensionValues
    module Network.AWS.CostExplorer.GetDimensionValues,

    -- ** GetAnomalies
    module Network.AWS.CostExplorer.GetAnomalies,

    -- ** GetReservationPurchaseRecommendation
    module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation,

    -- ** DeleteAnomalyMonitor
    module Network.AWS.CostExplorer.DeleteAnomalyMonitor,

    -- ** UpdateAnomalyMonitor
    module Network.AWS.CostExplorer.UpdateAnomalyMonitor,

    -- ** ListCostCategoryDefinitions
    module Network.AWS.CostExplorer.ListCostCategoryDefinitions,

    -- ** UpdateCostCategoryDefinition
    module Network.AWS.CostExplorer.UpdateCostCategoryDefinition,

    -- ** DeleteCostCategoryDefinition
    module Network.AWS.CostExplorer.DeleteCostCategoryDefinition,

    -- ** GetAnomalySubscriptions
    module Network.AWS.CostExplorer.GetAnomalySubscriptions,

    -- ** CreateCostCategoryDefinition
    module Network.AWS.CostExplorer.CreateCostCategoryDefinition,

    -- ** GetAnomalyMonitors
    module Network.AWS.CostExplorer.GetAnomalyMonitors,

    -- ** DeleteAnomalySubscription
    module Network.AWS.CostExplorer.DeleteAnomalySubscription,

    -- ** UpdateAnomalySubscription
    module Network.AWS.CostExplorer.UpdateAnomalySubscription,

    -- ** GetCostAndUsage
    module Network.AWS.CostExplorer.GetCostAndUsage,

    -- ** GetSavingsPlansPurchaseRecommendation
    module Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation,

    -- ** ProvideAnomalyFeedback
    module Network.AWS.CostExplorer.ProvideAnomalyFeedback,

    -- ** GetSavingsPlansUtilization
    module Network.AWS.CostExplorer.GetSavingsPlansUtilization,

    -- ** DescribeCostCategoryDefinition
    module Network.AWS.CostExplorer.DescribeCostCategoryDefinition,

    -- ** CreateAnomalySubscription
    module Network.AWS.CostExplorer.CreateAnomalySubscription,

    -- ** CreateAnomalyMonitor
    module Network.AWS.CostExplorer.CreateAnomalyMonitor,

    -- ** GetSavingsPlansUtilizationDetails
    module Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails,

    -- * Types

    -- ** AccountScope
    AccountScope (..),

    -- ** AnomalyFeedbackType
    AnomalyFeedbackType (..),

    -- ** AnomalySubscriptionFrequency
    AnomalySubscriptionFrequency (..),

    -- ** Context
    Context (..),

    -- ** CostCategoryRuleVersion
    CostCategoryRuleVersion (..),

    -- ** CostCategoryStatus
    CostCategoryStatus (..),

    -- ** CostCategoryStatusComponent
    CostCategoryStatusComponent (..),

    -- ** Dimension
    Dimension (..),

    -- ** Granularity
    Granularity (..),

    -- ** GroupDefinitionType
    GroupDefinitionType (..),

    -- ** LookbackPeriodInDays
    LookbackPeriodInDays (..),

    -- ** MatchOption
    MatchOption (..),

    -- ** Metric
    Metric (..),

    -- ** MonitorDimension
    MonitorDimension (..),

    -- ** MonitorType
    MonitorType (..),

    -- ** NumericOperator
    NumericOperator (..),

    -- ** OfferingClass
    OfferingClass (..),

    -- ** PaymentOption
    PaymentOption (..),

    -- ** RecommendationTarget
    RecommendationTarget (..),

    -- ** RightsizingType
    RightsizingType (..),

    -- ** SubscriberStatus
    SubscriberStatus (..),

    -- ** SubscriberType
    SubscriberType (..),

    -- ** SupportedSavingsPlansType
    SupportedSavingsPlansType (..),

    -- ** TermInYears
    TermInYears (..),

    -- ** Anomaly
    Anomaly,
    anomaly,
    aAnomalyStartDate,
    aDimensionValue,
    aRootCauses,
    aAnomalyEndDate,
    aFeedback,
    aAnomalyId,
    aAnomalyScore,
    aImpact,
    aMonitorARN,

    -- ** AnomalyDateInterval
    AnomalyDateInterval,
    anomalyDateInterval,
    adiEndDate,
    adiStartDate,

    -- ** AnomalyMonitor
    AnomalyMonitor,
    anomalyMonitor,
    amDimensionalValueCount,
    amMonitorSpecification,
    amMonitorDimension,
    amCreationDate,
    amLastUpdatedDate,
    amLastEvaluatedDate,
    amMonitorARN,
    amMonitorName,
    amMonitorType,

    -- ** AnomalyScore
    AnomalyScore,
    anomalyScore,
    asMaxScore,
    asCurrentScore,

    -- ** AnomalySubscription
    AnomalySubscription,
    anomalySubscription,
    asAccountId,
    asSubscriptionARN,
    asMonitorARNList,
    asSubscribers,
    asThreshold,
    asFrequency,
    asSubscriptionName,

    -- ** CostCategory
    CostCategory,
    costCategory,
    ccProcessingStatus,
    ccEffectiveEnd,
    ccCostCategoryARN,
    ccEffectiveStart,
    ccName,
    ccRuleVersion,
    ccRules,

    -- ** CostCategoryProcessingStatus
    CostCategoryProcessingStatus,
    costCategoryProcessingStatus,
    ccpsStatus,
    ccpsComponent,

    -- ** CostCategoryReference
    CostCategoryReference,
    costCategoryReference,
    ccrEffectiveStart,
    ccrValues,
    ccrCostCategoryARN,
    ccrProcessingStatus,
    ccrNumberOfRules,
    ccrName,
    ccrEffectiveEnd,

    -- ** CostCategoryRule
    CostCategoryRule,
    costCategoryRule,
    ccrValue,
    ccrRule,

    -- ** CostCategoryValues
    CostCategoryValues,
    costCategoryValues,
    ccvValues,
    ccvKey,
    ccvMatchOptions,

    -- ** Coverage
    Coverage,
    coverage,
    cCoverageNormalizedUnits,
    cCoverageHours,
    cCoverageCost,

    -- ** CoverageByTime
    CoverageByTime,
    coverageByTime,
    cbtGroups,
    cbtTimePeriod,
    cbtTotal,

    -- ** CoverageCost
    CoverageCost,
    coverageCost,
    ccOnDemandCost,

    -- ** CoverageHours
    CoverageHours,
    coverageHours,
    chCoverageHoursPercentage,
    chOnDemandHours,
    chTotalRunningHours,
    chReservedHours,

    -- ** CoverageNormalizedUnits
    CoverageNormalizedUnits,
    coverageNormalizedUnits,
    cnuReservedNormalizedUnits,
    cnuTotalRunningNormalizedUnits,
    cnuCoverageNormalizedUnitsPercentage,
    cnuOnDemandNormalizedUnits,

    -- ** CurrentInstance
    CurrentInstance,
    currentInstance,
    ciResourceId,
    ciCurrencyCode,
    ciResourceUtilization,
    ciResourceDetails,
    ciTotalRunningHoursInLookbackPeriod,
    ciReservationCoveredHoursInLookbackPeriod,
    ciOnDemandHoursInLookbackPeriod,
    ciMonthlyCost,
    ciInstanceName,
    ciSavingsPlansCoveredHoursInLookbackPeriod,
    ciTags,

    -- ** DateInterval
    DateInterval,
    dateInterval,
    diStart,
    diEnd,

    -- ** DimensionValues
    DimensionValues,
    dimensionValues,
    dvValues,
    dvKey,
    dvMatchOptions,

    -- ** DimensionValuesWithAttributes
    DimensionValuesWithAttributes,
    dimensionValuesWithAttributes,
    dvwaValue,
    dvwaAttributes,

    -- ** EBSResourceUtilization
    EBSResourceUtilization,
    ebsResourceUtilization,
    eruEBSWriteBytesPerSecond,
    eruEBSWriteOpsPerSecond,
    eruEBSReadOpsPerSecond,
    eruEBSReadBytesPerSecond,

    -- ** EC2InstanceDetails
    EC2InstanceDetails,
    ec2InstanceDetails,
    eidCurrentGeneration,
    eidPlatform,
    eidFamily,
    eidInstanceType,
    eidAvailabilityZone,
    eidSizeFlexEligible,
    eidTenancy,
    eidRegion,

    -- ** EC2ResourceDetails
    EC2ResourceDetails,
    ec2ResourceDetails,
    erdPlatform,
    erdVcpu,
    erdNetworkPerformance,
    erdMemory,
    erdInstanceType,
    erdStorage,
    erdSku,
    erdRegion,
    erdHourlyOnDemandRate,

    -- ** EC2ResourceUtilization
    EC2ResourceUtilization,
    ec2ResourceUtilization,
    eruMaxCPUUtilizationPercentage,
    eruEBSResourceUtilization,
    eruMaxStorageUtilizationPercentage,
    eruMaxMemoryUtilizationPercentage,

    -- ** EC2Specification
    EC2Specification,
    ec2Specification,
    esOfferingClass,

    -- ** ESInstanceDetails
    ESInstanceDetails,
    eSInstanceDetails,
    esidCurrentGeneration,
    esidInstanceClass,
    esidInstanceSize,
    esidSizeFlexEligible,
    esidRegion,

    -- ** ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails,
    elastiCacheInstanceDetails,
    ecidCurrentGeneration,
    ecidProductDescription,
    ecidFamily,
    ecidSizeFlexEligible,
    ecidRegion,
    ecidNodeType,

    -- ** Expression
    Expression,
    expression,
    eNot,
    eAnd,
    eOr,
    eCostCategories,
    eDimensions,
    eTags,

    -- ** ForecastResult
    ForecastResult,
    forecastResult,
    frTimePeriod,
    frMeanValue,
    frPredictionIntervalUpperBound,
    frPredictionIntervalLowerBound,

    -- ** Group
    Group,
    group',
    gMetrics,
    gKeys,

    -- ** GroupDefinition
    GroupDefinition,
    groupDefinition,
    gdKey,
    gdType,

    -- ** Impact
    Impact,
    impact,
    iTotalImpact,
    iMaxImpact,

    -- ** InstanceDetails
    InstanceDetails,
    instanceDetails,
    idESInstanceDetails,
    idRDSInstanceDetails,
    idElastiCacheInstanceDetails,
    idEC2InstanceDetails,
    idRedshiftInstanceDetails,

    -- ** MetricValue
    MetricValue,
    metricValue,
    mvAmount,
    mvUnit,

    -- ** ModifyRecommendationDetail
    ModifyRecommendationDetail,
    modifyRecommendationDetail,
    mrdTargetInstances,

    -- ** RDSInstanceDetails
    RDSInstanceDetails,
    rdsInstanceDetails,
    ridCurrentGeneration,
    ridDeploymentOption,
    ridFamily,
    ridInstanceType,
    ridLicenseModel,
    ridSizeFlexEligible,
    ridRegion,
    ridDatabaseEngine,
    ridDatabaseEdition,

    -- ** RedshiftInstanceDetails
    RedshiftInstanceDetails,
    redshiftInstanceDetails,
    rCurrentGeneration,
    rFamily,
    rSizeFlexEligible,
    rRegion,
    rNodeType,

    -- ** ReservationAggregates
    ReservationAggregates,
    reservationAggregates,
    raPurchasedHours,
    raTotalActualHours,
    raUtilizationPercentage,
    raTotalAmortizedFee,
    raUnusedUnits,
    raUnusedHours,
    raPurchasedUnits,
    raAmortizedUpfrontFee,
    raAmortizedRecurringFee,
    raUtilizationPercentageInUnits,
    raNetRISavings,
    raOnDemandCostOfRIHoursUsed,
    raTotalPotentialRISavings,
    raTotalActualUnits,

    -- ** ReservationCoverageGroup
    ReservationCoverageGroup,
    reservationCoverageGroup,
    rcgCoverage,
    rcgAttributes,

    -- ** ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation,
    reservationPurchaseRecommendation,
    rprTermInYears,
    rprRecommendationSummary,
    rprServiceSpecification,
    rprAccountScope,
    rprRecommendationDetails,
    rprLookbackPeriodInDays,
    rprPaymentOption,

    -- ** ReservationPurchaseRecommendationDetail
    ReservationPurchaseRecommendationDetail,
    reservationPurchaseRecommendationDetail,
    rprdMaximumNormalizedUnitsUsedPerHour,
    rprdRecurringStandardMonthlyCost,
    rprdAverageNormalizedUnitsUsedPerHour,
    rprdCurrencyCode,
    rprdEstimatedMonthlySavingsPercentage,
    rprdRecommendedNormalizedUnitsToPurchase,
    rprdAverageUtilization,
    rprdAccountId,
    rprdEstimatedMonthlySavingsAmount,
    rprdUpfrontCost,
    rprdMinimumNormalizedUnitsUsedPerHour,
    rprdEstimatedMonthlyOnDemandCost,
    rprdRecommendedNumberOfInstancesToPurchase,
    rprdMaximumNumberOfInstancesUsedPerHour,
    rprdEstimatedReservationCostForLookbackPeriod,
    rprdInstanceDetails,
    rprdAverageNumberOfInstancesUsedPerHour,
    rprdMinimumNumberOfInstancesUsedPerHour,
    rprdEstimatedBreakEvenInMonths,

    -- ** ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata,
    reservationPurchaseRecommendationMetadata,
    rprmRecommendationId,
    rprmGenerationTimestamp,

    -- ** ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary,
    reservationPurchaseRecommendationSummary,
    rprsCurrencyCode,
    rprsTotalEstimatedMonthlySavingsPercentage,
    rprsTotalEstimatedMonthlySavingsAmount,

    -- ** ReservationUtilizationGroup
    ReservationUtilizationGroup,
    reservationUtilizationGroup,
    rugValue,
    rugKey,
    rugAttributes,
    rugUtilization,

    -- ** ResourceDetails
    ResourceDetails,
    resourceDetails,
    rdEC2ResourceDetails,

    -- ** ResourceUtilization
    ResourceUtilization,
    resourceUtilization,
    ruEC2ResourceUtilization,

    -- ** ResultByTime
    ResultByTime,
    resultByTime,
    rbtGroups,
    rbtTimePeriod,
    rbtTotal,
    rbtEstimated,

    -- ** RightsizingRecommendation
    RightsizingRecommendation,
    rightsizingRecommendation,
    rrAccountId,
    rrModifyRecommendationDetail,
    rrCurrentInstance,
    rrRightsizingType,
    rrTerminateRecommendationDetail,

    -- ** RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration,
    rightsizingRecommendationConfiguration,
    rrcRecommendationTarget,
    rrcBenefitsConsidered,

    -- ** RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata,
    rightsizingRecommendationMetadata,
    rrmRecommendationId,
    rrmGenerationTimestamp,
    rrmLookbackPeriodInDays,

    -- ** RightsizingRecommendationSummary
    RightsizingRecommendationSummary,
    rightsizingRecommendationSummary,
    rrsSavingsPercentage,
    rrsSavingsCurrencyCode,
    rrsTotalRecommendationCount,
    rrsEstimatedTotalMonthlySavingsAmount,

    -- ** RootCause
    RootCause,
    rootCause,
    rcService,
    rcUsageType,
    rcLinkedAccount,
    rcRegion,

    -- ** SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment,
    savingsPlansAmortizedCommitment,
    spacAmortizedUpfrontCommitment,
    spacTotalAmortizedCommitment,
    spacAmortizedRecurringCommitment,

    -- ** SavingsPlansCoverage
    SavingsPlansCoverage,
    savingsPlansCoverage,
    spcTimePeriod,
    spcCoverage,
    spcAttributes,

    -- ** SavingsPlansCoverageData
    SavingsPlansCoverageData,
    savingsPlansCoverageData,
    spcdOnDemandCost,
    spcdSpendCoveredBySavingsPlans,
    spcdCoveragePercentage,
    spcdTotalCost,

    -- ** SavingsPlansDetails
    SavingsPlansDetails,
    savingsPlansDetails,
    spdInstanceFamily,
    spdOfferingId,
    spdRegion,

    -- ** SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation,
    savingsPlansPurchaseRecommendation,
    spprSavingsPlansPurchaseRecommendationDetails,
    spprTermInYears,
    spprAccountScope,
    spprSavingsPlansType,
    spprLookbackPeriodInDays,
    spprPaymentOption,
    spprSavingsPlansPurchaseRecommendationSummary,

    -- ** SavingsPlansPurchaseRecommendationDetail
    SavingsPlansPurchaseRecommendationDetail,
    savingsPlansPurchaseRecommendationDetail,
    spprdCurrencyCode,
    spprdCurrentAverageHourlyOnDemandSpend,
    spprdSavingsPlansDetails,
    spprdCurrentMinimumHourlyOnDemandSpend,
    spprdEstimatedROI,
    spprdCurrentMaximumHourlyOnDemandSpend,
    spprdEstimatedSavingsAmount,
    spprdAccountId,
    spprdEstimatedMonthlySavingsAmount,
    spprdEstimatedOnDemandCost,
    spprdEstimatedOnDemandCostWithCurrentCommitment,
    spprdUpfrontCost,
    spprdEstimatedSPCost,
    spprdEstimatedSavingsPercentage,
    spprdEstimatedAverageUtilization,
    spprdHourlyCommitmentToPurchase,

    -- ** SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata,
    savingsPlansPurchaseRecommendationMetadata,
    spprmRecommendationId,
    spprmGenerationTimestamp,
    spprmAdditionalMetadata,

    -- ** SavingsPlansPurchaseRecommendationSummary
    SavingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendationSummary,
    spprsCurrencyCode,
    spprsDailyCommitmentToPurchase,
    spprsEstimatedTotalCost,
    spprsEstimatedROI,
    spprsEstimatedSavingsAmount,
    spprsEstimatedMonthlySavingsAmount,
    spprsEstimatedOnDemandCostWithCurrentCommitment,
    spprsEstimatedSavingsPercentage,
    spprsTotalRecommendationCount,
    spprsCurrentOnDemandSpend,
    spprsHourlyCommitmentToPurchase,

    -- ** SavingsPlansSavings
    SavingsPlansSavings,
    savingsPlansSavings,
    spsNetSavings,
    spsOnDemandCostEquivalent,

    -- ** SavingsPlansUtilization
    SavingsPlansUtilization,
    savingsPlansUtilization,
    spuUnusedCommitment,
    spuUtilizationPercentage,
    spuTotalCommitment,
    spuUsedCommitment,

    -- ** SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates,
    savingsPlansUtilizationAggregates,
    spuaAmortizedCommitment,
    spuaSavings,
    spuaUtilization,

    -- ** SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime,
    savingsPlansUtilizationByTime,
    spubtAmortizedCommitment,
    spubtSavings,
    spubtTimePeriod,
    spubtUtilization,

    -- ** SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail,
    savingsPlansUtilizationDetail,
    spudAmortizedCommitment,
    spudSavings,
    spudAttributes,
    spudUtilization,
    spudSavingsPlanARN,

    -- ** ServiceSpecification
    ServiceSpecification,
    serviceSpecification,
    ssEC2Specification,

    -- ** Subscriber
    Subscriber,
    subscriber,
    sStatus,
    sAddress,
    sType,

    -- ** TagValues
    TagValues,
    tagValues,
    tvValues,
    tvKey,
    tvMatchOptions,

    -- ** TargetInstance
    TargetInstance,
    targetInstance,
    tiCurrencyCode,
    tiResourceDetails,
    tiDefaultTargetInstance,
    tiEstimatedMonthlyCost,
    tiEstimatedMonthlySavings,
    tiExpectedResourceUtilization,

    -- ** TerminateRecommendationDetail
    TerminateRecommendationDetail,
    terminateRecommendationDetail,
    trdCurrencyCode,
    trdEstimatedMonthlySavings,

    -- ** TotalImpactFilter
    TotalImpactFilter,
    totalImpactFilter,
    tifEndValue,
    tifNumericOperator,
    tifStartValue,

    -- ** UtilizationByTime
    UtilizationByTime,
    utilizationByTime,
    ubtGroups,
    ubtTimePeriod,
    ubtTotal,
  )
where

import Network.AWS.CostExplorer.CreateAnomalyMonitor
import Network.AWS.CostExplorer.CreateAnomalySubscription
import Network.AWS.CostExplorer.CreateCostCategoryDefinition
import Network.AWS.CostExplorer.DeleteAnomalyMonitor
import Network.AWS.CostExplorer.DeleteAnomalySubscription
import Network.AWS.CostExplorer.DeleteCostCategoryDefinition
import Network.AWS.CostExplorer.DescribeCostCategoryDefinition
import Network.AWS.CostExplorer.GetAnomalies
import Network.AWS.CostExplorer.GetAnomalyMonitors
import Network.AWS.CostExplorer.GetAnomalySubscriptions
import Network.AWS.CostExplorer.GetCostAndUsage
import Network.AWS.CostExplorer.GetCostAndUsageWithResources
import Network.AWS.CostExplorer.GetCostForecast
import Network.AWS.CostExplorer.GetDimensionValues
import Network.AWS.CostExplorer.GetReservationCoverage
import Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
import Network.AWS.CostExplorer.GetReservationUtilization
import Network.AWS.CostExplorer.GetRightsizingRecommendation
import Network.AWS.CostExplorer.GetSavingsPlansCoverage
import Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
import Network.AWS.CostExplorer.GetSavingsPlansUtilization
import Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
import Network.AWS.CostExplorer.GetTags
import Network.AWS.CostExplorer.GetUsageForecast
import Network.AWS.CostExplorer.ListCostCategoryDefinitions
import Network.AWS.CostExplorer.ProvideAnomalyFeedback
import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.UpdateAnomalyMonitor
import Network.AWS.CostExplorer.UpdateAnomalySubscription
import Network.AWS.CostExplorer.UpdateCostCategoryDefinition
import Network.AWS.CostExplorer.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CostExplorer'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
