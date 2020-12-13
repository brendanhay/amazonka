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
-- Service Endpoint
-- The Cost Explorer API provides the following endpoint:
--
--     * @https://ce.us-east-1.amazonaws.com@
--
--
-- For information about costs associated with the Cost Explorer API, see <http://aws.amazon.com/aws-cost-management/pricing/ AWS Cost Management Pricing> .
module Network.AWS.CostExplorer
  ( -- * Service configuration
    costExplorerService,

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
    Anomaly (..),
    mkAnomaly,
    aAnomalyStartDate,
    aDimensionValue,
    aAnomalyId,
    aRootCauses,
    aAnomalyEndDate,
    aImpact,
    aAnomalyScore,
    aFeedback,
    aMonitorARN,

    -- ** AnomalyDateInterval
    AnomalyDateInterval (..),
    mkAnomalyDateInterval,
    adiEndDate,
    adiStartDate,

    -- ** AnomalyMonitor
    AnomalyMonitor (..),
    mkAnomalyMonitor,
    amMonitorType,
    amDimensionalValueCount,
    amMonitorName,
    amMonitorSpecification,
    amMonitorDimension,
    amCreationDate,
    amLastUpdatedDate,
    amLastEvaluatedDate,
    amMonitorARN,

    -- ** AnomalyScore
    AnomalyScore (..),
    mkAnomalyScore,
    asMaxScore,
    asCurrentScore,

    -- ** AnomalySubscription
    AnomalySubscription (..),
    mkAnomalySubscription,
    asSubscriptionName,
    asFrequency,
    asAccountId,
    asThreshold,
    asMonitorARNList,
    asSubscriptionARN,
    asSubscribers,

    -- ** CostCategory
    CostCategory (..),
    mkCostCategory,
    ccRuleVersion,
    ccRules,
    ccEffectiveStart,
    ccCostCategoryARN,
    ccProcessingStatus,
    ccName,
    ccEffectiveEnd,

    -- ** CostCategoryProcessingStatus
    CostCategoryProcessingStatus (..),
    mkCostCategoryProcessingStatus,
    ccpsStatus,
    ccpsComponent,

    -- ** CostCategoryReference
    CostCategoryReference (..),
    mkCostCategoryReference,
    ccrEffectiveStart,
    ccrValues,
    ccrCostCategoryARN,
    ccrProcessingStatus,
    ccrNumberOfRules,
    ccrName,
    ccrEffectiveEnd,

    -- ** CostCategoryRule
    CostCategoryRule (..),
    mkCostCategoryRule,
    ccrValue,
    ccrRule,

    -- ** CostCategoryValues
    CostCategoryValues (..),
    mkCostCategoryValues,
    ccvValues,
    ccvKey,
    ccvMatchOptions,

    -- ** Coverage
    Coverage (..),
    mkCoverage,
    cCoverageNormalizedUnits,
    cCoverageHours,
    cCoverageCost,

    -- ** CoverageByTime
    CoverageByTime (..),
    mkCoverageByTime,
    cbtGroups,
    cbtTimePeriod,
    cbtTotal,

    -- ** CoverageCost
    CoverageCost (..),
    mkCoverageCost,
    ccOnDemandCost,

    -- ** CoverageHours
    CoverageHours (..),
    mkCoverageHours,
    chCoverageHoursPercentage,
    chOnDemandHours,
    chTotalRunningHours,
    chReservedHours,

    -- ** CoverageNormalizedUnits
    CoverageNormalizedUnits (..),
    mkCoverageNormalizedUnits,
    cnuReservedNormalizedUnits,
    cnuTotalRunningNormalizedUnits,
    cnuCoverageNormalizedUnitsPercentage,
    cnuOnDemandNormalizedUnits,

    -- ** CurrentInstance
    CurrentInstance (..),
    mkCurrentInstance,
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
    DateInterval (..),
    mkDateInterval,
    diStart,
    diEnd,

    -- ** DimensionValues
    DimensionValues (..),
    mkDimensionValues,
    dvValues,
    dvKey,
    dvMatchOptions,

    -- ** DimensionValuesWithAttributes
    DimensionValuesWithAttributes (..),
    mkDimensionValuesWithAttributes,
    dvwaValue,
    dvwaAttributes,

    -- ** EBSResourceUtilization
    EBSResourceUtilization (..),
    mkEBSResourceUtilization,
    eruEBSWriteBytesPerSecond,
    eruEBSWriteOpsPerSecond,
    eruEBSReadOpsPerSecond,
    eruEBSReadBytesPerSecond,

    -- ** EC2InstanceDetails
    EC2InstanceDetails (..),
    mkEC2InstanceDetails,
    eidCurrentGeneration,
    eidPlatform,
    eidFamily,
    eidInstanceType,
    eidAvailabilityZone,
    eidSizeFlexEligible,
    eidTenancy,
    eidRegion,

    -- ** EC2ResourceDetails
    EC2ResourceDetails (..),
    mkEC2ResourceDetails,
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
    EC2ResourceUtilization (..),
    mkEC2ResourceUtilization,
    eruMaxCPUUtilizationPercentage,
    eruEBSResourceUtilization,
    eruMaxStorageUtilizationPercentage,
    eruMaxMemoryUtilizationPercentage,

    -- ** EC2Specification
    EC2Specification (..),
    mkEC2Specification,
    esOfferingClass,

    -- ** ESInstanceDetails
    ESInstanceDetails (..),
    mkESInstanceDetails,
    esidCurrentGeneration,
    esidInstanceClass,
    esidInstanceSize,
    esidSizeFlexEligible,
    esidRegion,

    -- ** ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails (..),
    mkElastiCacheInstanceDetails,
    ecidCurrentGeneration,
    ecidProductDescription,
    ecidFamily,
    ecidSizeFlexEligible,
    ecidRegion,
    ecidNodeType,

    -- ** Expression
    Expression (..),
    mkExpression,
    eNot,
    eAnd,
    eOr,
    eCostCategories,
    eDimensions,
    eTags,

    -- ** ForecastResult
    ForecastResult (..),
    mkForecastResult,
    frTimePeriod,
    frMeanValue,
    frPredictionIntervalUpperBound,
    frPredictionIntervalLowerBound,

    -- ** Group
    Group (..),
    mkGroup,
    gMetrics,
    gKeys,

    -- ** GroupDefinition
    GroupDefinition (..),
    mkGroupDefinition,
    gdKey,
    gdType,

    -- ** Impact
    Impact (..),
    mkImpact,
    iTotalImpact,
    iMaxImpact,

    -- ** InstanceDetails
    InstanceDetails (..),
    mkInstanceDetails,
    idESInstanceDetails,
    idRDSInstanceDetails,
    idElastiCacheInstanceDetails,
    idEC2InstanceDetails,
    idRedshiftInstanceDetails,

    -- ** MetricValue
    MetricValue (..),
    mkMetricValue,
    mvAmount,
    mvUnit,

    -- ** ModifyRecommendationDetail
    ModifyRecommendationDetail (..),
    mkModifyRecommendationDetail,
    mrdTargetInstances,

    -- ** RDSInstanceDetails
    RDSInstanceDetails (..),
    mkRDSInstanceDetails,
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
    RedshiftInstanceDetails (..),
    mkRedshiftInstanceDetails,
    rCurrentGeneration,
    rFamily,
    rSizeFlexEligible,
    rRegion,
    rNodeType,

    -- ** ReservationAggregates
    ReservationAggregates (..),
    mkReservationAggregates,
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
    ReservationCoverageGroup (..),
    mkReservationCoverageGroup,
    rcgCoverage,
    rcgAttributes,

    -- ** ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation (..),
    mkReservationPurchaseRecommendation,
    rprTermInYears,
    rprRecommendationSummary,
    rprServiceSpecification,
    rprAccountScope,
    rprRecommendationDetails,
    rprLookbackPeriodInDays,
    rprPaymentOption,

    -- ** ReservationPurchaseRecommendationDetail
    ReservationPurchaseRecommendationDetail (..),
    mkReservationPurchaseRecommendationDetail,
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
    ReservationPurchaseRecommendationMetadata (..),
    mkReservationPurchaseRecommendationMetadata,
    rprmRecommendationId,
    rprmGenerationTimestamp,

    -- ** ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary (..),
    mkReservationPurchaseRecommendationSummary,
    rprsCurrencyCode,
    rprsTotalEstimatedMonthlySavingsPercentage,
    rprsTotalEstimatedMonthlySavingsAmount,

    -- ** ReservationUtilizationGroup
    ReservationUtilizationGroup (..),
    mkReservationUtilizationGroup,
    rugValue,
    rugKey,
    rugAttributes,
    rugUtilization,

    -- ** ResourceDetails
    ResourceDetails (..),
    mkResourceDetails,
    rdEC2ResourceDetails,

    -- ** ResourceUtilization
    ResourceUtilization (..),
    mkResourceUtilization,
    ruEC2ResourceUtilization,

    -- ** ResultByTime
    ResultByTime (..),
    mkResultByTime,
    rbtGroups,
    rbtTimePeriod,
    rbtTotal,
    rbtEstimated,

    -- ** RightsizingRecommendation
    RightsizingRecommendation (..),
    mkRightsizingRecommendation,
    rrAccountId,
    rrModifyRecommendationDetail,
    rrCurrentInstance,
    rrRightsizingType,
    rrTerminateRecommendationDetail,

    -- ** RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration (..),
    mkRightsizingRecommendationConfiguration,
    rrcRecommendationTarget,
    rrcBenefitsConsidered,

    -- ** RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata (..),
    mkRightsizingRecommendationMetadata,
    rrmRecommendationId,
    rrmGenerationTimestamp,
    rrmLookbackPeriodInDays,

    -- ** RightsizingRecommendationSummary
    RightsizingRecommendationSummary (..),
    mkRightsizingRecommendationSummary,
    rrsSavingsPercentage,
    rrsSavingsCurrencyCode,
    rrsTotalRecommendationCount,
    rrsEstimatedTotalMonthlySavingsAmount,

    -- ** RootCause
    RootCause (..),
    mkRootCause,
    rcService,
    rcUsageType,
    rcLinkedAccount,
    rcRegion,

    -- ** SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment (..),
    mkSavingsPlansAmortizedCommitment,
    spacAmortizedUpfrontCommitment,
    spacTotalAmortizedCommitment,
    spacAmortizedRecurringCommitment,

    -- ** SavingsPlansCoverage
    SavingsPlansCoverage (..),
    mkSavingsPlansCoverage,
    spcTimePeriod,
    spcCoverage,
    spcAttributes,

    -- ** SavingsPlansCoverageData
    SavingsPlansCoverageData (..),
    mkSavingsPlansCoverageData,
    spcdOnDemandCost,
    spcdSpendCoveredBySavingsPlans,
    spcdCoveragePercentage,
    spcdTotalCost,

    -- ** SavingsPlansDetails
    SavingsPlansDetails (..),
    mkSavingsPlansDetails,
    spdInstanceFamily,
    spdOfferingId,
    spdRegion,

    -- ** SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation (..),
    mkSavingsPlansPurchaseRecommendation,
    spprSavingsPlansPurchaseRecommendationDetails,
    spprTermInYears,
    spprAccountScope,
    spprSavingsPlansType,
    spprLookbackPeriodInDays,
    spprPaymentOption,
    spprSavingsPlansPurchaseRecommendationSummary,

    -- ** SavingsPlansPurchaseRecommendationDetail
    SavingsPlansPurchaseRecommendationDetail (..),
    mkSavingsPlansPurchaseRecommendationDetail,
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
    SavingsPlansPurchaseRecommendationMetadata (..),
    mkSavingsPlansPurchaseRecommendationMetadata,
    spprmRecommendationId,
    spprmGenerationTimestamp,
    spprmAdditionalMetadata,

    -- ** SavingsPlansPurchaseRecommendationSummary
    SavingsPlansPurchaseRecommendationSummary (..),
    mkSavingsPlansPurchaseRecommendationSummary,
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
    SavingsPlansSavings (..),
    mkSavingsPlansSavings,
    spsNetSavings,
    spsOnDemandCostEquivalent,

    -- ** SavingsPlansUtilization
    SavingsPlansUtilization (..),
    mkSavingsPlansUtilization,
    spuUnusedCommitment,
    spuUtilizationPercentage,
    spuTotalCommitment,
    spuUsedCommitment,

    -- ** SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates (..),
    mkSavingsPlansUtilizationAggregates,
    spuaAmortizedCommitment,
    spuaSavings,
    spuaUtilization,

    -- ** SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime (..),
    mkSavingsPlansUtilizationByTime,
    spubtAmortizedCommitment,
    spubtTimePeriod,
    spubtSavings,
    spubtUtilization,

    -- ** SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail (..),
    mkSavingsPlansUtilizationDetail,
    spudAmortizedCommitment,
    spudSavings,
    spudAttributes,
    spudUtilization,
    spudSavingsPlanARN,

    -- ** ServiceSpecification
    ServiceSpecification (..),
    mkServiceSpecification,
    ssEC2Specification,

    -- ** Subscriber
    Subscriber (..),
    mkSubscriber,
    sStatus,
    sAddress,
    sType,

    -- ** TagValues
    TagValues (..),
    mkTagValues,
    tvValues,
    tvKey,
    tvMatchOptions,

    -- ** TargetInstance
    TargetInstance (..),
    mkTargetInstance,
    tiCurrencyCode,
    tiResourceDetails,
    tiDefaultTargetInstance,
    tiEstimatedMonthlyCost,
    tiEstimatedMonthlySavings,
    tiExpectedResourceUtilization,

    -- ** TerminateRecommendationDetail
    TerminateRecommendationDetail (..),
    mkTerminateRecommendationDetail,
    trdCurrencyCode,
    trdEstimatedMonthlySavings,

    -- ** TotalImpactFilter
    TotalImpactFilter (..),
    mkTotalImpactFilter,
    tifEndValue,
    tifStartValue,
    tifNumericOperator,

    -- ** UtilizationByTime
    UtilizationByTime (..),
    mkUtilizationByTime,
    ubtGroups,
    ubtTimePeriod,
    ubtTotal,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import qualified Network.AWS.Prelude as Lude

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
