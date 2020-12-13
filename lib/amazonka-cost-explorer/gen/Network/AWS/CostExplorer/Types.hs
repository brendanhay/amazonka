-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types
  ( -- * Service configuration
    costExplorerService,

    -- * Errors

    -- * AccountScope
    AccountScope (..),

    -- * AnomalyFeedbackType
    AnomalyFeedbackType (..),

    -- * AnomalySubscriptionFrequency
    AnomalySubscriptionFrequency (..),

    -- * Context
    Context (..),

    -- * CostCategoryRuleVersion
    CostCategoryRuleVersion (..),

    -- * CostCategoryStatus
    CostCategoryStatus (..),

    -- * CostCategoryStatusComponent
    CostCategoryStatusComponent (..),

    -- * Dimension
    Dimension (..),

    -- * Granularity
    Granularity (..),

    -- * GroupDefinitionType
    GroupDefinitionType (..),

    -- * LookbackPeriodInDays
    LookbackPeriodInDays (..),

    -- * MatchOption
    MatchOption (..),

    -- * Metric
    Metric (..),

    -- * MonitorDimension
    MonitorDimension (..),

    -- * MonitorType
    MonitorType (..),

    -- * NumericOperator
    NumericOperator (..),

    -- * OfferingClass
    OfferingClass (..),

    -- * PaymentOption
    PaymentOption (..),

    -- * RecommendationTarget
    RecommendationTarget (..),

    -- * RightsizingType
    RightsizingType (..),

    -- * SubscriberStatus
    SubscriberStatus (..),

    -- * SubscriberType
    SubscriberType (..),

    -- * SupportedSavingsPlansType
    SupportedSavingsPlansType (..),

    -- * TermInYears
    TermInYears (..),

    -- * Anomaly
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

    -- * AnomalyDateInterval
    AnomalyDateInterval (..),
    mkAnomalyDateInterval,
    adiEndDate,
    adiStartDate,

    -- * AnomalyMonitor
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

    -- * AnomalyScore
    AnomalyScore (..),
    mkAnomalyScore,
    asMaxScore,
    asCurrentScore,

    -- * AnomalySubscription
    AnomalySubscription (..),
    mkAnomalySubscription,
    asSubscriptionName,
    asFrequency,
    asAccountId,
    asThreshold,
    asMonitorARNList,
    asSubscriptionARN,
    asSubscribers,

    -- * CostCategory
    CostCategory (..),
    mkCostCategory,
    ccRuleVersion,
    ccRules,
    ccEffectiveStart,
    ccCostCategoryARN,
    ccProcessingStatus,
    ccName,
    ccEffectiveEnd,

    -- * CostCategoryProcessingStatus
    CostCategoryProcessingStatus (..),
    mkCostCategoryProcessingStatus,
    ccpsStatus,
    ccpsComponent,

    -- * CostCategoryReference
    CostCategoryReference (..),
    mkCostCategoryReference,
    ccrEffectiveStart,
    ccrValues,
    ccrCostCategoryARN,
    ccrProcessingStatus,
    ccrNumberOfRules,
    ccrName,
    ccrEffectiveEnd,

    -- * CostCategoryRule
    CostCategoryRule (..),
    mkCostCategoryRule,
    ccrValue,
    ccrRule,

    -- * CostCategoryValues
    CostCategoryValues (..),
    mkCostCategoryValues,
    ccvValues,
    ccvKey,
    ccvMatchOptions,

    -- * Coverage
    Coverage (..),
    mkCoverage,
    cCoverageNormalizedUnits,
    cCoverageHours,
    cCoverageCost,

    -- * CoverageByTime
    CoverageByTime (..),
    mkCoverageByTime,
    cbtGroups,
    cbtTimePeriod,
    cbtTotal,

    -- * CoverageCost
    CoverageCost (..),
    mkCoverageCost,
    ccOnDemandCost,

    -- * CoverageHours
    CoverageHours (..),
    mkCoverageHours,
    chCoverageHoursPercentage,
    chOnDemandHours,
    chTotalRunningHours,
    chReservedHours,

    -- * CoverageNormalizedUnits
    CoverageNormalizedUnits (..),
    mkCoverageNormalizedUnits,
    cnuReservedNormalizedUnits,
    cnuTotalRunningNormalizedUnits,
    cnuCoverageNormalizedUnitsPercentage,
    cnuOnDemandNormalizedUnits,

    -- * CurrentInstance
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

    -- * DateInterval
    DateInterval (..),
    mkDateInterval,
    diStart,
    diEnd,

    -- * DimensionValues
    DimensionValues (..),
    mkDimensionValues,
    dvValues,
    dvKey,
    dvMatchOptions,

    -- * DimensionValuesWithAttributes
    DimensionValuesWithAttributes (..),
    mkDimensionValuesWithAttributes,
    dvwaValue,
    dvwaAttributes,

    -- * EBSResourceUtilization
    EBSResourceUtilization (..),
    mkEBSResourceUtilization,
    eruEBSWriteBytesPerSecond,
    eruEBSWriteOpsPerSecond,
    eruEBSReadOpsPerSecond,
    eruEBSReadBytesPerSecond,

    -- * EC2InstanceDetails
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

    -- * EC2ResourceDetails
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

    -- * EC2ResourceUtilization
    EC2ResourceUtilization (..),
    mkEC2ResourceUtilization,
    eruMaxCPUUtilizationPercentage,
    eruEBSResourceUtilization,
    eruMaxStorageUtilizationPercentage,
    eruMaxMemoryUtilizationPercentage,

    -- * EC2Specification
    EC2Specification (..),
    mkEC2Specification,
    esOfferingClass,

    -- * ESInstanceDetails
    ESInstanceDetails (..),
    mkESInstanceDetails,
    esidCurrentGeneration,
    esidInstanceClass,
    esidInstanceSize,
    esidSizeFlexEligible,
    esidRegion,

    -- * ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails (..),
    mkElastiCacheInstanceDetails,
    ecidCurrentGeneration,
    ecidProductDescription,
    ecidFamily,
    ecidSizeFlexEligible,
    ecidRegion,
    ecidNodeType,

    -- * Expression
    Expression (..),
    mkExpression,
    eNot,
    eAnd,
    eOr,
    eCostCategories,
    eDimensions,
    eTags,

    -- * ForecastResult
    ForecastResult (..),
    mkForecastResult,
    frTimePeriod,
    frMeanValue,
    frPredictionIntervalUpperBound,
    frPredictionIntervalLowerBound,

    -- * Group
    Group (..),
    mkGroup,
    gMetrics,
    gKeys,

    -- * GroupDefinition
    GroupDefinition (..),
    mkGroupDefinition,
    gdKey,
    gdType,

    -- * Impact
    Impact (..),
    mkImpact,
    iTotalImpact,
    iMaxImpact,

    -- * InstanceDetails
    InstanceDetails (..),
    mkInstanceDetails,
    idESInstanceDetails,
    idRDSInstanceDetails,
    idElastiCacheInstanceDetails,
    idEC2InstanceDetails,
    idRedshiftInstanceDetails,

    -- * MetricValue
    MetricValue (..),
    mkMetricValue,
    mvAmount,
    mvUnit,

    -- * ModifyRecommendationDetail
    ModifyRecommendationDetail (..),
    mkModifyRecommendationDetail,
    mrdTargetInstances,

    -- * RDSInstanceDetails
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

    -- * RedshiftInstanceDetails
    RedshiftInstanceDetails (..),
    mkRedshiftInstanceDetails,
    rCurrentGeneration,
    rFamily,
    rSizeFlexEligible,
    rRegion,
    rNodeType,

    -- * ReservationAggregates
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

    -- * ReservationCoverageGroup
    ReservationCoverageGroup (..),
    mkReservationCoverageGroup,
    rcgCoverage,
    rcgAttributes,

    -- * ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation (..),
    mkReservationPurchaseRecommendation,
    rprTermInYears,
    rprRecommendationSummary,
    rprServiceSpecification,
    rprAccountScope,
    rprRecommendationDetails,
    rprLookbackPeriodInDays,
    rprPaymentOption,

    -- * ReservationPurchaseRecommendationDetail
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

    -- * ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata (..),
    mkReservationPurchaseRecommendationMetadata,
    rprmRecommendationId,
    rprmGenerationTimestamp,

    -- * ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary (..),
    mkReservationPurchaseRecommendationSummary,
    rprsCurrencyCode,
    rprsTotalEstimatedMonthlySavingsPercentage,
    rprsTotalEstimatedMonthlySavingsAmount,

    -- * ReservationUtilizationGroup
    ReservationUtilizationGroup (..),
    mkReservationUtilizationGroup,
    rugValue,
    rugKey,
    rugAttributes,
    rugUtilization,

    -- * ResourceDetails
    ResourceDetails (..),
    mkResourceDetails,
    rdEC2ResourceDetails,

    -- * ResourceUtilization
    ResourceUtilization (..),
    mkResourceUtilization,
    ruEC2ResourceUtilization,

    -- * ResultByTime
    ResultByTime (..),
    mkResultByTime,
    rbtGroups,
    rbtTimePeriod,
    rbtTotal,
    rbtEstimated,

    -- * RightsizingRecommendation
    RightsizingRecommendation (..),
    mkRightsizingRecommendation,
    rrAccountId,
    rrModifyRecommendationDetail,
    rrCurrentInstance,
    rrRightsizingType,
    rrTerminateRecommendationDetail,

    -- * RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration (..),
    mkRightsizingRecommendationConfiguration,
    rrcRecommendationTarget,
    rrcBenefitsConsidered,

    -- * RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata (..),
    mkRightsizingRecommendationMetadata,
    rrmRecommendationId,
    rrmGenerationTimestamp,
    rrmLookbackPeriodInDays,

    -- * RightsizingRecommendationSummary
    RightsizingRecommendationSummary (..),
    mkRightsizingRecommendationSummary,
    rrsSavingsPercentage,
    rrsSavingsCurrencyCode,
    rrsTotalRecommendationCount,
    rrsEstimatedTotalMonthlySavingsAmount,

    -- * RootCause
    RootCause (..),
    mkRootCause,
    rcService,
    rcUsageType,
    rcLinkedAccount,
    rcRegion,

    -- * SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment (..),
    mkSavingsPlansAmortizedCommitment,
    spacAmortizedUpfrontCommitment,
    spacTotalAmortizedCommitment,
    spacAmortizedRecurringCommitment,

    -- * SavingsPlansCoverage
    SavingsPlansCoverage (..),
    mkSavingsPlansCoverage,
    spcTimePeriod,
    spcCoverage,
    spcAttributes,

    -- * SavingsPlansCoverageData
    SavingsPlansCoverageData (..),
    mkSavingsPlansCoverageData,
    spcdOnDemandCost,
    spcdSpendCoveredBySavingsPlans,
    spcdCoveragePercentage,
    spcdTotalCost,

    -- * SavingsPlansDetails
    SavingsPlansDetails (..),
    mkSavingsPlansDetails,
    spdInstanceFamily,
    spdOfferingId,
    spdRegion,

    -- * SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation (..),
    mkSavingsPlansPurchaseRecommendation,
    spprSavingsPlansPurchaseRecommendationDetails,
    spprTermInYears,
    spprAccountScope,
    spprSavingsPlansType,
    spprLookbackPeriodInDays,
    spprPaymentOption,
    spprSavingsPlansPurchaseRecommendationSummary,

    -- * SavingsPlansPurchaseRecommendationDetail
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

    -- * SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata (..),
    mkSavingsPlansPurchaseRecommendationMetadata,
    spprmRecommendationId,
    spprmGenerationTimestamp,
    spprmAdditionalMetadata,

    -- * SavingsPlansPurchaseRecommendationSummary
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

    -- * SavingsPlansSavings
    SavingsPlansSavings (..),
    mkSavingsPlansSavings,
    spsNetSavings,
    spsOnDemandCostEquivalent,

    -- * SavingsPlansUtilization
    SavingsPlansUtilization (..),
    mkSavingsPlansUtilization,
    spuUnusedCommitment,
    spuUtilizationPercentage,
    spuTotalCommitment,
    spuUsedCommitment,

    -- * SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates (..),
    mkSavingsPlansUtilizationAggregates,
    spuaAmortizedCommitment,
    spuaSavings,
    spuaUtilization,

    -- * SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime (..),
    mkSavingsPlansUtilizationByTime,
    spubtAmortizedCommitment,
    spubtTimePeriod,
    spubtSavings,
    spubtUtilization,

    -- * SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail (..),
    mkSavingsPlansUtilizationDetail,
    spudAmortizedCommitment,
    spudSavings,
    spudAttributes,
    spudUtilization,
    spudSavingsPlanARN,

    -- * ServiceSpecification
    ServiceSpecification (..),
    mkServiceSpecification,
    ssEC2Specification,

    -- * Subscriber
    Subscriber (..),
    mkSubscriber,
    sStatus,
    sAddress,
    sType,

    -- * TagValues
    TagValues (..),
    mkTagValues,
    tvValues,
    tvKey,
    tvMatchOptions,

    -- * TargetInstance
    TargetInstance (..),
    mkTargetInstance,
    tiCurrencyCode,
    tiResourceDetails,
    tiDefaultTargetInstance,
    tiEstimatedMonthlyCost,
    tiEstimatedMonthlySavings,
    tiExpectedResourceUtilization,

    -- * TerminateRecommendationDetail
    TerminateRecommendationDetail (..),
    mkTerminateRecommendationDetail,
    trdCurrencyCode,
    trdEstimatedMonthlySavings,

    -- * TotalImpactFilter
    TotalImpactFilter (..),
    mkTotalImpactFilter,
    tifEndValue,
    tifStartValue,
    tifNumericOperator,

    -- * UtilizationByTime
    UtilizationByTime (..),
    mkUtilizationByTime,
    ubtGroups,
    ubtTimePeriod,
    ubtTotal,
  )
where

import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.Anomaly
import Network.AWS.CostExplorer.Types.AnomalyDateInterval
import Network.AWS.CostExplorer.Types.AnomalyFeedbackType
import Network.AWS.CostExplorer.Types.AnomalyMonitor
import Network.AWS.CostExplorer.Types.AnomalyScore
import Network.AWS.CostExplorer.Types.AnomalySubscription
import Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
import Network.AWS.CostExplorer.Types.Context
import Network.AWS.CostExplorer.Types.CostCategory
import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import Network.AWS.CostExplorer.Types.CostCategoryReference
import Network.AWS.CostExplorer.Types.CostCategoryRule
import Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
import Network.AWS.CostExplorer.Types.CostCategoryStatus
import Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.CoverageByTime
import Network.AWS.CostExplorer.Types.CoverageCost
import Network.AWS.CostExplorer.Types.CoverageHours
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.Dimension
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import Network.AWS.CostExplorer.Types.EC2Specification
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.ForecastResult
import Network.AWS.CostExplorer.Types.Granularity
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.GroupDefinition
import Network.AWS.CostExplorer.Types.GroupDefinitionType
import Network.AWS.CostExplorer.Types.Impact
import Network.AWS.CostExplorer.Types.InstanceDetails
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.MatchOption
import Network.AWS.CostExplorer.Types.Metric
import Network.AWS.CostExplorer.Types.MetricValue
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.MonitorDimension
import Network.AWS.CostExplorer.Types.MonitorType
import Network.AWS.CostExplorer.Types.NumericOperator
import Network.AWS.CostExplorer.Types.OfferingClass
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
import Network.AWS.CostExplorer.Types.RecommendationTarget
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
import Network.AWS.CostExplorer.Types.ReservationAggregates
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.ResultByTime
import Network.AWS.CostExplorer.Types.RightsizingRecommendation
import Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
import Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
import Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
import Network.AWS.CostExplorer.Types.RightsizingType
import Network.AWS.CostExplorer.Types.RootCause
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansCoverage
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
import Network.AWS.CostExplorer.Types.SavingsPlansDetails
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
import Network.AWS.CostExplorer.Types.ServiceSpecification
import Network.AWS.CostExplorer.Types.Subscriber
import Network.AWS.CostExplorer.Types.SubscriberStatus
import Network.AWS.CostExplorer.Types.SubscriberType
import Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
import Network.AWS.CostExplorer.Types.TagValues
import Network.AWS.CostExplorer.Types.TargetInstance
import Network.AWS.CostExplorer.Types.TermInYears
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import Network.AWS.CostExplorer.Types.TotalImpactFilter
import Network.AWS.CostExplorer.Types.UtilizationByTime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-25@ of the Amazon Cost Explorer Service SDK configuration.
costExplorerService :: Lude.Service
costExplorerService =
  Lude.Service
    { Lude._svcAbbrev = "CostExplorer",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "ce",
      Lude._svcVersion = "2017-10-25",
      Lude._svcEndpoint = Lude.defaultEndpoint costExplorerService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CostExplorer",
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
      | Lude.otherwise = Lude.Nothing
