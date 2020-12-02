{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types
  ( -- * Service Configuration
    costExplorer,

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

    -- * AnomalyDateInterval
    AnomalyDateInterval,
    anomalyDateInterval,
    adiEndDate,
    adiStartDate,

    -- * AnomalyMonitor
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

    -- * AnomalyScore
    AnomalyScore,
    anomalyScore,
    asMaxScore,
    asCurrentScore,

    -- * AnomalySubscription
    AnomalySubscription,
    anomalySubscription,
    asAccountId,
    asSubscriptionARN,
    asMonitorARNList,
    asSubscribers,
    asThreshold,
    asFrequency,
    asSubscriptionName,

    -- * CostCategory
    CostCategory,
    costCategory,
    ccProcessingStatus,
    ccEffectiveEnd,
    ccCostCategoryARN,
    ccEffectiveStart,
    ccName,
    ccRuleVersion,
    ccRules,

    -- * CostCategoryProcessingStatus
    CostCategoryProcessingStatus,
    costCategoryProcessingStatus,
    ccpsStatus,
    ccpsComponent,

    -- * CostCategoryReference
    CostCategoryReference,
    costCategoryReference,
    ccrEffectiveStart,
    ccrValues,
    ccrCostCategoryARN,
    ccrProcessingStatus,
    ccrNumberOfRules,
    ccrName,
    ccrEffectiveEnd,

    -- * CostCategoryRule
    CostCategoryRule,
    costCategoryRule,
    ccrValue,
    ccrRule,

    -- * CostCategoryValues
    CostCategoryValues,
    costCategoryValues,
    ccvValues,
    ccvKey,
    ccvMatchOptions,

    -- * Coverage
    Coverage,
    coverage,
    cCoverageNormalizedUnits,
    cCoverageHours,
    cCoverageCost,

    -- * CoverageByTime
    CoverageByTime,
    coverageByTime,
    cbtGroups,
    cbtTimePeriod,
    cbtTotal,

    -- * CoverageCost
    CoverageCost,
    coverageCost,
    ccOnDemandCost,

    -- * CoverageHours
    CoverageHours,
    coverageHours,
    chCoverageHoursPercentage,
    chOnDemandHours,
    chTotalRunningHours,
    chReservedHours,

    -- * CoverageNormalizedUnits
    CoverageNormalizedUnits,
    coverageNormalizedUnits,
    cnuReservedNormalizedUnits,
    cnuTotalRunningNormalizedUnits,
    cnuCoverageNormalizedUnitsPercentage,
    cnuOnDemandNormalizedUnits,

    -- * CurrentInstance
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

    -- * DateInterval
    DateInterval,
    dateInterval,
    diStart,
    diEnd,

    -- * DimensionValues
    DimensionValues,
    dimensionValues,
    dvValues,
    dvKey,
    dvMatchOptions,

    -- * DimensionValuesWithAttributes
    DimensionValuesWithAttributes,
    dimensionValuesWithAttributes,
    dvwaValue,
    dvwaAttributes,

    -- * EBSResourceUtilization
    EBSResourceUtilization,
    ebsResourceUtilization,
    eruEBSWriteBytesPerSecond,
    eruEBSWriteOpsPerSecond,
    eruEBSReadOpsPerSecond,
    eruEBSReadBytesPerSecond,

    -- * EC2InstanceDetails
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

    -- * EC2ResourceDetails
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

    -- * EC2ResourceUtilization
    EC2ResourceUtilization,
    ec2ResourceUtilization,
    eruMaxCPUUtilizationPercentage,
    eruEBSResourceUtilization,
    eruMaxStorageUtilizationPercentage,
    eruMaxMemoryUtilizationPercentage,

    -- * EC2Specification
    EC2Specification,
    ec2Specification,
    esOfferingClass,

    -- * ESInstanceDetails
    ESInstanceDetails,
    eSInstanceDetails,
    esidCurrentGeneration,
    esidInstanceClass,
    esidInstanceSize,
    esidSizeFlexEligible,
    esidRegion,

    -- * ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails,
    elastiCacheInstanceDetails,
    ecidCurrentGeneration,
    ecidProductDescription,
    ecidFamily,
    ecidSizeFlexEligible,
    ecidRegion,
    ecidNodeType,

    -- * Expression
    Expression,
    expression,
    eNot,
    eAnd,
    eOr,
    eCostCategories,
    eDimensions,
    eTags,

    -- * ForecastResult
    ForecastResult,
    forecastResult,
    frTimePeriod,
    frMeanValue,
    frPredictionIntervalUpperBound,
    frPredictionIntervalLowerBound,

    -- * Group
    Group,
    group',
    gMetrics,
    gKeys,

    -- * GroupDefinition
    GroupDefinition,
    groupDefinition,
    gdKey,
    gdType,

    -- * Impact
    Impact,
    impact,
    iTotalImpact,
    iMaxImpact,

    -- * InstanceDetails
    InstanceDetails,
    instanceDetails,
    idESInstanceDetails,
    idRDSInstanceDetails,
    idElastiCacheInstanceDetails,
    idEC2InstanceDetails,
    idRedshiftInstanceDetails,

    -- * MetricValue
    MetricValue,
    metricValue,
    mvAmount,
    mvUnit,

    -- * ModifyRecommendationDetail
    ModifyRecommendationDetail,
    modifyRecommendationDetail,
    mrdTargetInstances,

    -- * RDSInstanceDetails
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

    -- * RedshiftInstanceDetails
    RedshiftInstanceDetails,
    redshiftInstanceDetails,
    rCurrentGeneration,
    rFamily,
    rSizeFlexEligible,
    rRegion,
    rNodeType,

    -- * ReservationAggregates
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

    -- * ReservationCoverageGroup
    ReservationCoverageGroup,
    reservationCoverageGroup,
    rcgCoverage,
    rcgAttributes,

    -- * ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation,
    reservationPurchaseRecommendation,
    rprTermInYears,
    rprRecommendationSummary,
    rprServiceSpecification,
    rprAccountScope,
    rprRecommendationDetails,
    rprLookbackPeriodInDays,
    rprPaymentOption,

    -- * ReservationPurchaseRecommendationDetail
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

    -- * ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata,
    reservationPurchaseRecommendationMetadata,
    rprmRecommendationId,
    rprmGenerationTimestamp,

    -- * ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary,
    reservationPurchaseRecommendationSummary,
    rprsCurrencyCode,
    rprsTotalEstimatedMonthlySavingsPercentage,
    rprsTotalEstimatedMonthlySavingsAmount,

    -- * ReservationUtilizationGroup
    ReservationUtilizationGroup,
    reservationUtilizationGroup,
    rugValue,
    rugKey,
    rugAttributes,
    rugUtilization,

    -- * ResourceDetails
    ResourceDetails,
    resourceDetails,
    rdEC2ResourceDetails,

    -- * ResourceUtilization
    ResourceUtilization,
    resourceUtilization,
    ruEC2ResourceUtilization,

    -- * ResultByTime
    ResultByTime,
    resultByTime,
    rbtGroups,
    rbtTimePeriod,
    rbtTotal,
    rbtEstimated,

    -- * RightsizingRecommendation
    RightsizingRecommendation,
    rightsizingRecommendation,
    rrAccountId,
    rrModifyRecommendationDetail,
    rrCurrentInstance,
    rrRightsizingType,
    rrTerminateRecommendationDetail,

    -- * RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration,
    rightsizingRecommendationConfiguration,
    rrcRecommendationTarget,
    rrcBenefitsConsidered,

    -- * RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata,
    rightsizingRecommendationMetadata,
    rrmRecommendationId,
    rrmGenerationTimestamp,
    rrmLookbackPeriodInDays,

    -- * RightsizingRecommendationSummary
    RightsizingRecommendationSummary,
    rightsizingRecommendationSummary,
    rrsSavingsPercentage,
    rrsSavingsCurrencyCode,
    rrsTotalRecommendationCount,
    rrsEstimatedTotalMonthlySavingsAmount,

    -- * RootCause
    RootCause,
    rootCause,
    rcService,
    rcUsageType,
    rcLinkedAccount,
    rcRegion,

    -- * SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment,
    savingsPlansAmortizedCommitment,
    spacAmortizedUpfrontCommitment,
    spacTotalAmortizedCommitment,
    spacAmortizedRecurringCommitment,

    -- * SavingsPlansCoverage
    SavingsPlansCoverage,
    savingsPlansCoverage,
    spcTimePeriod,
    spcCoverage,
    spcAttributes,

    -- * SavingsPlansCoverageData
    SavingsPlansCoverageData,
    savingsPlansCoverageData,
    spcdOnDemandCost,
    spcdSpendCoveredBySavingsPlans,
    spcdCoveragePercentage,
    spcdTotalCost,

    -- * SavingsPlansDetails
    SavingsPlansDetails,
    savingsPlansDetails,
    spdInstanceFamily,
    spdOfferingId,
    spdRegion,

    -- * SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation,
    savingsPlansPurchaseRecommendation,
    spprSavingsPlansPurchaseRecommendationDetails,
    spprTermInYears,
    spprAccountScope,
    spprSavingsPlansType,
    spprLookbackPeriodInDays,
    spprPaymentOption,
    spprSavingsPlansPurchaseRecommendationSummary,

    -- * SavingsPlansPurchaseRecommendationDetail
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

    -- * SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata,
    savingsPlansPurchaseRecommendationMetadata,
    spprmRecommendationId,
    spprmGenerationTimestamp,
    spprmAdditionalMetadata,

    -- * SavingsPlansPurchaseRecommendationSummary
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

    -- * SavingsPlansSavings
    SavingsPlansSavings,
    savingsPlansSavings,
    spsNetSavings,
    spsOnDemandCostEquivalent,

    -- * SavingsPlansUtilization
    SavingsPlansUtilization,
    savingsPlansUtilization,
    spuUnusedCommitment,
    spuUtilizationPercentage,
    spuTotalCommitment,
    spuUsedCommitment,

    -- * SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates,
    savingsPlansUtilizationAggregates,
    spuaAmortizedCommitment,
    spuaSavings,
    spuaUtilization,

    -- * SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime,
    savingsPlansUtilizationByTime,
    spubtAmortizedCommitment,
    spubtSavings,
    spubtTimePeriod,
    spubtUtilization,

    -- * SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail,
    savingsPlansUtilizationDetail,
    spudAmortizedCommitment,
    spudSavings,
    spudAttributes,
    spudUtilization,
    spudSavingsPlanARN,

    -- * ServiceSpecification
    ServiceSpecification,
    serviceSpecification,
    ssEC2Specification,

    -- * Subscriber
    Subscriber,
    subscriber,
    sStatus,
    sAddress,
    sType,

    -- * TagValues
    TagValues,
    tagValues,
    tvValues,
    tvKey,
    tvMatchOptions,

    -- * TargetInstance
    TargetInstance,
    targetInstance,
    tiCurrencyCode,
    tiResourceDetails,
    tiDefaultTargetInstance,
    tiEstimatedMonthlyCost,
    tiEstimatedMonthlySavings,
    tiExpectedResourceUtilization,

    -- * TerminateRecommendationDetail
    TerminateRecommendationDetail,
    terminateRecommendationDetail,
    trdCurrencyCode,
    trdEstimatedMonthlySavings,

    -- * TotalImpactFilter
    TotalImpactFilter,
    totalImpactFilter,
    tifEndValue,
    tifNumericOperator,
    tifStartValue,

    -- * UtilizationByTime
    UtilizationByTime,
    utilizationByTime,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-10-25@ of the Amazon Cost Explorer Service SDK configuration.
costExplorer :: Service
costExplorer =
  Service
    { _svcAbbrev = "CostExplorer",
      _svcSigner = v4,
      _svcPrefix = "ce",
      _svcVersion = "2017-10-25",
      _svcEndpoint = defaultEndpoint costExplorer,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CostExplorer",
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
