-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _UnknownSubscriptionException
    , _BillExpirationException
    , _RequestChangedException
    , _UnresolvableUsageUnitException
    , _ServiceQuotaExceededException
    , _InvalidNextTokenException
    , _DataUnavailableException
    , _UnknownMonitorException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * PurchasedHours
    , PurchasedHours (..)

    -- * EC2ResourceUtilization
    , EC2ResourceUtilization (..)
    , mkEC2ResourceUtilization
    , ecruEBSResourceUtilization
    , ecruMaxCpuUtilizationPercentage
    , ecruMaxMemoryUtilizationPercentage
    , ecruMaxStorageUtilizationPercentage

    -- * EC2ResourceDetails
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

    -- * MonitorType
    , MonitorType (..)

    -- * RightsizingRecommendationMetadata
    , RightsizingRecommendationMetadata (..)
    , mkRightsizingRecommendationMetadata
    , rrmGenerationTimestamp
    , rrmLookbackPeriodInDays
    , rrmRecommendationId

    -- * NextPageToken
    , NextPageToken (..)

    -- * TotalActualHours
    , TotalActualHours (..)

    -- * Context
    , Context (..)

    -- * CostCategoryStatus
    , CostCategoryStatus (..)

    -- * Group
    , Group (..)
    , mkGroup
    , gKeys
    , gMetrics

    -- * SavingsPlansUtilizationAggregates
    , SavingsPlansUtilizationAggregates (..)
    , mkSavingsPlansUtilizationAggregates
    , spuaUtilization
    , spuaAmortizedCommitment
    , spuaSavings

    -- * ResultByTime
    , ResultByTime (..)
    , mkResultByTime
    , rbtEstimated
    , rbtGroups
    , rbtTimePeriod
    , rbtTotal

    -- * AttributeValue
    , AttributeValue (..)

    -- * UtilizationPercentage
    , UtilizationPercentage (..)

    -- * CoverageNormalizedUnits
    , CoverageNormalizedUnits (..)
    , mkCoverageNormalizedUnits
    , cnuCoverageNormalizedUnitsPercentage
    , cnuOnDemandNormalizedUnits
    , cnuReservedNormalizedUnits
    , cnuTotalRunningNormalizedUnits

    -- * ForecastResult
    , ForecastResult (..)
    , mkForecastResult
    , frMeanValue
    , frPredictionIntervalLowerBound
    , frPredictionIntervalUpperBound
    , frTimePeriod

    -- * ReservationAggregates
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

    -- * TagValues
    , TagValues (..)
    , mkTagValues
    , tvKey
    , tvMatchOptions
    , tvValues

    -- * GroupDefinition
    , GroupDefinition (..)
    , mkGroupDefinition
    , gdKey
    , gdType

    -- * AnomalyDateInterval
    , AnomalyDateInterval (..)
    , mkAnomalyDateInterval
    , adiStartDate
    , adiEndDate

    -- * Subscriber
    , Subscriber (..)
    , mkSubscriber
    , sAddress
    , sStatus
    , sType

    -- * ReservationPurchaseRecommendation
    , ReservationPurchaseRecommendation (..)
    , mkReservationPurchaseRecommendation
    , rprAccountScope
    , rprLookbackPeriodInDays
    , rprPaymentOption
    , rprRecommendationDetails
    , rprRecommendationSummary
    , rprServiceSpecification
    , rprTermInYears

    -- * CoverageHoursPercentage
    , CoverageHoursPercentage (..)

    -- * ESInstanceDetails
    , ESInstanceDetails (..)
    , mkESInstanceDetails
    , esidCurrentGeneration
    , esidInstanceClass
    , esidInstanceSize
    , esidRegion
    , esidSizeFlexEligible

    -- * OnDemandHours
    , OnDemandHours (..)

    -- * OnDemandCost
    , OnDemandCost (..)

    -- * TermInYears
    , TermInYears (..)

    -- * SavingsPlansPurchaseRecommendation
    , SavingsPlansPurchaseRecommendation (..)
    , mkSavingsPlansPurchaseRecommendation
    , spprAccountScope
    , spprLookbackPeriodInDays
    , spprPaymentOption
    , spprSavingsPlansPurchaseRecommendationDetails
    , spprSavingsPlansPurchaseRecommendationSummary
    , spprSavingsPlansType
    , spprTermInYears

    -- * Arn
    , Arn (..)

    -- * RightsizingRecommendationSummary
    , RightsizingRecommendationSummary (..)
    , mkRightsizingRecommendationSummary
    , rrsEstimatedTotalMonthlySavingsAmount
    , rrsSavingsCurrencyCode
    , rrsSavingsPercentage
    , rrsTotalRecommendationCount

    -- * SavingsPlansDetails
    , SavingsPlansDetails (..)
    , mkSavingsPlansDetails
    , spdInstanceFamily
    , spdOfferingId
    , spdRegion

    -- * SavingsPlansUtilization
    , SavingsPlansUtilization (..)
    , mkSavingsPlansUtilization
    , spuTotalCommitment
    , spuUnusedCommitment
    , spuUsedCommitment
    , spuUtilizationPercentage

    -- * AnomalyMonitor
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

    -- * AnomalyFeedbackType
    , AnomalyFeedbackType (..)

    -- * ResourceUtilization
    , ResourceUtilization (..)
    , mkResourceUtilization
    , ruEC2ResourceUtilization

    -- * ResourceDetails
    , ResourceDetails (..)
    , mkResourceDetails
    , rdEC2ResourceDetails

    -- * ReservationGroupKey
    , ReservationGroupKey (..)

    -- * DimensionValues
    , DimensionValues (..)
    , mkDimensionValues
    , dvKey
    , dvMatchOptions
    , dvValues

    -- * TargetInstance
    , TargetInstance (..)
    , mkTargetInstance
    , tiCurrencyCode
    , tiDefaultTargetInstance
    , tiEstimatedMonthlyCost
    , tiEstimatedMonthlySavings
    , tiExpectedResourceUtilization
    , tiResourceDetails

    -- * ServiceSpecification
    , ServiceSpecification (..)
    , mkServiceSpecification
    , ssEC2Specification

    -- * SavingsPlansSavings
    , SavingsPlansSavings (..)
    , mkSavingsPlansSavings
    , spsNetSavings
    , spsOnDemandCostEquivalent

    -- * ReservedNormalizedUnits
    , ReservedNormalizedUnits (..)

    -- * MatchOption
    , MatchOption (..)

    -- * Dimension
    , Dimension (..)

    -- * RecommendationTarget
    , RecommendationTarget (..)

    -- * SavingsPlansUtilizationByTime
    , SavingsPlansUtilizationByTime (..)
    , mkSavingsPlansUtilizationByTime
    , spubtTimePeriod
    , spubtUtilization
    , spubtAmortizedCommitment
    , spubtSavings

    -- * MetricName
    , MetricName (..)

    -- * CostCategoryValues
    , CostCategoryValues (..)
    , mkCostCategoryValues
    , ccvKey
    , ccvMatchOptions
    , ccvValues

    -- * SearchString
    , SearchString (..)

    -- * DateInterval
    , DateInterval (..)
    , mkDateInterval
    , diStart
    , diEnd

    -- * AccountScope
    , AccountScope (..)

    -- * CostCategory
    , CostCategory (..)
    , mkCostCategory
    , ccCostCategoryArn
    , ccEffectiveStart
    , ccName
    , ccRuleVersion
    , ccRules
    , ccEffectiveEnd
    , ccProcessingStatus

    -- * Value
    , Value (..)

    -- * TotalAmortizedFee
    , TotalAmortizedFee (..)

    -- * AnomalySubscriptionFrequency
    , AnomalySubscriptionFrequency (..)

    -- * Coverage
    , Coverage (..)
    , mkCoverage
    , cCoverageCost
    , cCoverageHours
    , cCoverageNormalizedUnits

    -- * RDSInstanceDetails
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

    -- * TotalRunningHours
    , TotalRunningHours (..)

    -- * TotalRunningNormalizedUnits
    , TotalRunningNormalizedUnits (..)

    -- * UnusedUnits
    , UnusedUnits (..)

    -- * ElastiCacheInstanceDetails
    , ElastiCacheInstanceDetails (..)
    , mkElastiCacheInstanceDetails
    , eCurrentGeneration
    , eFamily
    , eNodeType
    , eProductDescription
    , eRegion
    , eSizeFlexEligible

    -- * SupportedSavingsPlansType
    , SupportedSavingsPlansType (..)

    -- * Anomaly
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

    -- * CostCategoryName
    , CostCategoryName (..)

    -- * SavingsPlansPurchaseRecommendationDetail
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

    -- * Granularity
    , Granularity (..)

    -- * UtilizationByTime
    , UtilizationByTime (..)
    , mkUtilizationByTime
    , ubtGroups
    , ubtTimePeriod
    , ubtTotal

    -- * ReservationGroupValue
    , ReservationGroupValue (..)

    -- * GenericString
    , GenericString (..)

    -- * CostCategoryProcessingStatus
    , CostCategoryProcessingStatus (..)
    , mkCostCategoryProcessingStatus
    , ccpsComponent
    , ccpsStatus

    -- * MonitorDimension
    , MonitorDimension (..)

    -- * SavingsPlansUtilizationDetail
    , SavingsPlansUtilizationDetail (..)
    , mkSavingsPlansUtilizationDetail
    , spudAmortizedCommitment
    , spudAttributes
    , spudSavings
    , spudSavingsPlanArn
    , spudUtilization

    -- * Metric
    , Metric (..)

    -- * AttributeType
    , AttributeType (..)

    -- * EBSResourceUtilization
    , EBSResourceUtilization (..)
    , mkEBSResourceUtilization
    , ebsruEbsReadBytesPerSecond
    , ebsruEbsReadOpsPerSecond
    , ebsruEbsWriteBytesPerSecond
    , ebsruEbsWriteOpsPerSecond

    -- * ReservedHours
    , ReservedHours (..)

    -- * RootCause
    , RootCause (..)
    , mkRootCause
    , rcLinkedAccount
    , rcRegion
    , rcService
    , rcUsageType

    -- * UnusedHours
    , UnusedHours (..)

    -- * Key
    , Key (..)

    -- * CostCategoryRuleVersion
    , CostCategoryRuleVersion (..)

    -- * EC2InstanceDetails
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

    -- * MetricValue
    , MetricValue (..)
    , mkMetricValue
    , mvAmount
    , mvUnit

    -- * ReservationPurchaseRecommendationMetadata
    , ReservationPurchaseRecommendationMetadata (..)
    , mkReservationPurchaseRecommendationMetadata
    , rprmGenerationTimestamp
    , rprmRecommendationId

    -- * SubscriberType
    , SubscriberType (..)

    -- * AnomalySubscription
    , AnomalySubscription (..)
    , mkAnomalySubscription
    , asMonitorArnList
    , asSubscribers
    , asThreshold
    , asFrequency
    , asSubscriptionName
    , asAccountId
    , asSubscriptionArn

    -- * GroupDefinitionType
    , GroupDefinitionType (..)

    -- * CoverageNormalizedUnitsPercentage
    , CoverageNormalizedUnitsPercentage (..)

    -- * Impact
    , Impact (..)
    , mkImpact
    , iMaxImpact
    , iTotalImpact

    -- * Expression
    , Expression (..)
    , mkExpression
    , eAnd
    , eCostCategories
    , eDimensions
    , eNot
    , eOr
    , eTags

    -- * PurchasedUnits
    , PurchasedUnits (..)

    -- * OnDemandNormalizedUnits
    , OnDemandNormalizedUnits (..)

    -- * CoverageHours
    , CoverageHours (..)
    , mkCoverageHours
    , chCoverageHoursPercentage
    , chOnDemandHours
    , chReservedHours
    , chTotalRunningHours

    -- * ReservationUtilizationGroup
    , ReservationUtilizationGroup (..)
    , mkReservationUtilizationGroup
    , rugAttributes
    , rugKey
    , rugUtilization
    , rugValue

    -- * AmortizedUpfrontFee
    , AmortizedUpfrontFee (..)

    -- * CoverageCost
    , CoverageCost (..)
    , mkCoverageCost
    , ccOnDemandCost

    -- * CostCategoryRule
    , CostCategoryRule (..)
    , mkCostCategoryRule
    , ccrValue
    , ccrRule

    -- * DimensionValuesWithAttributes
    , DimensionValuesWithAttributes (..)
    , mkDimensionValuesWithAttributes
    , dvwaAttributes
    , dvwaValue

    -- * TagKey
    , TagKey (..)

    -- * ModifyRecommendationDetail
    , ModifyRecommendationDetail (..)
    , mkModifyRecommendationDetail
    , mrdTargetInstances

    -- * AmortizedRecurringFee
    , AmortizedRecurringFee (..)

    -- * CostCategoryValue
    , CostCategoryValue (..)

    -- * CoverageByTime
    , CoverageByTime (..)
    , mkCoverageByTime
    , cbtGroups
    , cbtTimePeriod
    , cbtTotal

    -- * TotalImpactFilter
    , TotalImpactFilter (..)
    , mkTotalImpactFilter
    , tifNumericOperator
    , tifStartValue
    , tifEndValue

    -- * CurrentInstance
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

    -- * CostCategoryStatusComponent
    , CostCategoryStatusComponent (..)

    -- * RightsizingType
    , RightsizingType (..)

    -- * UtilizationPercentageInUnits
    , UtilizationPercentageInUnits (..)

    -- * RedshiftInstanceDetails
    , RedshiftInstanceDetails (..)
    , mkRedshiftInstanceDetails
    , ridCurrentGeneration
    , ridFamily
    , ridNodeType
    , ridRegion
    , ridSizeFlexEligible

    -- * SavingsPlansPurchaseRecommendationMetadata
    , SavingsPlansPurchaseRecommendationMetadata (..)
    , mkSavingsPlansPurchaseRecommendationMetadata
    , spprmAdditionalMetadata
    , spprmGenerationTimestamp
    , spprmRecommendationId

    -- * CostCategoryReference
    , CostCategoryReference (..)
    , mkCostCategoryReference
    , ccrCostCategoryArn
    , ccrEffectiveEnd
    , ccrEffectiveStart
    , ccrName
    , ccrNumberOfRules
    , ccrProcessingStatus
    , ccrValues

    -- * SavingsPlansAmortizedCommitment
    , SavingsPlansAmortizedCommitment (..)
    , mkSavingsPlansAmortizedCommitment
    , spacAmortizedRecurringCommitment
    , spacAmortizedUpfrontCommitment
    , spacTotalAmortizedCommitment

    -- * NetRISavings
    , NetRISavings (..)

    -- * InstanceDetails
    , InstanceDetails (..)
    , mkInstanceDetails
    , idEC2InstanceDetails
    , idESInstanceDetails
    , idElastiCacheInstanceDetails
    , idRDSInstanceDetails
    , idRedshiftInstanceDetails

    -- * OnDemandCostOfRIHoursUsed
    , OnDemandCostOfRIHoursUsed (..)

    -- * Entity
    , Entity (..)

    -- * ReservationPurchaseRecommendationDetail
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

    -- * NumericOperator
    , NumericOperator (..)

    -- * ZonedDateTime
    , ZonedDateTime (..)

    -- * OfferingClass
    , OfferingClass (..)

    -- * ReservationPurchaseRecommendationSummary
    , ReservationPurchaseRecommendationSummary (..)
    , mkReservationPurchaseRecommendationSummary
    , rprsCurrencyCode
    , rprsTotalEstimatedMonthlySavingsAmount
    , rprsTotalEstimatedMonthlySavingsPercentage

    -- * TotalPotentialRISavings
    , TotalPotentialRISavings (..)

    -- * SubscriberStatus
    , SubscriberStatus (..)

    -- * AnomalyScore
    , AnomalyScore (..)
    , mkAnomalyScore
    , asMaxScore
    , asCurrentScore

    -- * RightsizingRecommendationConfiguration
    , RightsizingRecommendationConfiguration (..)
    , mkRightsizingRecommendationConfiguration
    , rrcRecommendationTarget
    , rrcBenefitsConsidered

    -- * SavingsPlansCoverage
    , SavingsPlansCoverage (..)
    , mkSavingsPlansCoverage
    , spcAttributes
    , spcCoverage
    , spcTimePeriod

    -- * SavingsPlanArn
    , SavingsPlanArn (..)

    -- * LookbackPeriodInDays
    , LookbackPeriodInDays (..)

    -- * SavingsPlansCoverageData
    , SavingsPlansCoverageData (..)
    , mkSavingsPlansCoverageData
    , spcdCoveragePercentage
    , spcdOnDemandCost
    , spcdSpendCoveredBySavingsPlans
    , spcdTotalCost

    -- * RightsizingRecommendation
    , RightsizingRecommendation (..)
    , mkRightsizingRecommendation
    , rrAccountId
    , rrCurrentInstance
    , rrModifyRecommendationDetail
    , rrRightsizingType
    , rrTerminateRecommendationDetail

    -- * PaymentOption
    , PaymentOption (..)

    -- * SavingsPlansPurchaseRecommendationSummary
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

    -- * EC2Specification
    , EC2Specification (..)
    , mkEC2Specification
    , ecsOfferingClass

    -- * ReservationCoverageGroup
    , ReservationCoverageGroup (..)
    , mkReservationCoverageGroup
    , rcgAttributes
    , rcgCoverage

    -- * TotalActualUnits
    , TotalActualUnits (..)

    -- * TerminateRecommendationDetail
    , TerminateRecommendationDetail (..)
    , mkTerminateRecommendationDetail
    , trdCurrencyCode
    , trdEstimatedMonthlySavings

    -- * MaxCpuUtilizationPercentage
    , MaxCpuUtilizationPercentage (..)

    -- * MaxMemoryUtilizationPercentage
    , MaxMemoryUtilizationPercentage (..)

    -- * MaxStorageUtilizationPercentage
    , MaxStorageUtilizationPercentage (..)

    -- * HourlyOnDemandRate
    , HourlyOnDemandRate (..)

    -- * InstanceType
    , InstanceType (..)

    -- * Memory
    , Memory (..)

    -- * NetworkPerformance
    , NetworkPerformance (..)

    -- * Platform
    , Platform (..)

    -- * Region
    , Region (..)

    -- * Sku
    , Sku (..)

    -- * Storage
    , Storage (..)

    -- * Vcpu
    , Vcpu (..)

    -- * GenerationTimestamp
    , GenerationTimestamp (..)

    -- * RecommendationId
    , RecommendationId (..)

    -- * NextToken
    , NextToken (..)

    -- * MeanValue
    , MeanValue (..)

    -- * PredictionIntervalLowerBound
    , PredictionIntervalLowerBound (..)

    -- * PredictionIntervalUpperBound
    , PredictionIntervalUpperBound (..)

    -- * StartDate
    , StartDate (..)

    -- * EndDate
    , EndDate (..)

    -- * Address
    , Address (..)

    -- * InstanceClass
    , InstanceClass (..)

    -- * InstanceSize
    , InstanceSize (..)

    -- * Service
    , Service (..)

    -- * AccountId
    , AccountId (..)

    -- * EstimatedTotalMonthlySavingsAmount
    , EstimatedTotalMonthlySavingsAmount (..)

    -- * SavingsCurrencyCode
    , SavingsCurrencyCode (..)

    -- * SavingsPercentage
    , SavingsPercentage (..)

    -- * TotalRecommendationCount
    , TotalRecommendationCount (..)

    -- * InstanceFamily
    , InstanceFamily (..)

    -- * OfferingId
    , OfferingId (..)

    -- * MonitorArn
    , MonitorArn (..)

    -- * MonitorName
    , MonitorName (..)

    -- * SubscriptionArn
    , SubscriptionArn (..)

    -- * TotalCommitment
    , TotalCommitment (..)

    -- * UnusedCommitment
    , UnusedCommitment (..)

    -- * UsedCommitment
    , UsedCommitment (..)

    -- * CreationDate
    , CreationDate (..)

    -- * LastEvaluatedDate
    , LastEvaluatedDate (..)

    -- * LastUpdatedDate
    , LastUpdatedDate (..)

    -- * CurrencyCode
    , CurrencyCode (..)

    -- * EstimatedMonthlyCost
    , EstimatedMonthlyCost (..)

    -- * EstimatedMonthlySavings
    , EstimatedMonthlySavings (..)

    -- * NetSavings
    , NetSavings (..)

    -- * OnDemandCostEquivalent
    , OnDemandCostEquivalent (..)

    -- * Start
    , Start (..)

    -- * End
    , End (..)

    -- * EffectiveStart
    , EffectiveStart (..)

    -- * Name
    , Name (..)

    -- * EffectiveEnd
    , EffectiveEnd (..)

    -- * EffectiveOn
    , EffectiveOn (..)

    -- * DatabaseEdition
    , DatabaseEdition (..)

    -- * DatabaseEngine
    , DatabaseEngine (..)

    -- * DeploymentOption
    , DeploymentOption (..)

    -- * Family
    , Family (..)

    -- * LicenseModel
    , LicenseModel (..)

    -- * NodeType
    , NodeType (..)

    -- * ProductDescription
    , ProductDescription (..)

    -- * AnomalyId
    , AnomalyId (..)

    -- * AnomalyEndDate
    , AnomalyEndDate (..)

    -- * AnomalyStartDate
    , AnomalyStartDate (..)

    -- * DimensionValue
    , DimensionValue (..)

    -- * CurrentAverageHourlyOnDemandSpend
    , CurrentAverageHourlyOnDemandSpend (..)

    -- * CurrentMaximumHourlyOnDemandSpend
    , CurrentMaximumHourlyOnDemandSpend (..)

    -- * CurrentMinimumHourlyOnDemandSpend
    , CurrentMinimumHourlyOnDemandSpend (..)

    -- * EstimatedAverageUtilization
    , EstimatedAverageUtilization (..)

    -- * EstimatedMonthlySavingsAmount
    , EstimatedMonthlySavingsAmount (..)

    -- * EstimatedOnDemandCost
    , EstimatedOnDemandCost (..)

    -- * EstimatedOnDemandCostWithCurrentCommitment
    , EstimatedOnDemandCostWithCurrentCommitment (..)

    -- * EstimatedROI
    , EstimatedROI (..)

    -- * EstimatedSPCost
    , EstimatedSPCost (..)

    -- * EstimatedSavingsAmount
    , EstimatedSavingsAmount (..)

    -- * EstimatedSavingsPercentage
    , EstimatedSavingsPercentage (..)

    -- * HourlyCommitmentToPurchase
    , HourlyCommitmentToPurchase (..)

    -- * UpfrontCost
    , UpfrontCost (..)

    -- * Amount
    , Amount (..)

    -- * Unit
    , Unit (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CostExplorer.Types.PurchasedHours
  
import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
  
import Network.AWS.CostExplorer.Types.EC2ResourceDetails
  
import Network.AWS.CostExplorer.Types.MonitorType
  
import Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
  
  
import Network.AWS.CostExplorer.Types.NextPageToken
  
import Network.AWS.CostExplorer.Types.TotalActualHours
  
import Network.AWS.CostExplorer.Types.Context
  
import Network.AWS.CostExplorer.Types.CostCategoryStatus
  
import Network.AWS.CostExplorer.Types.Group
  
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
  
  
import Network.AWS.CostExplorer.Types.ResultByTime
  
import Network.AWS.CostExplorer.Types.AttributeValue
  
import Network.AWS.CostExplorer.Types.UtilizationPercentage
  
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
  
import Network.AWS.CostExplorer.Types.ForecastResult
  
import Network.AWS.CostExplorer.Types.ReservationAggregates
  
import Network.AWS.CostExplorer.Types.TagValues
  
import Network.AWS.CostExplorer.Types.GroupDefinition
  
import Network.AWS.CostExplorer.Types.AnomalyDateInterval
  
import Network.AWS.CostExplorer.Types.Subscriber
  
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
  
import Network.AWS.CostExplorer.Types.CoverageHoursPercentage
  
import Network.AWS.CostExplorer.Types.ESInstanceDetails
  
import Network.AWS.CostExplorer.Types.OnDemandHours
  
import Network.AWS.CostExplorer.Types.OnDemandCost
  
import Network.AWS.CostExplorer.Types.TermInYears
  
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
  
import Network.AWS.CostExplorer.Types.Arn
  
import Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
  
import Network.AWS.CostExplorer.Types.SavingsPlansDetails
  
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
  
import Network.AWS.CostExplorer.Types.AnomalyMonitor
  
import Network.AWS.CostExplorer.Types.AnomalyFeedbackType
  
import Network.AWS.CostExplorer.Types.ResourceUtilization
  
import Network.AWS.CostExplorer.Types.ResourceDetails
  
import Network.AWS.CostExplorer.Types.ReservationGroupKey
  
import Network.AWS.CostExplorer.Types.DimensionValues
  
import Network.AWS.CostExplorer.Types.TargetInstance
  
import Network.AWS.CostExplorer.Types.ServiceSpecification
  
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
  
import Network.AWS.CostExplorer.Types.ReservedNormalizedUnits
  
import Network.AWS.CostExplorer.Types.MatchOption
  
import Network.AWS.CostExplorer.Types.Dimension
  
import Network.AWS.CostExplorer.Types.RecommendationTarget
  
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
  
import Network.AWS.CostExplorer.Types.MetricName
  
import Network.AWS.CostExplorer.Types.CostCategoryValues
  
import Network.AWS.CostExplorer.Types.SearchString
  
import Network.AWS.CostExplorer.Types.DateInterval
  
import Network.AWS.CostExplorer.Types.AccountScope
  
import Network.AWS.CostExplorer.Types.CostCategory
  
import Network.AWS.CostExplorer.Types.Value
  
import Network.AWS.CostExplorer.Types.TotalAmortizedFee
  
import Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
  
import Network.AWS.CostExplorer.Types.Coverage
  
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
  
import Network.AWS.CostExplorer.Types.TotalRunningHours
  
import Network.AWS.CostExplorer.Types.TotalRunningNormalizedUnits
  
import Network.AWS.CostExplorer.Types.UnusedUnits
  
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
  
import Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
  
import Network.AWS.CostExplorer.Types.Anomaly
  
import Network.AWS.CostExplorer.Types.CostCategoryName
  
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
  
import Network.AWS.CostExplorer.Types.Granularity
  
import Network.AWS.CostExplorer.Types.UtilizationByTime
  
import Network.AWS.CostExplorer.Types.ReservationGroupValue
  
import Network.AWS.CostExplorer.Types.GenericString
  
import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
  
import Network.AWS.CostExplorer.Types.MonitorDimension
  
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
  
import Network.AWS.CostExplorer.Types.Metric
  
import Network.AWS.CostExplorer.Types.AttributeType
  
import Network.AWS.CostExplorer.Types.EBSResourceUtilization
  
  
  
import Network.AWS.CostExplorer.Types.ReservedHours
  
import Network.AWS.CostExplorer.Types.RootCause
  
import Network.AWS.CostExplorer.Types.UnusedHours
  
import Network.AWS.CostExplorer.Types.Key
  
import Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
  
import Network.AWS.CostExplorer.Types.EC2InstanceDetails
  
import Network.AWS.CostExplorer.Types.MetricValue
  
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
  
  
import Network.AWS.CostExplorer.Types.SubscriberType
  
import Network.AWS.CostExplorer.Types.AnomalySubscription
  
import Network.AWS.CostExplorer.Types.GroupDefinitionType
  
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnitsPercentage
  
import Network.AWS.CostExplorer.Types.Impact
  
import Network.AWS.CostExplorer.Types.Expression
  
  
import Network.AWS.CostExplorer.Types.PurchasedUnits
  
import Network.AWS.CostExplorer.Types.OnDemandNormalizedUnits
  
import Network.AWS.CostExplorer.Types.CoverageHours
  
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
  
  
import Network.AWS.CostExplorer.Types.AmortizedUpfrontFee
  
import Network.AWS.CostExplorer.Types.CoverageCost
  
import Network.AWS.CostExplorer.Types.CostCategoryRule
  
import Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
  
import Network.AWS.CostExplorer.Types.TagKey
  
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
  
import Network.AWS.CostExplorer.Types.AmortizedRecurringFee
  
import Network.AWS.CostExplorer.Types.CostCategoryValue
  
import Network.AWS.CostExplorer.Types.CoverageByTime
  
import Network.AWS.CostExplorer.Types.TotalImpactFilter
  
import Network.AWS.CostExplorer.Types.CurrentInstance
  
import Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
  
import Network.AWS.CostExplorer.Types.RightsizingType
  
import Network.AWS.CostExplorer.Types.UtilizationPercentageInUnits
  
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
  
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
  
import Network.AWS.CostExplorer.Types.CostCategoryReference
  
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
  
import Network.AWS.CostExplorer.Types.NetRISavings
  
import Network.AWS.CostExplorer.Types.InstanceDetails
  
import Network.AWS.CostExplorer.Types.OnDemandCostOfRIHoursUsed
  
import Network.AWS.CostExplorer.Types.Entity
  
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
  
import Network.AWS.CostExplorer.Types.NumericOperator
  
  
import Network.AWS.CostExplorer.Types.ZonedDateTime
  
import Network.AWS.CostExplorer.Types.OfferingClass
  
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
  
import Network.AWS.CostExplorer.Types.TotalPotentialRISavings
  
import Network.AWS.CostExplorer.Types.SubscriberStatus
  
  
import Network.AWS.CostExplorer.Types.AnomalyScore
  
import Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
  
import Network.AWS.CostExplorer.Types.SavingsPlansCoverage
  
import Network.AWS.CostExplorer.Types.SavingsPlanArn
  
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
  
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
  
import Network.AWS.CostExplorer.Types.RightsizingRecommendation
  
import Network.AWS.CostExplorer.Types.PaymentOption
  
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
  
import Network.AWS.CostExplorer.Types.EC2Specification
  
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
  
import Network.AWS.CostExplorer.Types.TotalActualUnits
  
  
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
  
import Network.AWS.CostExplorer.Types.MaxCpuUtilizationPercentage
  
import Network.AWS.CostExplorer.Types.MaxMemoryUtilizationPercentage
  
import Network.AWS.CostExplorer.Types.MaxStorageUtilizationPercentage
  
import Network.AWS.CostExplorer.Types.HourlyOnDemandRate
  
import Network.AWS.CostExplorer.Types.InstanceType
  
import Network.AWS.CostExplorer.Types.Memory
  
import Network.AWS.CostExplorer.Types.NetworkPerformance
  
import Network.AWS.CostExplorer.Types.Platform
  
import Network.AWS.CostExplorer.Types.Region
  
import Network.AWS.CostExplorer.Types.Sku
  
import Network.AWS.CostExplorer.Types.Storage
  
import Network.AWS.CostExplorer.Types.Vcpu
  
import Network.AWS.CostExplorer.Types.GenerationTimestamp
  
import Network.AWS.CostExplorer.Types.RecommendationId
  
import Network.AWS.CostExplorer.Types.NextToken
  
import Network.AWS.CostExplorer.Types.MeanValue
  
import Network.AWS.CostExplorer.Types.PredictionIntervalLowerBound
  
import Network.AWS.CostExplorer.Types.PredictionIntervalUpperBound
  
import Network.AWS.CostExplorer.Types.StartDate
  
import Network.AWS.CostExplorer.Types.EndDate
  
import Network.AWS.CostExplorer.Types.Address
  
import Network.AWS.CostExplorer.Types.InstanceClass
  
import Network.AWS.CostExplorer.Types.InstanceSize
  
import Network.AWS.CostExplorer.Types.Service
  
import Network.AWS.CostExplorer.Types.AccountId
  
import Network.AWS.CostExplorer.Types.EstimatedTotalMonthlySavingsAmount
  
import Network.AWS.CostExplorer.Types.SavingsCurrencyCode
  
import Network.AWS.CostExplorer.Types.SavingsPercentage
  
import Network.AWS.CostExplorer.Types.TotalRecommendationCount
  
import Network.AWS.CostExplorer.Types.InstanceFamily
  
import Network.AWS.CostExplorer.Types.OfferingId
  
import Network.AWS.CostExplorer.Types.MonitorArn
  
import Network.AWS.CostExplorer.Types.MonitorName
  
import Network.AWS.CostExplorer.Types.SubscriptionArn
  
import Network.AWS.CostExplorer.Types.TotalCommitment
  
import Network.AWS.CostExplorer.Types.UnusedCommitment
  
import Network.AWS.CostExplorer.Types.UsedCommitment
  
import Network.AWS.CostExplorer.Types.CreationDate
  
import Network.AWS.CostExplorer.Types.LastEvaluatedDate
  
import Network.AWS.CostExplorer.Types.LastUpdatedDate
  
import Network.AWS.CostExplorer.Types.CurrencyCode
  
import Network.AWS.CostExplorer.Types.EstimatedMonthlyCost
  
import Network.AWS.CostExplorer.Types.EstimatedMonthlySavings
  
import Network.AWS.CostExplorer.Types.NetSavings
  
import Network.AWS.CostExplorer.Types.OnDemandCostEquivalent
  
import Network.AWS.CostExplorer.Types.Start
  
import Network.AWS.CostExplorer.Types.End
  
import Network.AWS.CostExplorer.Types.EffectiveStart
  
import Network.AWS.CostExplorer.Types.Name
  
import Network.AWS.CostExplorer.Types.EffectiveEnd
  
import Network.AWS.CostExplorer.Types.EffectiveOn
  
import Network.AWS.CostExplorer.Types.DatabaseEdition
  
import Network.AWS.CostExplorer.Types.DatabaseEngine
  
import Network.AWS.CostExplorer.Types.DeploymentOption
  
import Network.AWS.CostExplorer.Types.Family
  
import Network.AWS.CostExplorer.Types.LicenseModel
  
import Network.AWS.CostExplorer.Types.NodeType
  
import Network.AWS.CostExplorer.Types.ProductDescription
  
import Network.AWS.CostExplorer.Types.AnomalyId
  
import Network.AWS.CostExplorer.Types.AnomalyEndDate
  
import Network.AWS.CostExplorer.Types.AnomalyStartDate
  
import Network.AWS.CostExplorer.Types.DimensionValue
  
import Network.AWS.CostExplorer.Types.CurrentAverageHourlyOnDemandSpend
  
import Network.AWS.CostExplorer.Types.CurrentMaximumHourlyOnDemandSpend
  
import Network.AWS.CostExplorer.Types.CurrentMinimumHourlyOnDemandSpend
  
import Network.AWS.CostExplorer.Types.EstimatedAverageUtilization
  
import Network.AWS.CostExplorer.Types.EstimatedMonthlySavingsAmount
  
import Network.AWS.CostExplorer.Types.EstimatedOnDemandCost
  
import Network.AWS.CostExplorer.Types.EstimatedOnDemandCostWithCurrentCommitment
  
import Network.AWS.CostExplorer.Types.EstimatedROI
  
import Network.AWS.CostExplorer.Types.EstimatedSPCost
  
import Network.AWS.CostExplorer.Types.EstimatedSavingsAmount
  
import Network.AWS.CostExplorer.Types.EstimatedSavingsPercentage
  
import Network.AWS.CostExplorer.Types.HourlyCommitmentToPurchase
  
import Network.AWS.CostExplorer.Types.UpfrontCost
  
import Network.AWS.CostExplorer.Types.Amount
  
import Network.AWS.CostExplorer.Types.Unit
  

-- | API version @2017-10-25@ of the Amazon Cost Explorer Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CostExplorer",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "ce",
                 Core._svcVersion = "2017-10-25", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CostExplorer",
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
          | Core.otherwise = Core.Nothing

-- | The cost anomaly subscription does not exist for the account. 
_UnknownSubscriptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnknownSubscriptionException
  = Core._MatchServiceError mkServiceConfig
      "UnknownSubscriptionException"
{-# INLINEABLE _UnknownSubscriptionException #-}
{-# DEPRECATED _UnknownSubscriptionException "Use generic-lens or generic-optics instead"  #-}

-- | The requested report expired. Update the date interval and try again.
_BillExpirationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BillExpirationException
  = Core._MatchServiceError mkServiceConfig "BillExpirationException"
{-# INLINEABLE _BillExpirationException #-}
{-# DEPRECATED _BillExpirationException "Use generic-lens or generic-optics instead"  #-}

-- | Your request parameters changed between pages. Try again with the old parameters or without a pagination token.
_RequestChangedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestChangedException
  = Core._MatchServiceError mkServiceConfig "RequestChangedException"
{-# INLINEABLE _RequestChangedException #-}
{-# DEPRECATED _RequestChangedException "Use generic-lens or generic-optics instead"  #-}

-- | Cost Explorer was unable to identify the usage unit. Provide @UsageType/UsageTypeGroup@ filter selections that contain matching units, for example: @hours@ .
_UnresolvableUsageUnitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnresolvableUsageUnitException
  = Core._MatchServiceError mkServiceConfig
      "UnresolvableUsageUnitException"
{-# INLINEABLE _UnresolvableUsageUnitException #-}
{-# DEPRECATED _UnresolvableUsageUnitException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of resources you can create, or exceeded the size of an individual resource. 
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException
  = Core._MatchServiceError mkServiceConfig
      "ServiceQuotaExceededException"
{-# INLINEABLE _ServiceQuotaExceededException #-}
{-# DEPRECATED _ServiceQuotaExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The pagination token is invalid. Try again without a pagination token.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | The requested data is unavailable.
_DataUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DataUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "DataUnavailableException"
{-# INLINEABLE _DataUnavailableException #-}
{-# DEPRECATED _DataUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | The cost anomaly monitor does not exist for the account. 
_UnknownMonitorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnknownMonitorException
  = Core._MatchServiceError mkServiceConfig "UnknownMonitorException"
{-# INLINEABLE _UnknownMonitorException #-}
{-# DEPRECATED _UnknownMonitorException "Use generic-lens or generic-optics instead"  #-}

-- | The specified ARN in the request doesn't exist. 
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | You made too many calls in a short period of time. Try again later.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
