{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CostExplorer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- You can use the Cost Explorer API to programmatically query your cost
-- and usage data. You can query for aggregated data such as total monthly
-- costs or total daily usage. You can also query for granular data. This
-- might include the number of daily write operations for Amazon DynamoDB
-- database tables in your production environment.
--
-- Service Endpoint
--
-- The Cost Explorer API provides the following endpoint:
--
-- -   @https:\/\/ce.us-east-1.amazonaws.com@
--
-- For information about the costs that are associated with the Cost
-- Explorer API, see
-- <http://aws.amazon.com/aws-cost-management/pricing/ Amazon Web Services Cost Management Pricing>.
module Amazonka.CostExplorer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BillExpirationException
    _BillExpirationException,

    -- ** DataUnavailableException
    _DataUnavailableException,

    -- ** GenerationExistsException
    _GenerationExistsException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** RequestChangedException
    _RequestChangedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnknownMonitorException
    _UnknownMonitorException,

    -- ** UnknownSubscriptionException
    _UnknownSubscriptionException,

    -- ** UnresolvableUsageUnitException
    _UnresolvableUsageUnitException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAnomalyMonitor
    CreateAnomalyMonitor (CreateAnomalyMonitor'),
    newCreateAnomalyMonitor,
    CreateAnomalyMonitorResponse (CreateAnomalyMonitorResponse'),
    newCreateAnomalyMonitorResponse,

    -- ** CreateAnomalySubscription
    CreateAnomalySubscription (CreateAnomalySubscription'),
    newCreateAnomalySubscription,
    CreateAnomalySubscriptionResponse (CreateAnomalySubscriptionResponse'),
    newCreateAnomalySubscriptionResponse,

    -- ** CreateCostCategoryDefinition
    CreateCostCategoryDefinition (CreateCostCategoryDefinition'),
    newCreateCostCategoryDefinition,
    CreateCostCategoryDefinitionResponse (CreateCostCategoryDefinitionResponse'),
    newCreateCostCategoryDefinitionResponse,

    -- ** DeleteAnomalyMonitor
    DeleteAnomalyMonitor (DeleteAnomalyMonitor'),
    newDeleteAnomalyMonitor,
    DeleteAnomalyMonitorResponse (DeleteAnomalyMonitorResponse'),
    newDeleteAnomalyMonitorResponse,

    -- ** DeleteAnomalySubscription
    DeleteAnomalySubscription (DeleteAnomalySubscription'),
    newDeleteAnomalySubscription,
    DeleteAnomalySubscriptionResponse (DeleteAnomalySubscriptionResponse'),
    newDeleteAnomalySubscriptionResponse,

    -- ** DeleteCostCategoryDefinition
    DeleteCostCategoryDefinition (DeleteCostCategoryDefinition'),
    newDeleteCostCategoryDefinition,
    DeleteCostCategoryDefinitionResponse (DeleteCostCategoryDefinitionResponse'),
    newDeleteCostCategoryDefinitionResponse,

    -- ** DescribeCostCategoryDefinition
    DescribeCostCategoryDefinition (DescribeCostCategoryDefinition'),
    newDescribeCostCategoryDefinition,
    DescribeCostCategoryDefinitionResponse (DescribeCostCategoryDefinitionResponse'),
    newDescribeCostCategoryDefinitionResponse,

    -- ** GetAnomalies
    GetAnomalies (GetAnomalies'),
    newGetAnomalies,
    GetAnomaliesResponse (GetAnomaliesResponse'),
    newGetAnomaliesResponse,

    -- ** GetAnomalyMonitors
    GetAnomalyMonitors (GetAnomalyMonitors'),
    newGetAnomalyMonitors,
    GetAnomalyMonitorsResponse (GetAnomalyMonitorsResponse'),
    newGetAnomalyMonitorsResponse,

    -- ** GetAnomalySubscriptions
    GetAnomalySubscriptions (GetAnomalySubscriptions'),
    newGetAnomalySubscriptions,
    GetAnomalySubscriptionsResponse (GetAnomalySubscriptionsResponse'),
    newGetAnomalySubscriptionsResponse,

    -- ** GetCostAndUsage
    GetCostAndUsage (GetCostAndUsage'),
    newGetCostAndUsage,
    GetCostAndUsageResponse (GetCostAndUsageResponse'),
    newGetCostAndUsageResponse,

    -- ** GetCostAndUsageWithResources
    GetCostAndUsageWithResources (GetCostAndUsageWithResources'),
    newGetCostAndUsageWithResources,
    GetCostAndUsageWithResourcesResponse (GetCostAndUsageWithResourcesResponse'),
    newGetCostAndUsageWithResourcesResponse,

    -- ** GetCostCategories
    GetCostCategories (GetCostCategories'),
    newGetCostCategories,
    GetCostCategoriesResponse (GetCostCategoriesResponse'),
    newGetCostCategoriesResponse,

    -- ** GetCostForecast
    GetCostForecast (GetCostForecast'),
    newGetCostForecast,
    GetCostForecastResponse (GetCostForecastResponse'),
    newGetCostForecastResponse,

    -- ** GetDimensionValues
    GetDimensionValues (GetDimensionValues'),
    newGetDimensionValues,
    GetDimensionValuesResponse (GetDimensionValuesResponse'),
    newGetDimensionValuesResponse,

    -- ** GetReservationCoverage
    GetReservationCoverage (GetReservationCoverage'),
    newGetReservationCoverage,
    GetReservationCoverageResponse (GetReservationCoverageResponse'),
    newGetReservationCoverageResponse,

    -- ** GetReservationPurchaseRecommendation
    GetReservationPurchaseRecommendation (GetReservationPurchaseRecommendation'),
    newGetReservationPurchaseRecommendation,
    GetReservationPurchaseRecommendationResponse (GetReservationPurchaseRecommendationResponse'),
    newGetReservationPurchaseRecommendationResponse,

    -- ** GetReservationUtilization
    GetReservationUtilization (GetReservationUtilization'),
    newGetReservationUtilization,
    GetReservationUtilizationResponse (GetReservationUtilizationResponse'),
    newGetReservationUtilizationResponse,

    -- ** GetRightsizingRecommendation
    GetRightsizingRecommendation (GetRightsizingRecommendation'),
    newGetRightsizingRecommendation,
    GetRightsizingRecommendationResponse (GetRightsizingRecommendationResponse'),
    newGetRightsizingRecommendationResponse,

    -- ** GetSavingsPlansCoverage
    GetSavingsPlansCoverage (GetSavingsPlansCoverage'),
    newGetSavingsPlansCoverage,
    GetSavingsPlansCoverageResponse (GetSavingsPlansCoverageResponse'),
    newGetSavingsPlansCoverageResponse,

    -- ** GetSavingsPlansPurchaseRecommendation
    GetSavingsPlansPurchaseRecommendation (GetSavingsPlansPurchaseRecommendation'),
    newGetSavingsPlansPurchaseRecommendation,
    GetSavingsPlansPurchaseRecommendationResponse (GetSavingsPlansPurchaseRecommendationResponse'),
    newGetSavingsPlansPurchaseRecommendationResponse,

    -- ** GetSavingsPlansUtilization
    GetSavingsPlansUtilization (GetSavingsPlansUtilization'),
    newGetSavingsPlansUtilization,
    GetSavingsPlansUtilizationResponse (GetSavingsPlansUtilizationResponse'),
    newGetSavingsPlansUtilizationResponse,

    -- ** GetSavingsPlansUtilizationDetails
    GetSavingsPlansUtilizationDetails (GetSavingsPlansUtilizationDetails'),
    newGetSavingsPlansUtilizationDetails,
    GetSavingsPlansUtilizationDetailsResponse (GetSavingsPlansUtilizationDetailsResponse'),
    newGetSavingsPlansUtilizationDetailsResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** GetUsageForecast
    GetUsageForecast (GetUsageForecast'),
    newGetUsageForecast,
    GetUsageForecastResponse (GetUsageForecastResponse'),
    newGetUsageForecastResponse,

    -- ** ListCostAllocationTags
    ListCostAllocationTags (ListCostAllocationTags'),
    newListCostAllocationTags,
    ListCostAllocationTagsResponse (ListCostAllocationTagsResponse'),
    newListCostAllocationTagsResponse,

    -- ** ListCostCategoryDefinitions
    ListCostCategoryDefinitions (ListCostCategoryDefinitions'),
    newListCostCategoryDefinitions,
    ListCostCategoryDefinitionsResponse (ListCostCategoryDefinitionsResponse'),
    newListCostCategoryDefinitionsResponse,

    -- ** ListSavingsPlansPurchaseRecommendationGeneration
    ListSavingsPlansPurchaseRecommendationGeneration (ListSavingsPlansPurchaseRecommendationGeneration'),
    newListSavingsPlansPurchaseRecommendationGeneration,
    ListSavingsPlansPurchaseRecommendationGenerationResponse (ListSavingsPlansPurchaseRecommendationGenerationResponse'),
    newListSavingsPlansPurchaseRecommendationGenerationResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ProvideAnomalyFeedback
    ProvideAnomalyFeedback (ProvideAnomalyFeedback'),
    newProvideAnomalyFeedback,
    ProvideAnomalyFeedbackResponse (ProvideAnomalyFeedbackResponse'),
    newProvideAnomalyFeedbackResponse,

    -- ** StartSavingsPlansPurchaseRecommendationGeneration
    StartSavingsPlansPurchaseRecommendationGeneration (StartSavingsPlansPurchaseRecommendationGeneration'),
    newStartSavingsPlansPurchaseRecommendationGeneration,
    StartSavingsPlansPurchaseRecommendationGenerationResponse (StartSavingsPlansPurchaseRecommendationGenerationResponse'),
    newStartSavingsPlansPurchaseRecommendationGenerationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAnomalyMonitor
    UpdateAnomalyMonitor (UpdateAnomalyMonitor'),
    newUpdateAnomalyMonitor,
    UpdateAnomalyMonitorResponse (UpdateAnomalyMonitorResponse'),
    newUpdateAnomalyMonitorResponse,

    -- ** UpdateAnomalySubscription
    UpdateAnomalySubscription (UpdateAnomalySubscription'),
    newUpdateAnomalySubscription,
    UpdateAnomalySubscriptionResponse (UpdateAnomalySubscriptionResponse'),
    newUpdateAnomalySubscriptionResponse,

    -- ** UpdateCostAllocationTagsStatus
    UpdateCostAllocationTagsStatus (UpdateCostAllocationTagsStatus'),
    newUpdateCostAllocationTagsStatus,
    UpdateCostAllocationTagsStatusResponse (UpdateCostAllocationTagsStatusResponse'),
    newUpdateCostAllocationTagsStatusResponse,

    -- ** UpdateCostCategoryDefinition
    UpdateCostCategoryDefinition (UpdateCostCategoryDefinition'),
    newUpdateCostCategoryDefinition,
    UpdateCostCategoryDefinitionResponse (UpdateCostCategoryDefinitionResponse'),
    newUpdateCostCategoryDefinitionResponse,

    -- * Types

    -- ** AccountScope
    AccountScope (..),

    -- ** AnomalyFeedbackType
    AnomalyFeedbackType (..),

    -- ** AnomalySubscriptionFrequency
    AnomalySubscriptionFrequency (..),

    -- ** Context
    Context (..),

    -- ** CostAllocationTagStatus
    CostAllocationTagStatus (..),

    -- ** CostAllocationTagType
    CostAllocationTagType (..),

    -- ** CostCategoryInheritedValueDimensionName
    CostCategoryInheritedValueDimensionName (..),

    -- ** CostCategoryRuleType
    CostCategoryRuleType (..),

    -- ** CostCategoryRuleVersion
    CostCategoryRuleVersion (..),

    -- ** CostCategorySplitChargeMethod
    CostCategorySplitChargeMethod (..),

    -- ** CostCategorySplitChargeRuleParameterType
    CostCategorySplitChargeRuleParameterType (..),

    -- ** CostCategoryStatus
    CostCategoryStatus (..),

    -- ** CostCategoryStatusComponent
    CostCategoryStatusComponent (..),

    -- ** Dimension
    Dimension (..),

    -- ** FindingReasonCode
    FindingReasonCode (..),

    -- ** GenerationStatus
    GenerationStatus (..),

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

    -- ** PlatformDifference
    PlatformDifference (..),

    -- ** RecommendationTarget
    RecommendationTarget (..),

    -- ** RightsizingType
    RightsizingType (..),

    -- ** SavingsPlansDataType
    SavingsPlansDataType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** SubscriberStatus
    SubscriberStatus (..),

    -- ** SubscriberType
    SubscriberType (..),

    -- ** SupportedSavingsPlansType
    SupportedSavingsPlansType (..),

    -- ** TermInYears
    TermInYears (..),

    -- ** Anomaly
    Anomaly (Anomaly'),
    newAnomaly,

    -- ** AnomalyDateInterval
    AnomalyDateInterval (AnomalyDateInterval'),
    newAnomalyDateInterval,

    -- ** AnomalyMonitor
    AnomalyMonitor (AnomalyMonitor'),
    newAnomalyMonitor,

    -- ** AnomalyScore
    AnomalyScore (AnomalyScore'),
    newAnomalyScore,

    -- ** AnomalySubscription
    AnomalySubscription (AnomalySubscription'),
    newAnomalySubscription,

    -- ** CostAllocationTag
    CostAllocationTag (CostAllocationTag'),
    newCostAllocationTag,

    -- ** CostAllocationTagStatusEntry
    CostAllocationTagStatusEntry (CostAllocationTagStatusEntry'),
    newCostAllocationTagStatusEntry,

    -- ** CostCategory
    CostCategory (CostCategory'),
    newCostCategory,

    -- ** CostCategoryInheritedValueDimension
    CostCategoryInheritedValueDimension (CostCategoryInheritedValueDimension'),
    newCostCategoryInheritedValueDimension,

    -- ** CostCategoryProcessingStatus
    CostCategoryProcessingStatus (CostCategoryProcessingStatus'),
    newCostCategoryProcessingStatus,

    -- ** CostCategoryReference
    CostCategoryReference (CostCategoryReference'),
    newCostCategoryReference,

    -- ** CostCategoryRule
    CostCategoryRule (CostCategoryRule'),
    newCostCategoryRule,

    -- ** CostCategorySplitChargeRule
    CostCategorySplitChargeRule (CostCategorySplitChargeRule'),
    newCostCategorySplitChargeRule,

    -- ** CostCategorySplitChargeRuleParameter
    CostCategorySplitChargeRuleParameter (CostCategorySplitChargeRuleParameter'),
    newCostCategorySplitChargeRuleParameter,

    -- ** CostCategoryValues
    CostCategoryValues (CostCategoryValues'),
    newCostCategoryValues,

    -- ** Coverage
    Coverage (Coverage'),
    newCoverage,

    -- ** CoverageByTime
    CoverageByTime (CoverageByTime'),
    newCoverageByTime,

    -- ** CoverageCost
    CoverageCost (CoverageCost'),
    newCoverageCost,

    -- ** CoverageHours
    CoverageHours (CoverageHours'),
    newCoverageHours,

    -- ** CoverageNormalizedUnits
    CoverageNormalizedUnits (CoverageNormalizedUnits'),
    newCoverageNormalizedUnits,

    -- ** CurrentInstance
    CurrentInstance (CurrentInstance'),
    newCurrentInstance,

    -- ** DateInterval
    DateInterval (DateInterval'),
    newDateInterval,

    -- ** DimensionValues
    DimensionValues (DimensionValues'),
    newDimensionValues,

    -- ** DimensionValuesWithAttributes
    DimensionValuesWithAttributes (DimensionValuesWithAttributes'),
    newDimensionValuesWithAttributes,

    -- ** DiskResourceUtilization
    DiskResourceUtilization (DiskResourceUtilization'),
    newDiskResourceUtilization,

    -- ** EBSResourceUtilization
    EBSResourceUtilization (EBSResourceUtilization'),
    newEBSResourceUtilization,

    -- ** EC2InstanceDetails
    EC2InstanceDetails (EC2InstanceDetails'),
    newEC2InstanceDetails,

    -- ** EC2ResourceDetails
    EC2ResourceDetails (EC2ResourceDetails'),
    newEC2ResourceDetails,

    -- ** EC2ResourceUtilization
    EC2ResourceUtilization (EC2ResourceUtilization'),
    newEC2ResourceUtilization,

    -- ** EC2Specification
    EC2Specification (EC2Specification'),
    newEC2Specification,

    -- ** ESInstanceDetails
    ESInstanceDetails (ESInstanceDetails'),
    newESInstanceDetails,

    -- ** ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails (ElastiCacheInstanceDetails'),
    newElastiCacheInstanceDetails,

    -- ** Expression
    Expression (Expression'),
    newExpression,

    -- ** ForecastResult
    ForecastResult (ForecastResult'),
    newForecastResult,

    -- ** GenerationSummary
    GenerationSummary (GenerationSummary'),
    newGenerationSummary,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupDefinition
    GroupDefinition (GroupDefinition'),
    newGroupDefinition,

    -- ** Impact
    Impact (Impact'),
    newImpact,

    -- ** InstanceDetails
    InstanceDetails (InstanceDetails'),
    newInstanceDetails,

    -- ** MetricValue
    MetricValue (MetricValue'),
    newMetricValue,

    -- ** ModifyRecommendationDetail
    ModifyRecommendationDetail (ModifyRecommendationDetail'),
    newModifyRecommendationDetail,

    -- ** NetworkResourceUtilization
    NetworkResourceUtilization (NetworkResourceUtilization'),
    newNetworkResourceUtilization,

    -- ** RDSInstanceDetails
    RDSInstanceDetails (RDSInstanceDetails'),
    newRDSInstanceDetails,

    -- ** RedshiftInstanceDetails
    RedshiftInstanceDetails (RedshiftInstanceDetails'),
    newRedshiftInstanceDetails,

    -- ** ReservationAggregates
    ReservationAggregates (ReservationAggregates'),
    newReservationAggregates,

    -- ** ReservationCoverageGroup
    ReservationCoverageGroup (ReservationCoverageGroup'),
    newReservationCoverageGroup,

    -- ** ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation (ReservationPurchaseRecommendation'),
    newReservationPurchaseRecommendation,

    -- ** ReservationPurchaseRecommendationDetail
    ReservationPurchaseRecommendationDetail (ReservationPurchaseRecommendationDetail'),
    newReservationPurchaseRecommendationDetail,

    -- ** ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata (ReservationPurchaseRecommendationMetadata'),
    newReservationPurchaseRecommendationMetadata,

    -- ** ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary (ReservationPurchaseRecommendationSummary'),
    newReservationPurchaseRecommendationSummary,

    -- ** ReservationUtilizationGroup
    ReservationUtilizationGroup (ReservationUtilizationGroup'),
    newReservationUtilizationGroup,

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** ResourceUtilization
    ResourceUtilization (ResourceUtilization'),
    newResourceUtilization,

    -- ** ResultByTime
    ResultByTime (ResultByTime'),
    newResultByTime,

    -- ** RightsizingRecommendation
    RightsizingRecommendation (RightsizingRecommendation'),
    newRightsizingRecommendation,

    -- ** RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration (RightsizingRecommendationConfiguration'),
    newRightsizingRecommendationConfiguration,

    -- ** RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata (RightsizingRecommendationMetadata'),
    newRightsizingRecommendationMetadata,

    -- ** RightsizingRecommendationSummary
    RightsizingRecommendationSummary (RightsizingRecommendationSummary'),
    newRightsizingRecommendationSummary,

    -- ** RootCause
    RootCause (RootCause'),
    newRootCause,

    -- ** SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment (SavingsPlansAmortizedCommitment'),
    newSavingsPlansAmortizedCommitment,

    -- ** SavingsPlansCoverage
    SavingsPlansCoverage (SavingsPlansCoverage'),
    newSavingsPlansCoverage,

    -- ** SavingsPlansCoverageData
    SavingsPlansCoverageData (SavingsPlansCoverageData'),
    newSavingsPlansCoverageData,

    -- ** SavingsPlansDetails
    SavingsPlansDetails (SavingsPlansDetails'),
    newSavingsPlansDetails,

    -- ** SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation (SavingsPlansPurchaseRecommendation'),
    newSavingsPlansPurchaseRecommendation,

    -- ** SavingsPlansPurchaseRecommendationDetail
    SavingsPlansPurchaseRecommendationDetail (SavingsPlansPurchaseRecommendationDetail'),
    newSavingsPlansPurchaseRecommendationDetail,

    -- ** SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata (SavingsPlansPurchaseRecommendationMetadata'),
    newSavingsPlansPurchaseRecommendationMetadata,

    -- ** SavingsPlansPurchaseRecommendationSummary
    SavingsPlansPurchaseRecommendationSummary (SavingsPlansPurchaseRecommendationSummary'),
    newSavingsPlansPurchaseRecommendationSummary,

    -- ** SavingsPlansSavings
    SavingsPlansSavings (SavingsPlansSavings'),
    newSavingsPlansSavings,

    -- ** SavingsPlansUtilization
    SavingsPlansUtilization (SavingsPlansUtilization'),
    newSavingsPlansUtilization,

    -- ** SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates (SavingsPlansUtilizationAggregates'),
    newSavingsPlansUtilizationAggregates,

    -- ** SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime (SavingsPlansUtilizationByTime'),
    newSavingsPlansUtilizationByTime,

    -- ** SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail (SavingsPlansUtilizationDetail'),
    newSavingsPlansUtilizationDetail,

    -- ** ServiceSpecification
    ServiceSpecification (ServiceSpecification'),
    newServiceSpecification,

    -- ** SortDefinition
    SortDefinition (SortDefinition'),
    newSortDefinition,

    -- ** Subscriber
    Subscriber (Subscriber'),
    newSubscriber,

    -- ** TagValues
    TagValues (TagValues'),
    newTagValues,

    -- ** TargetInstance
    TargetInstance (TargetInstance'),
    newTargetInstance,

    -- ** TerminateRecommendationDetail
    TerminateRecommendationDetail (TerminateRecommendationDetail'),
    newTerminateRecommendationDetail,

    -- ** TotalImpactFilter
    TotalImpactFilter (TotalImpactFilter'),
    newTotalImpactFilter,

    -- ** UpdateCostAllocationTagsStatusError
    UpdateCostAllocationTagsStatusError (UpdateCostAllocationTagsStatusError'),
    newUpdateCostAllocationTagsStatusError,

    -- ** UtilizationByTime
    UtilizationByTime (UtilizationByTime'),
    newUtilizationByTime,
  )
where

import Amazonka.CostExplorer.CreateAnomalyMonitor
import Amazonka.CostExplorer.CreateAnomalySubscription
import Amazonka.CostExplorer.CreateCostCategoryDefinition
import Amazonka.CostExplorer.DeleteAnomalyMonitor
import Amazonka.CostExplorer.DeleteAnomalySubscription
import Amazonka.CostExplorer.DeleteCostCategoryDefinition
import Amazonka.CostExplorer.DescribeCostCategoryDefinition
import Amazonka.CostExplorer.GetAnomalies
import Amazonka.CostExplorer.GetAnomalyMonitors
import Amazonka.CostExplorer.GetAnomalySubscriptions
import Amazonka.CostExplorer.GetCostAndUsage
import Amazonka.CostExplorer.GetCostAndUsageWithResources
import Amazonka.CostExplorer.GetCostCategories
import Amazonka.CostExplorer.GetCostForecast
import Amazonka.CostExplorer.GetDimensionValues
import Amazonka.CostExplorer.GetReservationCoverage
import Amazonka.CostExplorer.GetReservationPurchaseRecommendation
import Amazonka.CostExplorer.GetReservationUtilization
import Amazonka.CostExplorer.GetRightsizingRecommendation
import Amazonka.CostExplorer.GetSavingsPlansCoverage
import Amazonka.CostExplorer.GetSavingsPlansPurchaseRecommendation
import Amazonka.CostExplorer.GetSavingsPlansUtilization
import Amazonka.CostExplorer.GetSavingsPlansUtilizationDetails
import Amazonka.CostExplorer.GetTags
import Amazonka.CostExplorer.GetUsageForecast
import Amazonka.CostExplorer.Lens
import Amazonka.CostExplorer.ListCostAllocationTags
import Amazonka.CostExplorer.ListCostCategoryDefinitions
import Amazonka.CostExplorer.ListSavingsPlansPurchaseRecommendationGeneration
import Amazonka.CostExplorer.ListTagsForResource
import Amazonka.CostExplorer.ProvideAnomalyFeedback
import Amazonka.CostExplorer.StartSavingsPlansPurchaseRecommendationGeneration
import Amazonka.CostExplorer.TagResource
import Amazonka.CostExplorer.Types
import Amazonka.CostExplorer.UntagResource
import Amazonka.CostExplorer.UpdateAnomalyMonitor
import Amazonka.CostExplorer.UpdateAnomalySubscription
import Amazonka.CostExplorer.UpdateCostAllocationTagsStatus
import Amazonka.CostExplorer.UpdateCostCategoryDefinition
import Amazonka.CostExplorer.Waiters

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
