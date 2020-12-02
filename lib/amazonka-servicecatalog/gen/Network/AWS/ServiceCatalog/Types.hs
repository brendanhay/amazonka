{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types
  ( -- * Service Configuration
    serviceCatalog,

    -- * Errors

    -- * AccessLevelFilterKey
    AccessLevelFilterKey (..),

    -- * AccessStatus
    AccessStatus (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * CopyOption
    CopyOption (..),

    -- * CopyProductStatus
    CopyProductStatus (..),

    -- * EvaluationType
    EvaluationType (..),

    -- * OrganizationNodeType
    OrganizationNodeType (..),

    -- * PortfolioShareType
    PortfolioShareType (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * ProductSource
    ProductSource (..),

    -- * ProductType
    ProductType (..),

    -- * ProductViewFilterBy
    ProductViewFilterBy (..),

    -- * ProductViewSortBy
    ProductViewSortBy (..),

    -- * PropertyKey
    PropertyKey (..),

    -- * ProvisionedProductPlanStatus
    ProvisionedProductPlanStatus (..),

    -- * ProvisionedProductPlanType
    ProvisionedProductPlanType (..),

    -- * ProvisionedProductStatus
    ProvisionedProductStatus (..),

    -- * ProvisionedProductViewFilterBy
    ProvisionedProductViewFilterBy (..),

    -- * ProvisioningArtifactGuidance
    ProvisioningArtifactGuidance (..),

    -- * ProvisioningArtifactPropertyName
    ProvisioningArtifactPropertyName (..),

    -- * ProvisioningArtifactType
    ProvisioningArtifactType (..),

    -- * RecordStatus
    RecordStatus (..),

    -- * Replacement
    Replacement (..),

    -- * RequestStatus
    RequestStatus (..),

    -- * RequiresRecreation
    RequiresRecreation (..),

    -- * ResourceAttribute
    ResourceAttribute (..),

    -- * ServiceActionAssociationErrorCode
    ServiceActionAssociationErrorCode (..),

    -- * ServiceActionDefinitionKey
    ServiceActionDefinitionKey (..),

    -- * ServiceActionDefinitionType
    ServiceActionDefinitionType (..),

    -- * ShareStatus
    ShareStatus (..),

    -- * SortOrder
    SortOrder (..),

    -- * StackInstanceStatus
    StackInstanceStatus (..),

    -- * StackSetOperationType
    StackSetOperationType (..),

    -- * AccessLevelFilter
    AccessLevelFilter,
    accessLevelFilter,
    alfValue,
    alfKey,

    -- * BudgetDetail
    BudgetDetail,
    budgetDetail,
    bdBudgetName,

    -- * CloudWatchDashboard
    CloudWatchDashboard,
    cloudWatchDashboard,
    cwdName,

    -- * ConstraintDetail
    ConstraintDetail,
    constraintDetail,
    cdPortfolioId,
    cdConstraintId,
    cdOwner,
    cdType,
    cdDescription,
    cdProductId,

    -- * ConstraintSummary
    ConstraintSummary,
    constraintSummary,
    csType,
    csDescription,

    -- * ExecutionParameter
    ExecutionParameter,
    executionParameter,
    epDefaultValues,
    epName,
    epType,

    -- * FailedServiceActionAssociation
    FailedServiceActionAssociation,
    failedServiceActionAssociation,
    fsaaProvisioningArtifactId,
    fsaaErrorCode,
    fsaaErrorMessage,
    fsaaServiceActionId,
    fsaaProductId,

    -- * LaunchPath
    LaunchPath,
    launchPath,
    lpName,
    lpId,

    -- * LaunchPathSummary
    LaunchPathSummary,
    launchPathSummary,
    lpsConstraintSummaries,
    lpsName,
    lpsId,
    lpsTags,

    -- * ListRecordHistorySearchFilter
    ListRecordHistorySearchFilter,
    listRecordHistorySearchFilter,
    lrhsfValue,
    lrhsfKey,

    -- * ListTagOptionsFilters
    ListTagOptionsFilters,
    listTagOptionsFilters,
    ltofValue,
    ltofActive,
    ltofKey,

    -- * OrganizationNode
    OrganizationNode,
    organizationNode,
    onValue,
    onType,

    -- * ParameterConstraints
    ParameterConstraints,
    parameterConstraints,
    pcAllowedValues,

    -- * PortfolioDetail
    PortfolioDetail,
    portfolioDetail,
    pdARN,
    pdCreatedTime,
    pdId,
    pdDisplayName,
    pdDescription,
    pdProviderName,

    -- * Principal
    Principal,
    principal,
    pPrincipalType,
    pPrincipalARN,

    -- * ProductViewAggregationValue
    ProductViewAggregationValue,
    productViewAggregationValue,
    pvavValue,
    pvavApproximateCount,

    -- * ProductViewDetail
    ProductViewDetail,
    productViewDetail,
    pvdStatus,
    pvdProductViewSummary,
    pvdCreatedTime,
    pvdProductARN,

    -- * ProductViewSummary
    ProductViewSummary,
    productViewSummary,
    pvsOwner,
    pvsSupportURL,
    pvsShortDescription,
    pvsHasDefaultPath,
    pvsDistributor,
    pvsName,
    pvsId,
    pvsType,
    pvsSupportEmail,
    pvsProductId,
    pvsSupportDescription,

    -- * ProvisionedProductAttribute
    ProvisionedProductAttribute,
    provisionedProductAttribute,
    ppaIdempotencyToken,
    ppaStatus,
    ppaProductName,
    ppaLastSuccessfulProvisioningRecordId,
    ppaProvisioningArtifactId,
    ppaARN,
    ppaCreatedTime,
    ppaProvisioningArtifactName,
    ppaUserARN,
    ppaStatusMessage,
    ppaName,
    ppaLastRecordId,
    ppaUserARNSession,
    ppaId,
    ppaType,
    ppaPhysicalId,
    ppaLastProvisioningRecordId,
    ppaProductId,
    ppaTags,

    -- * ProvisionedProductDetail
    ProvisionedProductDetail,
    provisionedProductDetail,
    ppdLaunchRoleARN,
    ppdIdempotencyToken,
    ppdStatus,
    ppdLastSuccessfulProvisioningRecordId,
    ppdProvisioningArtifactId,
    ppdARN,
    ppdCreatedTime,
    ppdStatusMessage,
    ppdName,
    ppdLastRecordId,
    ppdId,
    ppdType,
    ppdLastProvisioningRecordId,
    ppdProductId,

    -- * ProvisionedProductPlanDetails
    ProvisionedProductPlanDetails,
    provisionedProductPlanDetails,
    pppdStatus,
    pppdProvisionProductId,
    pppdProvisioningArtifactId,
    pppdProvisionProductName,
    pppdCreatedTime,
    pppdNotificationARNs,
    pppdPlanId,
    pppdPlanName,
    pppdStatusMessage,
    pppdUpdatedTime,
    pppdPathId,
    pppdProvisioningParameters,
    pppdPlanType,
    pppdProductId,
    pppdTags,

    -- * ProvisionedProductPlanSummary
    ProvisionedProductPlanSummary,
    provisionedProductPlanSummary,
    pppsProvisionProductId,
    pppsProvisioningArtifactId,
    pppsProvisionProductName,
    pppsPlanId,
    pppsPlanName,
    pppsPlanType,

    -- * ProvisioningArtifact
    ProvisioningArtifact,
    provisioningArtifact,
    paCreatedTime,
    paName,
    paId,
    paGuidance,
    paDescription,

    -- * ProvisioningArtifactDetail
    ProvisioningArtifactDetail,
    provisioningArtifactDetail,
    padCreatedTime,
    padActive,
    padName,
    padId,
    padType,
    padGuidance,
    padDescription,

    -- * ProvisioningArtifactOutput
    ProvisioningArtifactOutput,
    provisioningArtifactOutput,
    paoKey,
    paoDescription,

    -- * ProvisioningArtifactParameter
    ProvisioningArtifactParameter,
    provisioningArtifactParameter,
    pIsNoEcho,
    pParameterKey,
    pParameterType,
    pParameterConstraints,
    pDefaultValue,
    pDescription,

    -- * ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences,
    provisioningArtifactPreferences,
    papStackSetRegions,
    papStackSetAccounts,

    -- * ProvisioningArtifactProperties
    ProvisioningArtifactProperties,
    provisioningArtifactProperties,
    papDisableTemplateValidation,
    papName,
    papType,
    papDescription,
    papInfo,

    -- * ProvisioningArtifactSummary
    ProvisioningArtifactSummary,
    provisioningArtifactSummary,
    pasProvisioningArtifactMetadata,
    pasCreatedTime,
    pasName,
    pasId,
    pasDescription,

    -- * ProvisioningArtifactView
    ProvisioningArtifactView,
    provisioningArtifactView,
    pavProductViewSummary,
    pavProvisioningArtifact,

    -- * ProvisioningParameter
    ProvisioningParameter,
    provisioningParameter,
    ppValue,
    ppKey,

    -- * ProvisioningPreferences
    ProvisioningPreferences,
    provisioningPreferences,
    ppStackSetRegions,
    ppStackSetMaxConcurrencyPercentage,
    ppStackSetFailureToleranceCount,
    ppStackSetFailureTolerancePercentage,
    ppStackSetAccounts,
    ppStackSetMaxConcurrencyCount,

    -- * RecordDetail
    RecordDetail,
    recordDetail,
    rdLaunchRoleARN,
    rdStatus,
    rdRecordTags,
    rdProvisionedProductName,
    rdProvisioningArtifactId,
    rdCreatedTime,
    rdRecordType,
    rdRecordId,
    rdProvisionedProductType,
    rdUpdatedTime,
    rdPathId,
    rdProvisionedProductId,
    rdRecordErrors,
    rdProductId,

    -- * RecordError
    RecordError,
    recordError,
    reCode,
    reDescription,

    -- * RecordOutput
    RecordOutput,
    recordOutput,
    roOutputValue,
    roOutputKey,
    roDescription,

    -- * RecordTag
    RecordTag,
    recordTag,
    rtValue,
    rtKey,

    -- * ResourceChange
    ResourceChange,
    resourceChange,
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcResourceType,
    rcAction,
    rcScope,
    rcDetails,
    rcReplacement,

    -- * ResourceChangeDetail
    ResourceChangeDetail,
    resourceChangeDetail,
    rcdCausingEntity,
    rcdEvaluation,
    rcdTarget,

    -- * ResourceDetail
    ResourceDetail,
    resourceDetail,
    rARN,
    rCreatedTime,
    rName,
    rId,
    rDescription,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition,
    resourceTargetDefinition,
    rtdAttribute,
    rtdRequiresRecreation,
    rtdName,

    -- * ServiceActionAssociation
    ServiceActionAssociation,
    serviceActionAssociation,
    saaServiceActionId,
    saaProductId,
    saaProvisioningArtifactId,

    -- * ServiceActionDetail
    ServiceActionDetail,
    serviceActionDetail,
    sadServiceActionSummary,
    sadDefinition,

    -- * ServiceActionSummary
    ServiceActionSummary,
    serviceActionSummary,
    sasName,
    sasId,
    sasDefinitionType,
    sasDescription,

    -- * ShareDetails
    ShareDetails,
    shareDetails,
    sdShareErrors,
    sdSuccessfulShares,

    -- * ShareError
    ShareError,
    shareError,
    seAccounts,
    seError,
    seMessage,

    -- * StackInstance
    StackInstance,
    stackInstance,
    siAccount,
    siRegion,
    siStackInstanceStatus,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TagOptionDetail
    TagOptionDetail,
    tagOptionDetail,
    todValue,
    todActive,
    todKey,
    todId,

    -- * TagOptionSummary
    TagOptionSummary,
    tagOptionSummary,
    tosValues,
    tosKey,

    -- * UpdateProvisioningParameter
    UpdateProvisioningParameter,
    updateProvisioningParameter,
    uppValue,
    uppKey,
    uppUsePreviousValue,

    -- * UpdateProvisioningPreferences
    UpdateProvisioningPreferences,
    updateProvisioningPreferences,
    uppStackSetRegions,
    uppStackSetMaxConcurrencyPercentage,
    uppStackSetFailureToleranceCount,
    uppStackSetFailureTolerancePercentage,
    uppStackSetAccounts,
    uppStackSetMaxConcurrencyCount,
    uppStackSetOperationType,

    -- * UsageInstruction
    UsageInstruction,
    usageInstruction,
    uiValue,
    uiType,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.AccessLevelFilter
import Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey
import Network.AWS.ServiceCatalog.Types.AccessStatus
import Network.AWS.ServiceCatalog.Types.BudgetDetail
import Network.AWS.ServiceCatalog.Types.ChangeAction
import Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
import Network.AWS.ServiceCatalog.Types.ConstraintDetail
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
import Network.AWS.ServiceCatalog.Types.CopyOption
import Network.AWS.ServiceCatalog.Types.CopyProductStatus
import Network.AWS.ServiceCatalog.Types.EvaluationType
import Network.AWS.ServiceCatalog.Types.ExecutionParameter
import Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
import Network.AWS.ServiceCatalog.Types.LaunchPath
import Network.AWS.ServiceCatalog.Types.LaunchPathSummary
import Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
import Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
import Network.AWS.ServiceCatalog.Types.OrganizationNode
import Network.AWS.ServiceCatalog.Types.OrganizationNodeType
import Network.AWS.ServiceCatalog.Types.ParameterConstraints
import Network.AWS.ServiceCatalog.Types.PortfolioDetail
import Network.AWS.ServiceCatalog.Types.PortfolioShareType
import Network.AWS.ServiceCatalog.Types.Principal
import Network.AWS.ServiceCatalog.Types.PrincipalType
import Network.AWS.ServiceCatalog.Types.ProductSource
import Network.AWS.ServiceCatalog.Types.ProductType
import Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
import Network.AWS.ServiceCatalog.Types.ProductViewDetail
import Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
import Network.AWS.ServiceCatalog.Types.ProductViewSortBy
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.PropertyKey
import Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
import Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
import Network.AWS.ServiceCatalog.Types.ProvisionedProductViewFilterBy
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyName
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
import Network.AWS.ServiceCatalog.Types.ProvisioningParameter
import Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
import Network.AWS.ServiceCatalog.Types.RecordDetail
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordOutput
import Network.AWS.ServiceCatalog.Types.RecordStatus
import Network.AWS.ServiceCatalog.Types.RecordTag
import Network.AWS.ServiceCatalog.Types.Replacement
import Network.AWS.ServiceCatalog.Types.RequestStatus
import Network.AWS.ServiceCatalog.Types.RequiresRecreation
import Network.AWS.ServiceCatalog.Types.ResourceAttribute
import Network.AWS.ServiceCatalog.Types.ResourceChange
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
import Network.AWS.ServiceCatalog.Types.ResourceDetail
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType
import Network.AWS.ServiceCatalog.Types.ServiceActionDetail
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary
import Network.AWS.ServiceCatalog.Types.ShareDetails
import Network.AWS.ServiceCatalog.Types.ShareError
import Network.AWS.ServiceCatalog.Types.ShareStatus
import Network.AWS.ServiceCatalog.Types.SortOrder
import Network.AWS.ServiceCatalog.Types.StackInstance
import Network.AWS.ServiceCatalog.Types.StackInstanceStatus
import Network.AWS.ServiceCatalog.Types.StackSetOperationType
import Network.AWS.ServiceCatalog.Types.Tag
import Network.AWS.ServiceCatalog.Types.TagOptionDetail
import Network.AWS.ServiceCatalog.Types.TagOptionSummary
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
import Network.AWS.ServiceCatalog.Types.UsageInstruction
import Network.AWS.Sign.V4

-- | API version @2015-12-10@ of the Amazon Service Catalog SDK configuration.
serviceCatalog :: Service
serviceCatalog =
  Service
    { _svcAbbrev = "ServiceCatalog",
      _svcSigner = v4,
      _svcPrefix = "servicecatalog",
      _svcVersion = "2015-12-10",
      _svcEndpoint = defaultEndpoint serviceCatalog,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ServiceCatalog",
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
