-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types
  ( -- * Service configuration
    serviceCatalogService,

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
    AccessLevelFilter (..),
    mkAccessLevelFilter,
    alfValue,
    alfKey,

    -- * BudgetDetail
    BudgetDetail (..),
    mkBudgetDetail,
    bdBudgetName,

    -- * CloudWatchDashboard
    CloudWatchDashboard (..),
    mkCloudWatchDashboard,
    cwdName,

    -- * ConstraintDetail
    ConstraintDetail (..),
    mkConstraintDetail,
    cdPortfolioId,
    cdConstraintId,
    cdOwner,
    cdType,
    cdDescription,
    cdProductId,

    -- * ConstraintSummary
    ConstraintSummary (..),
    mkConstraintSummary,
    csType,
    csDescription,

    -- * ExecutionParameter
    ExecutionParameter (..),
    mkExecutionParameter,
    epDefaultValues,
    epName,
    epType,

    -- * FailedServiceActionAssociation
    FailedServiceActionAssociation (..),
    mkFailedServiceActionAssociation,
    fsaaProvisioningArtifactId,
    fsaaErrorCode,
    fsaaErrorMessage,
    fsaaServiceActionId,
    fsaaProductId,

    -- * LaunchPath
    LaunchPath (..),
    mkLaunchPath,
    lpName,
    lpId,

    -- * LaunchPathSummary
    LaunchPathSummary (..),
    mkLaunchPathSummary,
    lpsConstraintSummaries,
    lpsName,
    lpsId,
    lpsTags,

    -- * ListRecordHistorySearchFilter
    ListRecordHistorySearchFilter (..),
    mkListRecordHistorySearchFilter,
    lrhsfValue,
    lrhsfKey,

    -- * ListTagOptionsFilters
    ListTagOptionsFilters (..),
    mkListTagOptionsFilters,
    ltofValue,
    ltofActive,
    ltofKey,

    -- * OrganizationNode
    OrganizationNode (..),
    mkOrganizationNode,
    onValue,
    onType,

    -- * ParameterConstraints
    ParameterConstraints (..),
    mkParameterConstraints,
    pcAllowedValues,

    -- * PortfolioDetail
    PortfolioDetail (..),
    mkPortfolioDetail,
    pdARN,
    pdCreatedTime,
    pdId,
    pdDisplayName,
    pdDescription,
    pdProviderName,

    -- * Principal
    Principal (..),
    mkPrincipal,
    pPrincipalType,
    pPrincipalARN,

    -- * ProductViewAggregationValue
    ProductViewAggregationValue (..),
    mkProductViewAggregationValue,
    pvavValue,
    pvavApproximateCount,

    -- * ProductViewDetail
    ProductViewDetail (..),
    mkProductViewDetail,
    pvdStatus,
    pvdProductViewSummary,
    pvdCreatedTime,
    pvdProductARN,

    -- * ProductViewSummary
    ProductViewSummary (..),
    mkProductViewSummary,
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
    ProvisionedProductAttribute (..),
    mkProvisionedProductAttribute,
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
    ProvisionedProductDetail (..),
    mkProvisionedProductDetail,
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
    ProvisionedProductPlanDetails (..),
    mkProvisionedProductPlanDetails,
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
    ProvisionedProductPlanSummary (..),
    mkProvisionedProductPlanSummary,
    pppsProvisionProductId,
    pppsProvisioningArtifactId,
    pppsProvisionProductName,
    pppsPlanId,
    pppsPlanName,
    pppsPlanType,

    -- * ProvisioningArtifact
    ProvisioningArtifact (..),
    mkProvisioningArtifact,
    paCreatedTime,
    paName,
    paId,
    paGuidance,
    paDescription,

    -- * ProvisioningArtifactDetail
    ProvisioningArtifactDetail (..),
    mkProvisioningArtifactDetail,
    padCreatedTime,
    padActive,
    padName,
    padId,
    padType,
    padGuidance,
    padDescription,

    -- * ProvisioningArtifactOutput
    ProvisioningArtifactOutput (..),
    mkProvisioningArtifactOutput,
    paoKey,
    paoDescription,

    -- * ProvisioningArtifactParameter
    ProvisioningArtifactParameter (..),
    mkProvisioningArtifactParameter,
    pIsNoEcho,
    pParameterKey,
    pParameterType,
    pParameterConstraints,
    pDefaultValue,
    pDescription,

    -- * ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences (..),
    mkProvisioningArtifactPreferences,
    papStackSetRegions,
    papStackSetAccounts,

    -- * ProvisioningArtifactProperties
    ProvisioningArtifactProperties (..),
    mkProvisioningArtifactProperties,
    papDisableTemplateValidation,
    papName,
    papType,
    papDescription,
    papInfo,

    -- * ProvisioningArtifactSummary
    ProvisioningArtifactSummary (..),
    mkProvisioningArtifactSummary,
    pasProvisioningArtifactMetadata,
    pasCreatedTime,
    pasName,
    pasId,
    pasDescription,

    -- * ProvisioningArtifactView
    ProvisioningArtifactView (..),
    mkProvisioningArtifactView,
    pavProductViewSummary,
    pavProvisioningArtifact,

    -- * ProvisioningParameter
    ProvisioningParameter (..),
    mkProvisioningParameter,
    ppValue,
    ppKey,

    -- * ProvisioningPreferences
    ProvisioningPreferences (..),
    mkProvisioningPreferences,
    ppStackSetRegions,
    ppStackSetMaxConcurrencyPercentage,
    ppStackSetFailureToleranceCount,
    ppStackSetFailureTolerancePercentage,
    ppStackSetAccounts,
    ppStackSetMaxConcurrencyCount,

    -- * RecordDetail
    RecordDetail (..),
    mkRecordDetail,
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
    RecordError (..),
    mkRecordError,
    reCode,
    reDescription,

    -- * RecordOutput
    RecordOutput (..),
    mkRecordOutput,
    roOutputValue,
    roOutputKey,
    roDescription,

    -- * RecordTag
    RecordTag (..),
    mkRecordTag,
    rtValue,
    rtKey,

    -- * ResourceChange
    ResourceChange (..),
    mkResourceChange,
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcResourceType,
    rcAction,
    rcScope,
    rcDetails,
    rcReplacement,

    -- * ResourceChangeDetail
    ResourceChangeDetail (..),
    mkResourceChangeDetail,
    rcdCausingEntity,
    rcdEvaluation,
    rcdTarget,

    -- * ResourceDetail
    ResourceDetail (..),
    mkResourceDetail,
    rARN,
    rCreatedTime,
    rName,
    rId,
    rDescription,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    mkResourceTargetDefinition,
    rtdAttribute,
    rtdRequiresRecreation,
    rtdName,

    -- * ServiceActionAssociation
    ServiceActionAssociation (..),
    mkServiceActionAssociation,
    saaServiceActionId,
    saaProductId,
    saaProvisioningArtifactId,

    -- * ServiceActionDetail
    ServiceActionDetail (..),
    mkServiceActionDetail,
    sadServiceActionSummary,
    sadDefinition,

    -- * ServiceActionSummary
    ServiceActionSummary (..),
    mkServiceActionSummary,
    sasName,
    sasId,
    sasDefinitionType,
    sasDescription,

    -- * ShareDetails
    ShareDetails (..),
    mkShareDetails,
    sdShareErrors,
    sdSuccessfulShares,

    -- * ShareError
    ShareError (..),
    mkShareError,
    seAccounts,
    seError,
    seMessage,

    -- * StackInstance
    StackInstance (..),
    mkStackInstance,
    siAccount,
    siRegion,
    siStackInstanceStatus,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TagOptionDetail
    TagOptionDetail (..),
    mkTagOptionDetail,
    todValue,
    todActive,
    todKey,
    todId,

    -- * TagOptionSummary
    TagOptionSummary (..),
    mkTagOptionSummary,
    tosValues,
    tosKey,

    -- * UpdateProvisioningParameter
    UpdateProvisioningParameter (..),
    mkUpdateProvisioningParameter,
    uppValue,
    uppKey,
    uppUsePreviousValue,

    -- * UpdateProvisioningPreferences
    UpdateProvisioningPreferences (..),
    mkUpdateProvisioningPreferences,
    uppStackSetRegions,
    uppStackSetMaxConcurrencyPercentage,
    uppStackSetFailureToleranceCount,
    uppStackSetFailureTolerancePercentage,
    uppStackSetAccounts,
    uppStackSetMaxConcurrencyCount,
    uppStackSetOperationType,

    -- * UsageInstruction
    UsageInstruction (..),
    mkUsageInstruction,
    uiValue,
    uiType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-12-10@ of the Amazon Service Catalog SDK configuration.
serviceCatalogService :: Lude.Service
serviceCatalogService =
  Lude.Service
    { Lude._svcAbbrev = "ServiceCatalog",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "servicecatalog",
      Lude._svcVersion = "2015-12-10",
      Lude._svcEndpoint = Lude.defaultEndpoint serviceCatalogService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ServiceCatalog",
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
