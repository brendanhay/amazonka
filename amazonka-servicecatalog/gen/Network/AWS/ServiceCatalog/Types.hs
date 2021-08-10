{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidParametersException,
    _OperationNotSupportedException,
    _InvalidStateException,
    _DuplicateResourceException,
    _ResourceInUseException,
    _LimitExceededException,
    _TagOptionNotMigratedException,
    _ResourceNotFoundException,

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

    -- * DescribePortfolioShareType
    DescribePortfolioShareType (..),

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
    newAccessLevelFilter,
    accessLevelFilter_key,
    accessLevelFilter_value,

    -- * BudgetDetail
    BudgetDetail (..),
    newBudgetDetail,
    budgetDetail_budgetName,

    -- * CloudWatchDashboard
    CloudWatchDashboard (..),
    newCloudWatchDashboard,
    cloudWatchDashboard_name,

    -- * ConstraintDetail
    ConstraintDetail (..),
    newConstraintDetail,
    constraintDetail_constraintId,
    constraintDetail_portfolioId,
    constraintDetail_owner,
    constraintDetail_productId,
    constraintDetail_description,
    constraintDetail_type,

    -- * ConstraintSummary
    ConstraintSummary (..),
    newConstraintSummary,
    constraintSummary_description,
    constraintSummary_type,

    -- * ExecutionParameter
    ExecutionParameter (..),
    newExecutionParameter,
    executionParameter_name,
    executionParameter_defaultValues,
    executionParameter_type,

    -- * FailedServiceActionAssociation
    FailedServiceActionAssociation (..),
    newFailedServiceActionAssociation,
    failedServiceActionAssociation_provisioningArtifactId,
    failedServiceActionAssociation_serviceActionId,
    failedServiceActionAssociation_productId,
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_errorCode,

    -- * LaunchPath
    LaunchPath (..),
    newLaunchPath,
    launchPath_id,
    launchPath_name,

    -- * LaunchPathSummary
    LaunchPathSummary (..),
    newLaunchPathSummary,
    launchPathSummary_constraintSummaries,
    launchPathSummary_id,
    launchPathSummary_name,
    launchPathSummary_tags,

    -- * ListRecordHistorySearchFilter
    ListRecordHistorySearchFilter (..),
    newListRecordHistorySearchFilter,
    listRecordHistorySearchFilter_key,
    listRecordHistorySearchFilter_value,

    -- * ListTagOptionsFilters
    ListTagOptionsFilters (..),
    newListTagOptionsFilters,
    listTagOptionsFilters_key,
    listTagOptionsFilters_active,
    listTagOptionsFilters_value,

    -- * OrganizationNode
    OrganizationNode (..),
    newOrganizationNode,
    organizationNode_value,
    organizationNode_type,

    -- * ParameterConstraints
    ParameterConstraints (..),
    newParameterConstraints,
    parameterConstraints_maxValue,
    parameterConstraints_minLength,
    parameterConstraints_allowedValues,
    parameterConstraints_minValue,
    parameterConstraints_constraintDescription,
    parameterConstraints_maxLength,
    parameterConstraints_allowedPattern,

    -- * PortfolioDetail
    PortfolioDetail (..),
    newPortfolioDetail,
    portfolioDetail_providerName,
    portfolioDetail_id,
    portfolioDetail_arn,
    portfolioDetail_createdTime,
    portfolioDetail_description,
    portfolioDetail_displayName,

    -- * PortfolioShareDetail
    PortfolioShareDetail (..),
    newPortfolioShareDetail,
    portfolioShareDetail_shareTagOptions,
    portfolioShareDetail_principalId,
    portfolioShareDetail_accepted,
    portfolioShareDetail_type,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_principalARN,
    principal_principalType,

    -- * ProductViewAggregationValue
    ProductViewAggregationValue (..),
    newProductViewAggregationValue,
    productViewAggregationValue_value,
    productViewAggregationValue_approximateCount,

    -- * ProductViewDetail
    ProductViewDetail (..),
    newProductViewDetail,
    productViewDetail_status,
    productViewDetail_productARN,
    productViewDetail_createdTime,
    productViewDetail_productViewSummary,

    -- * ProductViewSummary
    ProductViewSummary (..),
    newProductViewSummary,
    productViewSummary_distributor,
    productViewSummary_id,
    productViewSummary_name,
    productViewSummary_hasDefaultPath,
    productViewSummary_shortDescription,
    productViewSummary_supportUrl,
    productViewSummary_supportDescription,
    productViewSummary_owner,
    productViewSummary_productId,
    productViewSummary_supportEmail,
    productViewSummary_type,

    -- * ProvisionedProductAttribute
    ProvisionedProductAttribute (..),
    newProvisionedProductAttribute,
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_status,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_arn,
    provisionedProductAttribute_id,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_name,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_tags,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_type,
    provisionedProductAttribute_physicalId,
    provisionedProductAttribute_userArnSession,
    provisionedProductAttribute_lastRecordId,

    -- * ProvisionedProductDetail
    ProvisionedProductDetail (..),
    newProvisionedProductDetail,
    provisionedProductDetail_statusMessage,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_status,
    provisionedProductDetail_arn,
    provisionedProductDetail_id,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_name,
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_productId,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_type,
    provisionedProductDetail_lastRecordId,

    -- * ProvisionedProductPlanDetails
    ProvisionedProductPlanDetails (..),
    newProvisionedProductPlanDetails,
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_updatedTime,
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_tags,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_provisionProductName,

    -- * ProvisionedProductPlanSummary
    ProvisionedProductPlanSummary (..),
    newProvisionedProductPlanSummary,
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_provisioningArtifactId,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_planType,
    provisionedProductPlanSummary_provisionProductName,

    -- * ProvisioningArtifact
    ProvisioningArtifact (..),
    newProvisioningArtifact,
    provisioningArtifact_guidance,
    provisioningArtifact_id,
    provisioningArtifact_createdTime,
    provisioningArtifact_name,
    provisioningArtifact_description,

    -- * ProvisioningArtifactDetail
    ProvisioningArtifactDetail (..),
    newProvisioningArtifactDetail,
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_description,
    provisioningArtifactDetail_type,

    -- * ProvisioningArtifactOutput
    ProvisioningArtifactOutput (..),
    newProvisioningArtifactOutput,
    provisioningArtifactOutput_key,
    provisioningArtifactOutput_description,

    -- * ProvisioningArtifactParameter
    ProvisioningArtifactParameter (..),
    newProvisioningArtifactParameter,
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_parameterType,
    provisioningArtifactParameter_parameterKey,
    provisioningArtifactParameter_description,
    provisioningArtifactParameter_defaultValue,

    -- * ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences (..),
    newProvisioningArtifactPreferences,
    provisioningArtifactPreferences_stackSetAccounts,
    provisioningArtifactPreferences_stackSetRegions,

    -- * ProvisioningArtifactProperties
    ProvisioningArtifactProperties (..),
    newProvisioningArtifactProperties,
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_description,
    provisioningArtifactProperties_type,
    provisioningArtifactProperties_info,

    -- * ProvisioningArtifactSummary
    ProvisioningArtifactSummary (..),
    newProvisioningArtifactSummary,
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_provisioningArtifactMetadata,
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_description,

    -- * ProvisioningArtifactView
    ProvisioningArtifactView (..),
    newProvisioningArtifactView,
    provisioningArtifactView_productViewSummary,
    provisioningArtifactView_provisioningArtifact,

    -- * ProvisioningParameter
    ProvisioningParameter (..),
    newProvisioningParameter,
    provisioningParameter_key,
    provisioningParameter_value,

    -- * ProvisioningPreferences
    ProvisioningPreferences (..),
    newProvisioningPreferences,
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetFailureTolerancePercentage,
    provisioningPreferences_stackSetRegions,
    provisioningPreferences_stackSetMaxConcurrencyCount,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,

    -- * RecordDetail
    RecordDetail (..),
    newRecordDetail,
    recordDetail_recordTags,
    recordDetail_status,
    recordDetail_provisionedProductName,
    recordDetail_recordErrors,
    recordDetail_provisionedProductId,
    recordDetail_provisionedProductType,
    recordDetail_recordId,
    recordDetail_updatedTime,
    recordDetail_createdTime,
    recordDetail_provisioningArtifactId,
    recordDetail_launchRoleArn,
    recordDetail_productId,
    recordDetail_pathId,
    recordDetail_recordType,

    -- * RecordError
    RecordError (..),
    newRecordError,
    recordError_code,
    recordError_description,

    -- * RecordOutput
    RecordOutput (..),
    newRecordOutput,
    recordOutput_outputKey,
    recordOutput_outputValue,
    recordOutput_description,

    -- * RecordTag
    RecordTag (..),
    newRecordTag,
    recordTag_key,
    recordTag_value,

    -- * ResourceChange
    ResourceChange (..),
    newResourceChange,
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_action,
    resourceChange_replacement,

    -- * ResourceChangeDetail
    ResourceChangeDetail (..),
    newResourceChangeDetail,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_target,

    -- * ResourceDetail
    ResourceDetail (..),
    newResourceDetail,
    resourceDetail_id,
    resourceDetail_arn,
    resourceDetail_createdTime,
    resourceDetail_name,
    resourceDetail_description,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    newResourceTargetDefinition,
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,
    resourceTargetDefinition_attribute,

    -- * ServiceActionAssociation
    ServiceActionAssociation (..),
    newServiceActionAssociation,
    serviceActionAssociation_serviceActionId,
    serviceActionAssociation_productId,
    serviceActionAssociation_provisioningArtifactId,

    -- * ServiceActionDetail
    ServiceActionDetail (..),
    newServiceActionDetail,
    serviceActionDetail_serviceActionSummary,
    serviceActionDetail_definition,

    -- * ServiceActionSummary
    ServiceActionSummary (..),
    newServiceActionSummary,
    serviceActionSummary_id,
    serviceActionSummary_definitionType,
    serviceActionSummary_name,
    serviceActionSummary_description,

    -- * ShareDetails
    ShareDetails (..),
    newShareDetails,
    shareDetails_shareErrors,
    shareDetails_successfulShares,

    -- * ShareError
    ShareError (..),
    newShareError,
    shareError_message,
    shareError_accounts,
    shareError_error,

    -- * StackInstance
    StackInstance (..),
    newStackInstance,
    stackInstance_stackInstanceStatus,
    stackInstance_account,
    stackInstance_region,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagOptionDetail
    TagOptionDetail (..),
    newTagOptionDetail,
    tagOptionDetail_key,
    tagOptionDetail_id,
    tagOptionDetail_active,
    tagOptionDetail_owner,
    tagOptionDetail_value,

    -- * TagOptionSummary
    TagOptionSummary (..),
    newTagOptionSummary,
    tagOptionSummary_key,
    tagOptionSummary_values,

    -- * UpdateProvisioningParameter
    UpdateProvisioningParameter (..),
    newUpdateProvisioningParameter,
    updateProvisioningParameter_key,
    updateProvisioningParameter_usePreviousValue,
    updateProvisioningParameter_value,

    -- * UpdateProvisioningPreferences
    UpdateProvisioningPreferences (..),
    newUpdateProvisioningPreferences,
    updateProvisioningPreferences_stackSetOperationType,
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,
    updateProvisioningPreferences_stackSetRegions,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,

    -- * UsageInstruction
    UsageInstruction (..),
    newUsageInstruction,
    usageInstruction_value,
    usageInstruction_type,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
import Network.AWS.ServiceCatalog.Types.DescribePortfolioShareType
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
import Network.AWS.ServiceCatalog.Types.PortfolioShareDetail
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ServiceCatalog",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "servicecatalog",
      Core._serviceSigningName = "servicecatalog",
      Core._serviceVersion = "2015-12-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ServiceCatalog",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more parameters provided to the operation are not valid.
_InvalidParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidParametersException"

-- | The operation is not supported.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    defaultService
    "OperationNotSupportedException"

-- | An attempt was made to modify a resource that is in a state that is not
-- valid. Check your resources to ensure that they are in valid states
-- before retrying the operation.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The specified resource is a duplicate.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"

-- | A resource that is currently in use. Ensure that the resource is not in
-- use and retry the operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The current limits of the service would have been exceeded by this
-- operation. Decrease your resource use or increase your service limits
-- and retry the operation.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | An operation requiring TagOptions failed because the TagOptions
-- migration process has not been performed for this account. Please use
-- the AWS console to perform the migration process before retrying the
-- operation.
_TagOptionNotMigratedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagOptionNotMigratedException =
  Core._MatchServiceError
    defaultService
    "TagOptionNotMigratedException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
