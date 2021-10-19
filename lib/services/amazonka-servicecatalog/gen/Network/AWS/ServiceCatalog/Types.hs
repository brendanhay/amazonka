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
    _DuplicateResourceException,
    _OperationNotSupportedException,
    _TagOptionNotMigratedException,
    _ResourceNotFoundException,
    _InvalidStateException,
    _LimitExceededException,
    _ResourceInUseException,

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
    accessLevelFilter_value,
    accessLevelFilter_key,

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
    constraintDetail_portfolioId,
    constraintDetail_constraintId,
    constraintDetail_owner,
    constraintDetail_type,
    constraintDetail_description,
    constraintDetail_productId,

    -- * ConstraintSummary
    ConstraintSummary (..),
    newConstraintSummary,
    constraintSummary_type,
    constraintSummary_description,

    -- * ExecutionParameter
    ExecutionParameter (..),
    newExecutionParameter,
    executionParameter_defaultValues,
    executionParameter_name,
    executionParameter_type,

    -- * FailedServiceActionAssociation
    FailedServiceActionAssociation (..),
    newFailedServiceActionAssociation,
    failedServiceActionAssociation_provisioningArtifactId,
    failedServiceActionAssociation_errorCode,
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_serviceActionId,
    failedServiceActionAssociation_productId,

    -- * LaunchPath
    LaunchPath (..),
    newLaunchPath,
    launchPath_name,
    launchPath_id,

    -- * LaunchPathSummary
    LaunchPathSummary (..),
    newLaunchPathSummary,
    launchPathSummary_constraintSummaries,
    launchPathSummary_name,
    launchPathSummary_id,
    launchPathSummary_tags,

    -- * ListRecordHistorySearchFilter
    ListRecordHistorySearchFilter (..),
    newListRecordHistorySearchFilter,
    listRecordHistorySearchFilter_value,
    listRecordHistorySearchFilter_key,

    -- * ListTagOptionsFilters
    ListTagOptionsFilters (..),
    newListTagOptionsFilters,
    listTagOptionsFilters_value,
    listTagOptionsFilters_active,
    listTagOptionsFilters_key,

    -- * OrganizationNode
    OrganizationNode (..),
    newOrganizationNode,
    organizationNode_value,
    organizationNode_type,

    -- * ParameterConstraints
    ParameterConstraints (..),
    newParameterConstraints,
    parameterConstraints_maxValue,
    parameterConstraints_maxLength,
    parameterConstraints_constraintDescription,
    parameterConstraints_minLength,
    parameterConstraints_allowedPattern,
    parameterConstraints_allowedValues,
    parameterConstraints_minValue,

    -- * PortfolioDetail
    PortfolioDetail (..),
    newPortfolioDetail,
    portfolioDetail_arn,
    portfolioDetail_createdTime,
    portfolioDetail_id,
    portfolioDetail_displayName,
    portfolioDetail_description,
    portfolioDetail_providerName,

    -- * PortfolioShareDetail
    PortfolioShareDetail (..),
    newPortfolioShareDetail,
    portfolioShareDetail_principalId,
    portfolioShareDetail_shareTagOptions,
    portfolioShareDetail_type,
    portfolioShareDetail_accepted,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_principalType,
    principal_principalARN,

    -- * ProductViewAggregationValue
    ProductViewAggregationValue (..),
    newProductViewAggregationValue,
    productViewAggregationValue_value,
    productViewAggregationValue_approximateCount,

    -- * ProductViewDetail
    ProductViewDetail (..),
    newProductViewDetail,
    productViewDetail_status,
    productViewDetail_productViewSummary,
    productViewDetail_createdTime,
    productViewDetail_productARN,

    -- * ProductViewSummary
    ProductViewSummary (..),
    newProductViewSummary,
    productViewSummary_owner,
    productViewSummary_supportUrl,
    productViewSummary_shortDescription,
    productViewSummary_hasDefaultPath,
    productViewSummary_distributor,
    productViewSummary_name,
    productViewSummary_id,
    productViewSummary_type,
    productViewSummary_supportEmail,
    productViewSummary_productId,
    productViewSummary_supportDescription,

    -- * ProvisionedProductAttribute
    ProvisionedProductAttribute (..),
    newProvisionedProductAttribute,
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_status,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_arn,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_name,
    provisionedProductAttribute_lastRecordId,
    provisionedProductAttribute_userArnSession,
    provisionedProductAttribute_id,
    provisionedProductAttribute_type,
    provisionedProductAttribute_physicalId,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_tags,

    -- * ProvisionedProductDetail
    ProvisionedProductDetail (..),
    newProvisionedProductDetail,
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_status,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_arn,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_statusMessage,
    provisionedProductDetail_name,
    provisionedProductDetail_lastRecordId,
    provisionedProductDetail_id,
    provisionedProductDetail_type,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_productId,

    -- * ProvisionedProductPlanDetails
    ProvisionedProductPlanDetails (..),
    newProvisionedProductPlanDetails,
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_provisionProductName,
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_updatedTime,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_tags,

    -- * ProvisionedProductPlanSummary
    ProvisionedProductPlanSummary (..),
    newProvisionedProductPlanSummary,
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_provisioningArtifactId,
    provisionedProductPlanSummary_provisionProductName,
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_planType,

    -- * ProvisioningArtifact
    ProvisioningArtifact (..),
    newProvisioningArtifact,
    provisioningArtifact_createdTime,
    provisioningArtifact_name,
    provisioningArtifact_id,
    provisioningArtifact_guidance,
    provisioningArtifact_description,

    -- * ProvisioningArtifactDetail
    ProvisioningArtifactDetail (..),
    newProvisioningArtifactDetail,
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_type,
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_description,

    -- * ProvisioningArtifactOutput
    ProvisioningArtifactOutput (..),
    newProvisioningArtifactOutput,
    provisioningArtifactOutput_key,
    provisioningArtifactOutput_description,

    -- * ProvisioningArtifactParameter
    ProvisioningArtifactParameter (..),
    newProvisioningArtifactParameter,
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_parameterKey,
    provisioningArtifactParameter_parameterType,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_defaultValue,
    provisioningArtifactParameter_description,

    -- * ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences (..),
    newProvisioningArtifactPreferences,
    provisioningArtifactPreferences_stackSetRegions,
    provisioningArtifactPreferences_stackSetAccounts,

    -- * ProvisioningArtifactProperties
    ProvisioningArtifactProperties (..),
    newProvisioningArtifactProperties,
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_type,
    provisioningArtifactProperties_description,
    provisioningArtifactProperties_info,

    -- * ProvisioningArtifactSummary
    ProvisioningArtifactSummary (..),
    newProvisioningArtifactSummary,
    provisioningArtifactSummary_provisioningArtifactMetadata,
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_description,

    -- * ProvisioningArtifactView
    ProvisioningArtifactView (..),
    newProvisioningArtifactView,
    provisioningArtifactView_productViewSummary,
    provisioningArtifactView_provisioningArtifact,

    -- * ProvisioningParameter
    ProvisioningParameter (..),
    newProvisioningParameter,
    provisioningParameter_value,
    provisioningParameter_key,

    -- * ProvisioningPreferences
    ProvisioningPreferences (..),
    newProvisioningPreferences,
    provisioningPreferences_stackSetRegions,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetFailureTolerancePercentage,
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetMaxConcurrencyCount,

    -- * RecordDetail
    RecordDetail (..),
    newRecordDetail,
    recordDetail_launchRoleArn,
    recordDetail_status,
    recordDetail_recordTags,
    recordDetail_provisionedProductName,
    recordDetail_provisioningArtifactId,
    recordDetail_createdTime,
    recordDetail_recordType,
    recordDetail_recordId,
    recordDetail_provisionedProductType,
    recordDetail_updatedTime,
    recordDetail_pathId,
    recordDetail_provisionedProductId,
    recordDetail_recordErrors,
    recordDetail_productId,

    -- * RecordError
    RecordError (..),
    newRecordError,
    recordError_code,
    recordError_description,

    -- * RecordOutput
    RecordOutput (..),
    newRecordOutput,
    recordOutput_outputValue,
    recordOutput_outputKey,
    recordOutput_description,

    -- * RecordTag
    RecordTag (..),
    newRecordTag,
    recordTag_value,
    recordTag_key,

    -- * ResourceChange
    ResourceChange (..),
    newResourceChange,
    resourceChange_logicalResourceId,
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_action,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_replacement,

    -- * ResourceChangeDetail
    ResourceChangeDetail (..),
    newResourceChangeDetail,
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_target,

    -- * ResourceDetail
    ResourceDetail (..),
    newResourceDetail,
    resourceDetail_arn,
    resourceDetail_createdTime,
    resourceDetail_name,
    resourceDetail_id,
    resourceDetail_description,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    newResourceTargetDefinition,
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,

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
    serviceActionSummary_name,
    serviceActionSummary_id,
    serviceActionSummary_definitionType,
    serviceActionSummary_description,

    -- * ShareDetails
    ShareDetails (..),
    newShareDetails,
    shareDetails_shareErrors,
    shareDetails_successfulShares,

    -- * ShareError
    ShareError (..),
    newShareError,
    shareError_accounts,
    shareError_error,
    shareError_message,

    -- * StackInstance
    StackInstance (..),
    newStackInstance,
    stackInstance_account,
    stackInstance_region,
    stackInstance_stackInstanceStatus,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagOptionDetail
    TagOptionDetail (..),
    newTagOptionDetail,
    tagOptionDetail_value,
    tagOptionDetail_owner,
    tagOptionDetail_active,
    tagOptionDetail_key,
    tagOptionDetail_id,

    -- * TagOptionSummary
    TagOptionSummary (..),
    newTagOptionSummary,
    tagOptionSummary_values,
    tagOptionSummary_key,

    -- * UpdateProvisioningParameter
    UpdateProvisioningParameter (..),
    newUpdateProvisioningParameter,
    updateProvisioningParameter_value,
    updateProvisioningParameter_key,
    updateProvisioningParameter_usePreviousValue,

    -- * UpdateProvisioningPreferences
    UpdateProvisioningPreferences (..),
    newUpdateProvisioningPreferences,
    updateProvisioningPreferences_stackSetRegions,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetOperationType,

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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more parameters provided to the operation are not valid.
_InvalidParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidParametersException"

-- | The specified resource is a duplicate.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"

-- | The operation is not supported.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    defaultService
    "OperationNotSupportedException"

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

-- | An attempt was made to modify a resource that is in a state that is not
-- valid. Check your resources to ensure that they are in valid states
-- before retrying the operation.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The current limits of the service would have been exceeded by this
-- operation. Decrease your resource use or increase your service limits
-- and retry the operation.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | A resource that is currently in use. Ensure that the resource is not in
-- use and retry the operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
