{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.AccessLevelFilter
import Amazonka.ServiceCatalog.Types.AccessLevelFilterKey
import Amazonka.ServiceCatalog.Types.AccessStatus
import Amazonka.ServiceCatalog.Types.BudgetDetail
import Amazonka.ServiceCatalog.Types.ChangeAction
import Amazonka.ServiceCatalog.Types.CloudWatchDashboard
import Amazonka.ServiceCatalog.Types.ConstraintDetail
import Amazonka.ServiceCatalog.Types.ConstraintSummary
import Amazonka.ServiceCatalog.Types.CopyOption
import Amazonka.ServiceCatalog.Types.CopyProductStatus
import Amazonka.ServiceCatalog.Types.DescribePortfolioShareType
import Amazonka.ServiceCatalog.Types.EvaluationType
import Amazonka.ServiceCatalog.Types.ExecutionParameter
import Amazonka.ServiceCatalog.Types.FailedServiceActionAssociation
import Amazonka.ServiceCatalog.Types.LaunchPath
import Amazonka.ServiceCatalog.Types.LaunchPathSummary
import Amazonka.ServiceCatalog.Types.ListRecordHistorySearchFilter
import Amazonka.ServiceCatalog.Types.ListTagOptionsFilters
import Amazonka.ServiceCatalog.Types.OrganizationNode
import Amazonka.ServiceCatalog.Types.OrganizationNodeType
import Amazonka.ServiceCatalog.Types.ParameterConstraints
import Amazonka.ServiceCatalog.Types.PortfolioDetail
import Amazonka.ServiceCatalog.Types.PortfolioShareDetail
import Amazonka.ServiceCatalog.Types.PortfolioShareType
import Amazonka.ServiceCatalog.Types.Principal
import Amazonka.ServiceCatalog.Types.PrincipalType
import Amazonka.ServiceCatalog.Types.ProductSource
import Amazonka.ServiceCatalog.Types.ProductType
import Amazonka.ServiceCatalog.Types.ProductViewAggregationValue
import Amazonka.ServiceCatalog.Types.ProductViewDetail
import Amazonka.ServiceCatalog.Types.ProductViewFilterBy
import Amazonka.ServiceCatalog.Types.ProductViewSortBy
import Amazonka.ServiceCatalog.Types.ProductViewSummary
import Amazonka.ServiceCatalog.Types.PropertyKey
import Amazonka.ServiceCatalog.Types.ProvisionedProductAttribute
import Amazonka.ServiceCatalog.Types.ProvisionedProductDetail
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanDetails
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanStatus
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanSummary
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanType
import Amazonka.ServiceCatalog.Types.ProvisionedProductStatus
import Amazonka.ServiceCatalog.Types.ProvisionedProductViewFilterBy
import Amazonka.ServiceCatalog.Types.ProvisioningArtifact
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactDetail
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactGuidance
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactOutput
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactParameter
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactPreferences
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactProperties
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactPropertyName
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactSummary
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactType
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactView
import Amazonka.ServiceCatalog.Types.ProvisioningParameter
import Amazonka.ServiceCatalog.Types.ProvisioningPreferences
import Amazonka.ServiceCatalog.Types.RecordDetail
import Amazonka.ServiceCatalog.Types.RecordError
import Amazonka.ServiceCatalog.Types.RecordOutput
import Amazonka.ServiceCatalog.Types.RecordStatus
import Amazonka.ServiceCatalog.Types.RecordTag
import Amazonka.ServiceCatalog.Types.Replacement
import Amazonka.ServiceCatalog.Types.RequestStatus
import Amazonka.ServiceCatalog.Types.RequiresRecreation
import Amazonka.ServiceCatalog.Types.ResourceAttribute
import Amazonka.ServiceCatalog.Types.ResourceChange
import Amazonka.ServiceCatalog.Types.ResourceChangeDetail
import Amazonka.ServiceCatalog.Types.ResourceDetail
import Amazonka.ServiceCatalog.Types.ResourceTargetDefinition
import Amazonka.ServiceCatalog.Types.ServiceActionAssociation
import Amazonka.ServiceCatalog.Types.ServiceActionAssociationErrorCode
import Amazonka.ServiceCatalog.Types.ServiceActionDefinitionKey
import Amazonka.ServiceCatalog.Types.ServiceActionDefinitionType
import Amazonka.ServiceCatalog.Types.ServiceActionDetail
import Amazonka.ServiceCatalog.Types.ServiceActionSummary
import Amazonka.ServiceCatalog.Types.ShareDetails
import Amazonka.ServiceCatalog.Types.ShareError
import Amazonka.ServiceCatalog.Types.ShareStatus
import Amazonka.ServiceCatalog.Types.SortOrder
import Amazonka.ServiceCatalog.Types.StackInstance
import Amazonka.ServiceCatalog.Types.StackInstanceStatus
import Amazonka.ServiceCatalog.Types.StackSetOperationType
import Amazonka.ServiceCatalog.Types.Tag
import Amazonka.ServiceCatalog.Types.TagOptionDetail
import Amazonka.ServiceCatalog.Types.TagOptionSummary
import Amazonka.ServiceCatalog.Types.UpdateProvisioningParameter
import Amazonka.ServiceCatalog.Types.UpdateProvisioningPreferences
import Amazonka.ServiceCatalog.Types.UsageInstruction
import qualified Amazonka.Sign.V4 as Sign

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
