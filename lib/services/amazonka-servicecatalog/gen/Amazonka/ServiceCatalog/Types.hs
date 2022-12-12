{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DuplicateResourceException,
    _InvalidParametersException,
    _InvalidStateException,
    _LimitExceededException,
    _OperationNotSupportedException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _TagOptionNotMigratedException,

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

    -- * LastSyncStatus
    LastSyncStatus (..),

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

    -- * SourceType
    SourceType (..),

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

    -- * CodeStarParameters
    CodeStarParameters (..),
    newCodeStarParameters,
    codeStarParameters_connectionArn,
    codeStarParameters_repository,
    codeStarParameters_branch,
    codeStarParameters_artifactPath,

    -- * ConstraintDetail
    ConstraintDetail (..),
    newConstraintDetail,
    constraintDetail_constraintId,
    constraintDetail_description,
    constraintDetail_owner,
    constraintDetail_portfolioId,
    constraintDetail_productId,
    constraintDetail_type,

    -- * ConstraintSummary
    ConstraintSummary (..),
    newConstraintSummary,
    constraintSummary_description,
    constraintSummary_type,

    -- * ExecutionParameter
    ExecutionParameter (..),
    newExecutionParameter,
    executionParameter_defaultValues,
    executionParameter_name,
    executionParameter_type,

    -- * FailedServiceActionAssociation
    FailedServiceActionAssociation (..),
    newFailedServiceActionAssociation,
    failedServiceActionAssociation_errorCode,
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_productId,
    failedServiceActionAssociation_provisioningArtifactId,
    failedServiceActionAssociation_serviceActionId,

    -- * LastSync
    LastSync (..),
    newLastSync,
    lastSync_lastSuccessfulSyncProvisioningArtifactId,
    lastSync_lastSuccessfulSyncTime,
    lastSync_lastSyncStatus,
    lastSync_lastSyncStatusMessage,
    lastSync_lastSyncTime,

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
    listTagOptionsFilters_active,
    listTagOptionsFilters_key,
    listTagOptionsFilters_value,

    -- * OrganizationNode
    OrganizationNode (..),
    newOrganizationNode,
    organizationNode_type,
    organizationNode_value,

    -- * ParameterConstraints
    ParameterConstraints (..),
    newParameterConstraints,
    parameterConstraints_allowedPattern,
    parameterConstraints_allowedValues,
    parameterConstraints_constraintDescription,
    parameterConstraints_maxLength,
    parameterConstraints_maxValue,
    parameterConstraints_minLength,
    parameterConstraints_minValue,

    -- * PortfolioDetail
    PortfolioDetail (..),
    newPortfolioDetail,
    portfolioDetail_arn,
    portfolioDetail_createdTime,
    portfolioDetail_description,
    portfolioDetail_displayName,
    portfolioDetail_id,
    portfolioDetail_providerName,

    -- * PortfolioShareDetail
    PortfolioShareDetail (..),
    newPortfolioShareDetail,
    portfolioShareDetail_accepted,
    portfolioShareDetail_principalId,
    portfolioShareDetail_sharePrincipals,
    portfolioShareDetail_shareTagOptions,
    portfolioShareDetail_type,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_principalARN,
    principal_principalType,

    -- * ProductViewAggregationValue
    ProductViewAggregationValue (..),
    newProductViewAggregationValue,
    productViewAggregationValue_approximateCount,
    productViewAggregationValue_value,

    -- * ProductViewDetail
    ProductViewDetail (..),
    newProductViewDetail,
    productViewDetail_createdTime,
    productViewDetail_productARN,
    productViewDetail_productViewSummary,
    productViewDetail_sourceConnection,
    productViewDetail_status,

    -- * ProductViewSummary
    ProductViewSummary (..),
    newProductViewSummary,
    productViewSummary_distributor,
    productViewSummary_hasDefaultPath,
    productViewSummary_id,
    productViewSummary_name,
    productViewSummary_owner,
    productViewSummary_productId,
    productViewSummary_shortDescription,
    productViewSummary_supportDescription,
    productViewSummary_supportEmail,
    productViewSummary_supportUrl,
    productViewSummary_type,

    -- * ProvisionedProductAttribute
    ProvisionedProductAttribute (..),
    newProvisionedProductAttribute,
    provisionedProductAttribute_arn,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_id,
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_lastRecordId,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_name,
    provisionedProductAttribute_physicalId,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_status,
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_tags,
    provisionedProductAttribute_type,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_userArnSession,

    -- * ProvisionedProductDetail
    ProvisionedProductDetail (..),
    newProvisionedProductDetail,
    provisionedProductDetail_arn,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_id,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_lastRecordId,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_name,
    provisionedProductDetail_productId,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_status,
    provisionedProductDetail_statusMessage,
    provisionedProductDetail_type,

    -- * ProvisionedProductPlanDetails
    ProvisionedProductPlanDetails (..),
    newProvisionedProductPlanDetails,
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_provisionProductName,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_tags,
    provisionedProductPlanDetails_updatedTime,

    -- * ProvisionedProductPlanSummary
    ProvisionedProductPlanSummary (..),
    newProvisionedProductPlanSummary,
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_planType,
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_provisionProductName,
    provisionedProductPlanSummary_provisioningArtifactId,

    -- * ProvisioningArtifact
    ProvisioningArtifact (..),
    newProvisioningArtifact,
    provisioningArtifact_createdTime,
    provisioningArtifact_description,
    provisioningArtifact_guidance,
    provisioningArtifact_id,
    provisioningArtifact_name,

    -- * ProvisioningArtifactDetail
    ProvisioningArtifactDetail (..),
    newProvisioningArtifactDetail,
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_description,
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_sourceRevision,
    provisioningArtifactDetail_type,

    -- * ProvisioningArtifactOutput
    ProvisioningArtifactOutput (..),
    newProvisioningArtifactOutput,
    provisioningArtifactOutput_description,
    provisioningArtifactOutput_key,

    -- * ProvisioningArtifactParameter
    ProvisioningArtifactParameter (..),
    newProvisioningArtifactParameter,
    provisioningArtifactParameter_defaultValue,
    provisioningArtifactParameter_description,
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_parameterKey,
    provisioningArtifactParameter_parameterType,

    -- * ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences (..),
    newProvisioningArtifactPreferences,
    provisioningArtifactPreferences_stackSetAccounts,
    provisioningArtifactPreferences_stackSetRegions,

    -- * ProvisioningArtifactProperties
    ProvisioningArtifactProperties (..),
    newProvisioningArtifactProperties,
    provisioningArtifactProperties_description,
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_info,
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_type,

    -- * ProvisioningArtifactSummary
    ProvisioningArtifactSummary (..),
    newProvisioningArtifactSummary,
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_description,
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_provisioningArtifactMetadata,

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
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetFailureTolerancePercentage,
    provisioningPreferences_stackSetMaxConcurrencyCount,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,
    provisioningPreferences_stackSetRegions,

    -- * RecordDetail
    RecordDetail (..),
    newRecordDetail,
    recordDetail_createdTime,
    recordDetail_launchRoleArn,
    recordDetail_pathId,
    recordDetail_productId,
    recordDetail_provisionedProductId,
    recordDetail_provisionedProductName,
    recordDetail_provisionedProductType,
    recordDetail_provisioningArtifactId,
    recordDetail_recordErrors,
    recordDetail_recordId,
    recordDetail_recordTags,
    recordDetail_recordType,
    recordDetail_status,
    recordDetail_updatedTime,

    -- * RecordError
    RecordError (..),
    newRecordError,
    recordError_code,
    recordError_description,

    -- * RecordOutput
    RecordOutput (..),
    newRecordOutput,
    recordOutput_description,
    recordOutput_outputKey,
    recordOutput_outputValue,

    -- * RecordTag
    RecordTag (..),
    newRecordTag,
    recordTag_key,
    recordTag_value,

    -- * ResourceChange
    ResourceChange (..),
    newResourceChange,
    resourceChange_action,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_physicalResourceId,
    resourceChange_replacement,
    resourceChange_resourceType,
    resourceChange_scope,

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
    resourceDetail_description,
    resourceDetail_id,
    resourceDetail_name,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    newResourceTargetDefinition,
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_name,
    resourceTargetDefinition_requiresRecreation,

    -- * ServiceActionAssociation
    ServiceActionAssociation (..),
    newServiceActionAssociation,
    serviceActionAssociation_serviceActionId,
    serviceActionAssociation_productId,
    serviceActionAssociation_provisioningArtifactId,

    -- * ServiceActionDetail
    ServiceActionDetail (..),
    newServiceActionDetail,
    serviceActionDetail_definition,
    serviceActionDetail_serviceActionSummary,

    -- * ServiceActionSummary
    ServiceActionSummary (..),
    newServiceActionSummary,
    serviceActionSummary_definitionType,
    serviceActionSummary_description,
    serviceActionSummary_id,
    serviceActionSummary_name,

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

    -- * SourceConnection
    SourceConnection (..),
    newSourceConnection,
    sourceConnection_type,
    sourceConnection_connectionParameters,

    -- * SourceConnectionDetail
    SourceConnectionDetail (..),
    newSourceConnectionDetail,
    sourceConnectionDetail_connectionParameters,
    sourceConnectionDetail_lastSync,
    sourceConnectionDetail_type,

    -- * SourceConnectionParameters
    SourceConnectionParameters (..),
    newSourceConnectionParameters,
    sourceConnectionParameters_codeStar,

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
    tagOptionDetail_active,
    tagOptionDetail_id,
    tagOptionDetail_key,
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
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,
    updateProvisioningPreferences_stackSetOperationType,
    updateProvisioningPreferences_stackSetRegions,

    -- * UsageInstruction
    UsageInstruction (..),
    newUsageInstruction,
    usageInstruction_type,
    usageInstruction_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.AccessLevelFilter
import Amazonka.ServiceCatalog.Types.AccessLevelFilterKey
import Amazonka.ServiceCatalog.Types.AccessStatus
import Amazonka.ServiceCatalog.Types.BudgetDetail
import Amazonka.ServiceCatalog.Types.ChangeAction
import Amazonka.ServiceCatalog.Types.CloudWatchDashboard
import Amazonka.ServiceCatalog.Types.CodeStarParameters
import Amazonka.ServiceCatalog.Types.ConstraintDetail
import Amazonka.ServiceCatalog.Types.ConstraintSummary
import Amazonka.ServiceCatalog.Types.CopyOption
import Amazonka.ServiceCatalog.Types.CopyProductStatus
import Amazonka.ServiceCatalog.Types.DescribePortfolioShareType
import Amazonka.ServiceCatalog.Types.EvaluationType
import Amazonka.ServiceCatalog.Types.ExecutionParameter
import Amazonka.ServiceCatalog.Types.FailedServiceActionAssociation
import Amazonka.ServiceCatalog.Types.LastSync
import Amazonka.ServiceCatalog.Types.LastSyncStatus
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
import Amazonka.ServiceCatalog.Types.SourceConnection
import Amazonka.ServiceCatalog.Types.SourceConnectionDetail
import Amazonka.ServiceCatalog.Types.SourceConnectionParameters
import Amazonka.ServiceCatalog.Types.SourceType
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
    { Core.abbrev = "ServiceCatalog",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "servicecatalog",
      Core.signingName = "servicecatalog",
      Core.version = "2015-12-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ServiceCatalog",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource is a duplicate.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"

-- | One or more parameters provided to the operation are not valid.
_InvalidParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidParametersException"

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

-- | The operation is not supported.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    defaultService
    "OperationNotSupportedException"

-- | A resource that is currently in use. Ensure that the resource is not in
-- use and retry the operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | An operation requiring TagOptions failed because the TagOptions
-- migration process has not been performed for this account. Use the
-- Amazon Web Services Management Console to perform the migration process
-- before retrying the operation.
_TagOptionNotMigratedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagOptionNotMigratedException =
  Core._MatchServiceError
    defaultService
    "TagOptionNotMigratedException"
