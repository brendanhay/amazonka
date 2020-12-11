-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types
  ( -- * Service configuration
    cloudFormationService,

    -- * Errors

    -- * AccountGateStatus
    AccountGateStatus (..),

    -- * Capability
    Capability (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * ChangeSetStatus
    ChangeSetStatus (..),

    -- * ChangeSetType
    ChangeSetType (..),

    -- * ChangeSource
    ChangeSource (..),

    -- * ChangeType
    ChangeType (..),

    -- * DeprecatedStatus
    DeprecatedStatus (..),

    -- * DifferenceType
    DifferenceType (..),

    -- * EvaluationType
    EvaluationType (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * HandlerErrorCode
    HandlerErrorCode (..),

    -- * OnFailure
    OnFailure (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * PermissionModels
    PermissionModels (..),

    -- * ProvisioningType
    ProvisioningType (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * RegistryType
    RegistryType (..),

    -- * Replacement
    Replacement (..),

    -- * RequiresRecreation
    RequiresRecreation (..),

    -- * ResourceAttribute
    ResourceAttribute (..),

    -- * ResourceSignalStatus
    ResourceSignalStatus (..),

    -- * ResourceStatus
    ResourceStatus (..),

    -- * StackDriftDetectionStatus
    StackDriftDetectionStatus (..),

    -- * StackDriftStatus
    StackDriftStatus (..),

    -- * StackInstanceDetailedStatus
    StackInstanceDetailedStatus (..),

    -- * StackInstanceFilterName
    StackInstanceFilterName (..),

    -- * StackInstanceStatus
    StackInstanceStatus (..),

    -- * StackResourceDriftStatus
    StackResourceDriftStatus (..),

    -- * StackSetDriftDetectionStatus
    StackSetDriftDetectionStatus (..),

    -- * StackSetDriftStatus
    StackSetDriftStatus (..),

    -- * StackSetOperationAction
    StackSetOperationAction (..),

    -- * StackSetOperationResultStatus
    StackSetOperationResultStatus (..),

    -- * StackSetOperationStatus
    StackSetOperationStatus (..),

    -- * StackSetStatus
    StackSetStatus (..),

    -- * StackStatus
    StackStatus (..),

    -- * TemplateStage
    TemplateStage (..),

    -- * Visibility
    Visibility (..),

    -- * AccountGateResult
    AccountGateResult (..),
    mkAccountGateResult,
    agrStatus,
    agrStatusReason,

    -- * AccountLimit
    AccountLimit (..),
    mkAccountLimit,
    alValue,
    alName,

    -- * AutoDeployment
    AutoDeployment (..),
    mkAutoDeployment,
    adEnabled,
    adRetainStacksOnAccountRemoval,

    -- * Change
    Change (..),
    mkChange,
    cResourceChange,
    cType,

    -- * ChangeSetSummary
    ChangeSetSummary (..),
    mkChangeSetSummary,
    cCreationTime,
    cStatus,
    cParentChangeSetId,
    cChangeSetName,
    cExecutionStatus,
    cChangeSetId,
    cIncludeNestedStacks,
    cRootChangeSetId,
    cStatusReason,
    cStackId,
    cDescription,
    cStackName,

    -- * DeploymentTargets
    DeploymentTargets (..),
    mkDeploymentTargets,
    dtAccounts,
    dtOrganizationalUnitIds,

    -- * Export
    Export (..),
    mkExport,
    eValue,
    eExportingStackId,
    eName,

    -- * LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcLogRoleARN,
    lcLogGroupName,

    -- * ModuleInfo
    ModuleInfo (..),
    mkModuleInfo,
    miTypeHierarchy,
    miLogicalIdHierarchy,

    -- * Output
    Output (..),
    mkOutput,
    oOutputValue,
    oOutputKey,
    oExportName,
    oDescription,

    -- * Parameter
    Parameter (..),
    mkParameter,
    pParameterValue,
    pResolvedValue,
    pParameterKey,
    pUsePreviousValue,

    -- * ParameterConstraints
    ParameterConstraints (..),
    mkParameterConstraints,
    pcAllowedValues,

    -- * ParameterDeclaration
    ParameterDeclaration (..),
    mkParameterDeclaration,
    pdParameterKey,
    pdParameterType,
    pdParameterConstraints,
    pdDefaultValue,
    pdNoEcho,
    pdDescription,

    -- * PhysicalResourceIdContextKeyValuePair
    PhysicalResourceIdContextKeyValuePair (..),
    mkPhysicalResourceIdContextKeyValuePair,
    prickvpKey,
    prickvpValue,

    -- * PropertyDifference
    PropertyDifference (..),
    mkPropertyDifference,
    pdPropertyPath,
    pdExpectedValue,
    pdActualValue,
    pdDifferenceType,

    -- * ResourceChange
    ResourceChange (..),
    mkResourceChange,
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcResourceType,
    rcAction,
    rcChangeSetId,
    rcModuleInfo,
    rcScope,
    rcDetails,
    rcReplacement,

    -- * ResourceChangeDetail
    ResourceChangeDetail (..),
    mkResourceChangeDetail,
    rcdCausingEntity,
    rcdChangeSource,
    rcdEvaluation,
    rcdTarget,

    -- * ResourceIdentifierSummary
    ResourceIdentifierSummary (..),
    mkResourceIdentifierSummary,
    risResourceType,
    risLogicalResourceIds,
    risResourceIdentifiers,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    mkResourceTargetDefinition,
    rtdAttribute,
    rtdRequiresRecreation,
    rtdName,

    -- * ResourceToImport
    ResourceToImport (..),
    mkResourceToImport,
    rtiResourceType,
    rtiLogicalResourceId,
    rtiResourceIdentifier,

    -- * RollbackConfiguration
    RollbackConfiguration (..),
    mkRollbackConfiguration,
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,

    -- * RollbackTrigger
    RollbackTrigger (..),
    mkRollbackTrigger,
    rtARN,
    rtType,

    -- * Stack
    Stack (..),
    mkStack,
    staDisableRollback,
    staLastUpdatedTime,
    staRootId,
    staNotificationARNs,
    staStackStatusReason,
    staEnableTerminationProtection,
    staDriftInformation,
    staChangeSetId,
    staDeletionTime,
    staOutputs,
    staParameters,
    staStackId,
    staDescription,
    staCapabilities,
    staRollbackConfiguration,
    staTags,
    staTimeoutInMinutes,
    staParentId,
    staRoleARN,
    staStackName,
    staCreationTime,
    staStackStatus,

    -- * StackDriftInformation
    StackDriftInformation (..),
    mkStackDriftInformation,
    sdiLastCheckTimestamp,
    sdiStackDriftStatus,

    -- * StackDriftInformationSummary
    StackDriftInformationSummary (..),
    mkStackDriftInformationSummary,
    sdisLastCheckTimestamp,
    sdisStackDriftStatus,

    -- * StackEvent
    StackEvent (..),
    mkStackEvent,
    seLogicalResourceId,
    sePhysicalResourceId,
    seResourceType,
    seResourceStatusReason,
    seResourceProperties,
    seResourceStatus,
    seClientRequestToken,
    seStackId,
    seEventId,
    seStackName,
    seTimestamp,

    -- * StackInstance
    StackInstance (..),
    mkStackInstance,
    siStatus,
    siLastDriftCheckTimestamp,
    siAccount,
    siDriftStatus,
    siOrganizationalUnitId,
    siRegion,
    siStatusReason,
    siStackId,
    siStackInstanceStatus,
    siParameterOverrides,
    siStackSetId,

    -- * StackInstanceComprehensiveStatus
    StackInstanceComprehensiveStatus (..),
    mkStackInstanceComprehensiveStatus,
    sicsDetailedStatus,

    -- * StackInstanceFilter
    StackInstanceFilter (..),
    mkStackInstanceFilter,
    sifValues,
    sifName,

    -- * StackInstanceSummary
    StackInstanceSummary (..),
    mkStackInstanceSummary,
    sisStatus,
    sisLastDriftCheckTimestamp,
    sisAccount,
    sisDriftStatus,
    sisOrganizationalUnitId,
    sisRegion,
    sisStatusReason,
    sisStackId,
    sisStackInstanceStatus,
    sisStackSetId,

    -- * StackResource
    StackResource (..),
    mkStackResource,
    srPhysicalResourceId,
    srResourceStatusReason,
    srDriftInformation,
    srModuleInfo,
    srStackId,
    srDescription,
    srStackName,
    srLogicalResourceId,
    srResourceType,
    srTimestamp,
    srResourceStatus,

    -- * StackResourceDetail
    StackResourceDetail (..),
    mkStackResourceDetail,
    sPhysicalResourceId,
    sResourceStatusReason,
    sDriftInformation,
    sModuleInfo,
    sMetadata,
    sStackId,
    sDescription,
    sStackName,
    sLogicalResourceId,
    sResourceType,
    sLastUpdatedTimestamp,
    sResourceStatus,

    -- * StackResourceDrift
    StackResourceDrift (..),
    mkStackResourceDrift,
    srdActualProperties,
    srdPhysicalResourceId,
    srdPhysicalResourceIdContext,
    srdPropertyDifferences,
    srdModuleInfo,
    srdExpectedProperties,
    srdStackId,
    srdLogicalResourceId,
    srdResourceType,
    srdStackResourceDriftStatus,
    srdTimestamp,

    -- * StackResourceDriftInformation
    StackResourceDriftInformation (..),
    mkStackResourceDriftInformation,
    srdiLastCheckTimestamp,
    srdiStackResourceDriftStatus,

    -- * StackResourceDriftInformationSummary
    StackResourceDriftInformationSummary (..),
    mkStackResourceDriftInformationSummary,
    srdisLastCheckTimestamp,
    srdisStackResourceDriftStatus,

    -- * StackResourceSummary
    StackResourceSummary (..),
    mkStackResourceSummary,
    srsPhysicalResourceId,
    srsResourceStatusReason,
    srsDriftInformation,
    srsModuleInfo,
    srsLogicalResourceId,
    srsResourceType,
    srsLastUpdatedTimestamp,
    srsResourceStatus,

    -- * StackSet
    StackSet (..),
    mkStackSet,
    ssStackSetDriftDetectionDetails,
    ssStatus,
    ssAdministrationRoleARN,
    ssAutoDeployment,
    ssOrganizationalUnitIds,
    ssStackSetARN,
    ssPermissionModel,
    ssParameters,
    ssTemplateBody,
    ssStackSetName,
    ssDescription,
    ssCapabilities,
    ssTags,
    ssStackSetId,
    ssExecutionRoleName,

    -- * StackSetDriftDetectionDetails
    StackSetDriftDetectionDetails (..),
    mkStackSetDriftDetectionDetails,
    ssdddLastDriftCheckTimestamp,
    ssdddTotalStackInstancesCount,
    ssdddInProgressStackInstancesCount,
    ssdddDriftedStackInstancesCount,
    ssdddDriftDetectionStatus,
    ssdddDriftStatus,
    ssdddFailedStackInstancesCount,
    ssdddInSyncStackInstancesCount,

    -- * StackSetOperation
    StackSetOperation (..),
    mkStackSetOperation,
    ssoStackSetDriftDetectionDetails,
    ssoStatus,
    ssoAdministrationRoleARN,
    ssoAction,
    ssoEndTimestamp,
    ssoCreationTimestamp,
    ssoOperationPreferences,
    ssoOperationId,
    ssoRetainStacks,
    ssoDeploymentTargets,
    ssoStackSetId,
    ssoExecutionRoleName,

    -- * StackSetOperationPreferences
    StackSetOperationPreferences (..),
    mkStackSetOperationPreferences,
    ssopRegionOrder,
    ssopMaxConcurrentCount,
    ssopMaxConcurrentPercentage,
    ssopFailureToleranceCount,
    ssopFailureTolerancePercentage,

    -- * StackSetOperationResultSummary
    StackSetOperationResultSummary (..),
    mkStackSetOperationResultSummary,
    ssorsStatus,
    ssorsAccount,
    ssorsAccountGateResult,
    ssorsOrganizationalUnitId,
    ssorsRegion,
    ssorsStatusReason,

    -- * StackSetOperationSummary
    StackSetOperationSummary (..),
    mkStackSetOperationSummary,
    ssosStatus,
    ssosAction,
    ssosEndTimestamp,
    ssosCreationTimestamp,
    ssosOperationId,

    -- * StackSetSummary
    StackSetSummary (..),
    mkStackSetSummary,
    sssStatus,
    sssLastDriftCheckTimestamp,
    sssAutoDeployment,
    sssDriftStatus,
    sssPermissionModel,
    sssStackSetName,
    sssDescription,
    sssStackSetId,

    -- * StackSummary
    StackSummary (..),
    mkStackSummary,
    ssLastUpdatedTime,
    ssRootId,
    ssStackStatusReason,
    ssTemplateDescription,
    ssDriftInformation,
    ssDeletionTime,
    ssStackId,
    ssParentId,
    ssStackName,
    ssCreationTime,
    ssStackStatus,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TemplateParameter
    TemplateParameter (..),
    mkTemplateParameter,
    tpParameterKey,
    tpDefaultValue,
    tpNoEcho,
    tpDescription,

    -- * TypeSummary
    TypeSummary (..),
    mkTypeSummary,
    tsLastUpdated,
    tsTypeName,
    tsDefaultVersionId,
    tsTypeARN,
    tsType,
    tsDescription,

    -- * TypeVersionSummary
    TypeVersionSummary (..),
    mkTypeVersionSummary,
    tvsVersionId,
    tvsTypeName,
    tvsARN,
    tvsTimeCreated,
    tvsType,
    tvsIsDefaultVersion,
    tvsDescription,
  )
where

import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.AccountGateStatus
import Network.AWS.CloudFormation.Types.AccountLimit
import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Change
import Network.AWS.CloudFormation.Types.ChangeAction
import Network.AWS.CloudFormation.Types.ChangeSetStatus
import Network.AWS.CloudFormation.Types.ChangeSetSummary
import Network.AWS.CloudFormation.Types.ChangeSetType
import Network.AWS.CloudFormation.Types.ChangeSource
import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.DeploymentTargets
import Network.AWS.CloudFormation.Types.DeprecatedStatus
import Network.AWS.CloudFormation.Types.DifferenceType
import Network.AWS.CloudFormation.Types.EvaluationType
import Network.AWS.CloudFormation.Types.ExecutionStatus
import Network.AWS.CloudFormation.Types.Export
import Network.AWS.CloudFormation.Types.HandlerErrorCode
import Network.AWS.CloudFormation.Types.LoggingConfig
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.OnFailure
import Network.AWS.CloudFormation.Types.OperationStatus
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.ParameterConstraints
import Network.AWS.CloudFormation.Types.ParameterDeclaration
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Network.AWS.CloudFormation.Types.PropertyDifference
import Network.AWS.CloudFormation.Types.ProvisioningType
import Network.AWS.CloudFormation.Types.RegistrationStatus
import Network.AWS.CloudFormation.Types.RegistryType
import Network.AWS.CloudFormation.Types.Replacement
import Network.AWS.CloudFormation.Types.RequiresRecreation
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.CloudFormation.Types.ResourceChange
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
import Network.AWS.CloudFormation.Types.ResourceSignalStatus
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.ResourceTargetDefinition
import Network.AWS.CloudFormation.Types.ResourceToImport
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.RollbackTrigger
import Network.AWS.CloudFormation.Types.Stack
import Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackEvent
import Network.AWS.CloudFormation.Types.StackInstance
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
import Network.AWS.CloudFormation.Types.StackInstanceFilter
import Network.AWS.CloudFormation.Types.StackInstanceFilterName
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import Network.AWS.CloudFormation.Types.StackInstanceSummary
import Network.AWS.CloudFormation.Types.StackResource
import Network.AWS.CloudFormation.Types.StackResourceDetail
import Network.AWS.CloudFormation.Types.StackResourceDrift
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import Network.AWS.CloudFormation.Types.StackResourceSummary
import Network.AWS.CloudFormation.Types.StackSet
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackSetDriftStatus
import Network.AWS.CloudFormation.Types.StackSetOperation
import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationPreferences
import Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
import Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import Network.AWS.CloudFormation.Types.StackSetOperationSummary
import Network.AWS.CloudFormation.Types.StackSetStatus
import Network.AWS.CloudFormation.Types.StackSetSummary
import Network.AWS.CloudFormation.Types.StackStatus
import Network.AWS.CloudFormation.Types.StackSummary
import Network.AWS.CloudFormation.Types.Tag
import Network.AWS.CloudFormation.Types.TemplateParameter
import Network.AWS.CloudFormation.Types.TemplateStage
import Network.AWS.CloudFormation.Types.TypeSummary
import Network.AWS.CloudFormation.Types.TypeVersionSummary
import Network.AWS.CloudFormation.Types.Visibility
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
cloudFormationService :: Lude.Service
cloudFormationService =
  Lude.Service
    { Lude._svcAbbrev = "CloudFormation",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloudformation",
      Lude._svcVersion = "2010-05-15",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudFormationService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "CloudFormation",
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
