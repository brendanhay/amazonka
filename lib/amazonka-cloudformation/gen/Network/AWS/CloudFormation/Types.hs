{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types
  ( -- * Service Configuration
    cloudFormation,

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
    AccountGateResult,
    accountGateResult,
    agrStatus,
    agrStatusReason,

    -- * AccountLimit
    AccountLimit,
    accountLimit,
    alValue,
    alName,

    -- * AutoDeployment
    AutoDeployment,
    autoDeployment,
    adEnabled,
    adRetainStacksOnAccountRemoval,

    -- * Change
    Change,
    change,
    cResourceChange,
    cType,

    -- * ChangeSetSummary
    ChangeSetSummary,
    changeSetSummary,
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
    DeploymentTargets,
    deploymentTargets,
    dtAccounts,
    dtOrganizationalUnitIds,

    -- * Export
    Export,
    export',
    eValue,
    eExportingStackId,
    eName,

    -- * LoggingConfig
    LoggingConfig,
    loggingConfig,
    lcLogRoleARN,
    lcLogGroupName,

    -- * ModuleInfo
    ModuleInfo,
    moduleInfo,
    miTypeHierarchy,
    miLogicalIdHierarchy,

    -- * Output
    Output,
    output,
    oOutputValue,
    oOutputKey,
    oExportName,
    oDescription,

    -- * Parameter
    Parameter,
    parameter,
    pParameterValue,
    pResolvedValue,
    pParameterKey,
    pUsePreviousValue,

    -- * ParameterConstraints
    ParameterConstraints,
    parameterConstraints,
    pcAllowedValues,

    -- * ParameterDeclaration
    ParameterDeclaration,
    parameterDeclaration,
    pdParameterKey,
    pdParameterType,
    pdParameterConstraints,
    pdDefaultValue,
    pdNoEcho,
    pdDescription,

    -- * PhysicalResourceIdContextKeyValuePair
    PhysicalResourceIdContextKeyValuePair,
    physicalResourceIdContextKeyValuePair,
    prickvpKey,
    prickvpValue,

    -- * PropertyDifference
    PropertyDifference,
    propertyDifference,
    pdPropertyPath,
    pdExpectedValue,
    pdActualValue,
    pdDifferenceType,

    -- * ResourceChange
    ResourceChange,
    resourceChange,
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
    ResourceChangeDetail,
    resourceChangeDetail,
    rcdCausingEntity,
    rcdChangeSource,
    rcdEvaluation,
    rcdTarget,

    -- * ResourceIdentifierSummary
    ResourceIdentifierSummary,
    resourceIdentifierSummary,
    risResourceType,
    risLogicalResourceIds,
    risResourceIdentifiers,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition,
    resourceTargetDefinition,
    rtdAttribute,
    rtdRequiresRecreation,
    rtdName,

    -- * ResourceToImport
    ResourceToImport,
    resourceToImport,
    rtiResourceType,
    rtiLogicalResourceId,
    rtiResourceIdentifier,

    -- * RollbackConfiguration
    RollbackConfiguration,
    rollbackConfiguration,
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,

    -- * RollbackTrigger
    RollbackTrigger,
    rollbackTrigger,
    rtARN,
    rtType,

    -- * Stack
    Stack,
    stack,
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
    StackDriftInformation,
    stackDriftInformation,
    sdiLastCheckTimestamp,
    sdiStackDriftStatus,

    -- * StackDriftInformationSummary
    StackDriftInformationSummary,
    stackDriftInformationSummary,
    sdisLastCheckTimestamp,
    sdisStackDriftStatus,

    -- * StackEvent
    StackEvent,
    stackEvent,
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
    StackInstance,
    stackInstance,
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
    StackInstanceComprehensiveStatus,
    stackInstanceComprehensiveStatus,
    sicsDetailedStatus,

    -- * StackInstanceFilter
    StackInstanceFilter,
    stackInstanceFilter,
    sifValues,
    sifName,

    -- * StackInstanceSummary
    StackInstanceSummary,
    stackInstanceSummary,
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
    StackResource,
    stackResource,
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
    StackResourceDetail,
    stackResourceDetail,
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
    StackResourceDrift,
    stackResourceDrift,
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
    StackResourceDriftInformation,
    stackResourceDriftInformation,
    srdiLastCheckTimestamp,
    srdiStackResourceDriftStatus,

    -- * StackResourceDriftInformationSummary
    StackResourceDriftInformationSummary,
    stackResourceDriftInformationSummary,
    srdisLastCheckTimestamp,
    srdisStackResourceDriftStatus,

    -- * StackResourceSummary
    StackResourceSummary,
    stackResourceSummary,
    srsPhysicalResourceId,
    srsResourceStatusReason,
    srsDriftInformation,
    srsModuleInfo,
    srsLogicalResourceId,
    srsResourceType,
    srsLastUpdatedTimestamp,
    srsResourceStatus,

    -- * StackSet
    StackSet,
    stackSet,
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
    StackSetDriftDetectionDetails,
    stackSetDriftDetectionDetails,
    ssdddLastDriftCheckTimestamp,
    ssdddTotalStackInstancesCount,
    ssdddInProgressStackInstancesCount,
    ssdddDriftedStackInstancesCount,
    ssdddDriftDetectionStatus,
    ssdddDriftStatus,
    ssdddFailedStackInstancesCount,
    ssdddInSyncStackInstancesCount,

    -- * StackSetOperation
    StackSetOperation,
    stackSetOperation,
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
    StackSetOperationPreferences,
    stackSetOperationPreferences,
    ssopRegionOrder,
    ssopMaxConcurrentCount,
    ssopMaxConcurrentPercentage,
    ssopFailureToleranceCount,
    ssopFailureTolerancePercentage,

    -- * StackSetOperationResultSummary
    StackSetOperationResultSummary,
    stackSetOperationResultSummary,
    ssorsStatus,
    ssorsAccount,
    ssorsAccountGateResult,
    ssorsOrganizationalUnitId,
    ssorsRegion,
    ssorsStatusReason,

    -- * StackSetOperationSummary
    StackSetOperationSummary,
    stackSetOperationSummary,
    ssosStatus,
    ssosAction,
    ssosEndTimestamp,
    ssosCreationTimestamp,
    ssosOperationId,

    -- * StackSetSummary
    StackSetSummary,
    stackSetSummary,
    sssStatus,
    sssLastDriftCheckTimestamp,
    sssAutoDeployment,
    sssDriftStatus,
    sssPermissionModel,
    sssStackSetName,
    sssDescription,
    sssStackSetId,

    -- * StackSummary
    StackSummary,
    stackSummary,
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
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TemplateParameter
    TemplateParameter,
    templateParameter,
    tpParameterKey,
    tpDefaultValue,
    tpNoEcho,
    tpDescription,

    -- * TypeSummary
    TypeSummary,
    typeSummary,
    tsLastUpdated,
    tsTypeName,
    tsDefaultVersionId,
    tsTypeARN,
    tsType,
    tsDescription,

    -- * TypeVersionSummary
    TypeVersionSummary,
    typeVersionSummary,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
cloudFormation :: Service
cloudFormation =
  Service
    { _svcAbbrev = "CloudFormation",
      _svcSigner = v4,
      _svcPrefix = "cloudformation",
      _svcVersion = "2010-05-15",
      _svcEndpoint = defaultEndpoint cloudFormation,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "CloudFormation",
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
