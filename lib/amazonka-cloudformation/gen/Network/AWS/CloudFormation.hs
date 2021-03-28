{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CloudFormation__ 
--
-- AWS CloudFormation allows you to create and manage AWS infrastructure deployments predictably and repeatedly. You can use AWS CloudFormation to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple Notification Service, Elastic Load Balancing, and Auto Scaling to build highly-reliable, highly scalable, cost-effective applications without creating or configuring the underlying AWS infrastructure.
-- With AWS CloudFormation, you declare all of your resources and dependencies in a template file. The template defines a collection of resources as a single unit called a stack. AWS CloudFormation creates and deletes all member resources of the stack together and manages all dependencies between the resources for you.
-- For more information about AWS CloudFormation, see the <http://aws.amazon.com/cloudformation/ AWS CloudFormation Product Page> .
-- Amazon CloudFormation makes use of other AWS products. If you need additional technical information about a specific AWS product, you can find the product's technical documentation at <https://docs.aws.amazon.com/ docs.aws.amazon.com> .
module Network.AWS.CloudFormation
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** TypeNotFoundException
    , _TypeNotFoundException

    -- ** CreatedButModifiedException
    , _CreatedButModifiedException

    -- ** ChangeSetNotFoundException
    , _ChangeSetNotFoundException

    -- ** OperationInProgressException
    , _OperationInProgressException

    -- ** InvalidChangeSetStatusException
    , _InvalidChangeSetStatusException

    -- ** OperationNotFoundException
    , _OperationNotFoundException

    -- ** OperationIdAlreadyExistsException
    , _OperationIdAlreadyExistsException

    -- ** InsufficientCapabilitiesException
    , _InsufficientCapabilitiesException

    -- ** TokenAlreadyExistsException
    , _TokenAlreadyExistsException

    -- ** StackSetNotFoundException
    , _StackSetNotFoundException

    -- ** StackInstanceNotFoundException
    , _StackInstanceNotFoundException

    -- ** OperationStatusCheckFailedException
    , _OperationStatusCheckFailedException

    -- ** StackSetNotEmptyException
    , _StackSetNotEmptyException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** InvalidStateTransitionException
    , _InvalidStateTransitionException

    -- ** NameAlreadyExistsException
    , _NameAlreadyExistsException

    -- ** CFNRegistryException
    , _CFNRegistryException

    -- ** StaleRequestException
    , _StaleRequestException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- ** StackImportComplete
    , mkStackImportComplete

    -- ** StackCreateComplete
    , mkStackCreateComplete

    -- ** StackRollbackComplete
    , mkStackRollbackComplete

    -- ** TypeRegistrationComplete
    , mkTypeRegistrationComplete

    -- ** StackUpdateComplete
    , mkStackUpdateComplete

    -- ** StackExists
    , mkStackExists

    -- ** StackDeleteComplete
    , mkStackDeleteComplete

    -- ** ChangeSetCreateComplete
    , mkChangeSetCreateComplete

    -- * Operations
    -- $operations

    -- ** DescribeStackSetOperation 
    , module Network.AWS.CloudFormation.DescribeStackSetOperation

    -- ** DeleteStack 
    , module Network.AWS.CloudFormation.DeleteStack

    -- ** UpdateStack 
    , module Network.AWS.CloudFormation.UpdateStack

    -- ** GetTemplateSummary 
    , module Network.AWS.CloudFormation.GetTemplateSummary

    -- ** ListChangeSets (Paginated)
    , module Network.AWS.CloudFormation.ListChangeSets

    -- ** ListStackResources (Paginated)
    , module Network.AWS.CloudFormation.ListStackResources

    -- ** UpdateStackInstances 
    , module Network.AWS.CloudFormation.UpdateStackInstances

    -- ** DeleteStackInstances 
    , module Network.AWS.CloudFormation.DeleteStackInstances

    -- ** DescribeType 
    , module Network.AWS.CloudFormation.DescribeType

    -- ** CreateStackInstances 
    , module Network.AWS.CloudFormation.CreateStackInstances

    -- ** ListTypeRegistrations 
    , module Network.AWS.CloudFormation.ListTypeRegistrations

    -- ** GetStackPolicy 
    , module Network.AWS.CloudFormation.GetStackPolicy

    -- ** DescribeStacks (Paginated)
    , module Network.AWS.CloudFormation.DescribeStacks

    -- ** CreateChangeSet 
    , module Network.AWS.CloudFormation.CreateChangeSet

    -- ** ListStackSetOperations (Paginated)
    , module Network.AWS.CloudFormation.ListStackSetOperations

    -- ** ExecuteChangeSet 
    , module Network.AWS.CloudFormation.ExecuteChangeSet

    -- ** ListStackInstances (Paginated)
    , module Network.AWS.CloudFormation.ListStackInstances

    -- ** ContinueUpdateRollback 
    , module Network.AWS.CloudFormation.ContinueUpdateRollback

    -- ** ValidateTemplate 
    , module Network.AWS.CloudFormation.ValidateTemplate

    -- ** CancelUpdateStack 
    , module Network.AWS.CloudFormation.CancelUpdateStack

    -- ** ListTypes 
    , module Network.AWS.CloudFormation.ListTypes

    -- ** DescribeTypeRegistration 
    , module Network.AWS.CloudFormation.DescribeTypeRegistration

    -- ** DetectStackDrift 
    , module Network.AWS.CloudFormation.DetectStackDrift

    -- ** DescribeStackEvents (Paginated)
    , module Network.AWS.CloudFormation.DescribeStackEvents

    -- ** SignalResource 
    , module Network.AWS.CloudFormation.SignalResource

    -- ** SetStackPolicy 
    , module Network.AWS.CloudFormation.SetStackPolicy

    -- ** ListImports (Paginated)
    , module Network.AWS.CloudFormation.ListImports

    -- ** DescribeStackResourceDrifts 
    , module Network.AWS.CloudFormation.DescribeStackResourceDrifts

    -- ** ListStacks (Paginated)
    , module Network.AWS.CloudFormation.ListStacks

    -- ** DescribeAccountLimits (Paginated)
    , module Network.AWS.CloudFormation.DescribeAccountLimits

    -- ** DescribeStackResources 
    , module Network.AWS.CloudFormation.DescribeStackResources

    -- ** DescribeStackInstance 
    , module Network.AWS.CloudFormation.DescribeStackInstance

    -- ** CreateStack 
    , module Network.AWS.CloudFormation.CreateStack

    -- ** UpdateStackSet 
    , module Network.AWS.CloudFormation.UpdateStackSet

    -- ** DeleteStackSet 
    , module Network.AWS.CloudFormation.DeleteStackSet

    -- ** EstimateTemplateCost 
    , module Network.AWS.CloudFormation.EstimateTemplateCost

    -- ** DeleteChangeSet 
    , module Network.AWS.CloudFormation.DeleteChangeSet

    -- ** ListStackSets (Paginated)
    , module Network.AWS.CloudFormation.ListStackSets

    -- ** ListExports (Paginated)
    , module Network.AWS.CloudFormation.ListExports

    -- ** DescribeStackDriftDetectionStatus 
    , module Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus

    -- ** CreateStackSet 
    , module Network.AWS.CloudFormation.CreateStackSet

    -- ** DeregisterType 
    , module Network.AWS.CloudFormation.DeregisterType

    -- ** RecordHandlerProgress 
    , module Network.AWS.CloudFormation.RecordHandlerProgress

    -- ** ListTypeVersions 
    , module Network.AWS.CloudFormation.ListTypeVersions

    -- ** SetTypeDefaultVersion 
    , module Network.AWS.CloudFormation.SetTypeDefaultVersion

    -- ** UpdateTerminationProtection 
    , module Network.AWS.CloudFormation.UpdateTerminationProtection

    -- ** GetTemplate 
    , module Network.AWS.CloudFormation.GetTemplate

    -- ** DetectStackSetDrift 
    , module Network.AWS.CloudFormation.DetectStackSetDrift

    -- ** DetectStackResourceDrift 
    , module Network.AWS.CloudFormation.DetectStackResourceDrift

    -- ** DescribeChangeSet (Paginated)
    , module Network.AWS.CloudFormation.DescribeChangeSet

    -- ** DescribeStackSet 
    , module Network.AWS.CloudFormation.DescribeStackSet

    -- ** ListStackSetOperationResults (Paginated)
    , module Network.AWS.CloudFormation.ListStackSetOperationResults

    -- ** RegisterType 
    , module Network.AWS.CloudFormation.RegisterType

    -- ** StopStackSetOperation 
    , module Network.AWS.CloudFormation.StopStackSetOperation

    -- ** DescribeStackResource 
    , module Network.AWS.CloudFormation.DescribeStackResource

    -- * Types

    -- ** ResourceChange
    , ResourceChange (..)
    , mkResourceChange
    , rcAction
    , rcChangeSetId
    , rcDetails
    , rcLogicalResourceId
    , rcModuleInfo
    , rcPhysicalResourceId
    , rcReplacement
    , rcResourceType
    , rcScope

    -- ** OutputValue
    , OutputValue (..)

    -- ** AccountLimit
    , AccountLimit (..)
    , mkAccountLimit
    , alName
    , alValue

    -- ** StackSetOperationResultStatus
    , StackSetOperationResultStatus (..)

    -- ** StackSetOperationResultSummary
    , StackSetOperationResultSummary (..)
    , mkStackSetOperationResultSummary
    , ssorsAccount
    , ssorsAccountGateResult
    , ssorsOrganizationalUnitId
    , ssorsRegion
    , ssorsStatus
    , ssorsStatusReason

    -- ** ChangeSetType
    , ChangeSetType (..)

    -- ** Export
    , Export (..)
    , mkExport
    , eExportingStackId
    , eName
    , eValue

    -- ** StackSet
    , StackSet (..)
    , mkStackSet
    , ssAdministrationRoleARN
    , ssAutoDeployment
    , ssCapabilities
    , ssDescription
    , ssExecutionRoleName
    , ssOrganizationalUnitIds
    , ssParameters
    , ssPermissionModel
    , ssStackSetARN
    , ssStackSetDriftDetectionDetails
    , ssStackSetId
    , ssStackSetName
    , ssStatus
    , ssTags
    , ssTemplateBody

    -- ** StackSetDriftDetectionDetails
    , StackSetDriftDetectionDetails (..)
    , mkStackSetDriftDetectionDetails
    , ssdddDriftDetectionStatus
    , ssdddDriftStatus
    , ssdddDriftedStackInstancesCount
    , ssdddFailedStackInstancesCount
    , ssdddInProgressStackInstancesCount
    , ssdddInSyncStackInstancesCount
    , ssdddLastDriftCheckTimestamp
    , ssdddTotalStackInstancesCount

    -- ** ResourceToImport
    , ResourceToImport (..)
    , mkResourceToImport
    , rtiResourceType
    , rtiLogicalResourceId
    , rtiResourceIdentifier

    -- ** LogicalResourceId
    , LogicalResourceId (..)

    -- ** ChangeSetStatusReason
    , ChangeSetStatusReason (..)

    -- ** StackDriftDetectionStatusReason
    , StackDriftDetectionStatusReason (..)

    -- ** TypeHierarchy
    , TypeHierarchy (..)

    -- ** PermissionModels
    , PermissionModels (..)

    -- ** StackDriftInformation
    , StackDriftInformation (..)
    , mkStackDriftInformation
    , sdiStackDriftStatus
    , sdiLastCheckTimestamp

    -- ** DifferenceType
    , DifferenceType (..)

    -- ** StackDriftStatus
    , StackDriftStatus (..)

    -- ** StackInstanceComprehensiveStatus
    , StackInstanceComprehensiveStatus (..)
    , mkStackInstanceComprehensiveStatus
    , sicsDetailedStatus

    -- ** StackInstanceFilter
    , StackInstanceFilter (..)
    , mkStackInstanceFilter
    , sifName
    , sifValues

    -- ** CausingEntity
    , CausingEntity (..)

    -- ** TypeName
    , TypeName (..)

    -- ** RequestToken
    , RequestToken (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** StackStatus
    , StackStatus (..)

    -- ** PhysicalResourceId
    , PhysicalResourceId (..)

    -- ** RegistrationToken
    , RegistrationToken (..)

    -- ** StackEvent
    , StackEvent (..)
    , mkStackEvent
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp
    , seClientRequestToken
    , seLogicalResourceId
    , sePhysicalResourceId
    , seResourceProperties
    , seResourceStatus
    , seResourceStatusReason
    , seResourceType

    -- ** ResourceType
    , ResourceType (..)

    -- ** StackSummary
    , StackSummary (..)
    , mkStackSummary
    , ssStackName
    , ssCreationTime
    , ssStackStatus
    , ssDeletionTime
    , ssDriftInformation
    , ssLastUpdatedTime
    , ssParentId
    , ssRootId
    , ssStackId
    , ssStackStatusReason
    , ssTemplateDescription

    -- ** ClientToken
    , ClientToken (..)

    -- ** ParameterValue
    , ParameterValue (..)

    -- ** PropertyPath
    , PropertyPath (..)

    -- ** ResourceStatusReason
    , ResourceStatusReason (..)

    -- ** ResourceIdentifierSummary
    , ResourceIdentifierSummary (..)
    , mkResourceIdentifierSummary
    , risLogicalResourceIds
    , risResourceIdentifiers
    , risResourceType

    -- ** ResourceModel
    , ResourceModel (..)

    -- ** Arn
    , Arn (..)

    -- ** RegistryType
    , RegistryType (..)

    -- ** HandlerErrorCode
    , HandlerErrorCode (..)

    -- ** StackSetOperationStatus
    , StackSetOperationStatus (..)

    -- ** StackSetDriftStatus
    , StackSetDriftStatus (..)

    -- ** StackResourceDetail
    , StackResourceDetail (..)
    , mkStackResourceDetail
    , srdLogicalResourceId
    , srdResourceType
    , srdLastUpdatedTimestamp
    , srdResourceStatus
    , srdDescription
    , srdDriftInformation
    , srdMetadata
    , srdModuleInfo
    , srdPhysicalResourceId
    , srdResourceStatusReason
    , srdStackId
    , srdStackName

    -- ** StackSetStatus
    , StackSetStatus (..)

    -- ** StackSetSummary
    , StackSetSummary (..)
    , mkStackSetSummary
    , sssAutoDeployment
    , sssDescription
    , sssDriftStatus
    , sssLastDriftCheckTimestamp
    , sssPermissionModel
    , sssStackSetId
    , sssStackSetName
    , sssStatus

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** PrivateTypeArn
    , PrivateTypeArn (..)

    -- ** RequiresRecreation
    , RequiresRecreation (..)

    -- ** StackInstanceSummary
    , StackInstanceSummary (..)
    , mkStackInstanceSummary
    , sisAccount
    , sisDriftStatus
    , sisLastDriftCheckTimestamp
    , sisOrganizationalUnitId
    , sisRegion
    , sisStackId
    , sisStackInstanceStatus
    , sisStackSetId
    , sisStatus
    , sisStatusReason

    -- ** ResourceProperties
    , ResourceProperties (..)

    -- ** Url
    , Url (..)

    -- ** Value
    , Value (..)

    -- ** ChangeSetNameOrId
    , ChangeSetNameOrId (..)

    -- ** ChangeSource
    , ChangeSource (..)

    -- ** ChangeSetName
    , ChangeSetName (..)

    -- ** AutoDeployment
    , AutoDeployment (..)
    , mkAutoDeployment
    , adEnabled
    , adRetainStacksOnAccountRemoval

    -- ** StackStatusReason
    , StackStatusReason (..)

    -- ** ExecutionStatus
    , ExecutionStatus (..)

    -- ** ParameterKey
    , ParameterKey (..)

    -- ** ResourceAttribute
    , ResourceAttribute (..)

    -- ** StackPolicyBody
    , StackPolicyBody (..)

    -- ** TemplateDescription
    , TemplateDescription (..)

    -- ** ResourceStatus
    , ResourceStatus (..)

    -- ** Change
    , Change (..)
    , mkChange
    , cResourceChange
    , cType

    -- ** EvaluationType
    , EvaluationType (..)

    -- ** Visibility
    , Visibility (..)

    -- ** LogicalIdHierarchy
    , LogicalIdHierarchy (..)

    -- ** TemplateParameter
    , TemplateParameter (..)
    , mkTemplateParameter
    , tpDefaultValue
    , tpDescription
    , tpNoEcho
    , tpParameterKey

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** ParameterType
    , ParameterType (..)

    -- ** ParameterDeclaration
    , ParameterDeclaration (..)
    , mkParameterDeclaration
    , pdDefaultValue
    , pdDescription
    , pdNoEcho
    , pdParameterConstraints
    , pdParameterKey
    , pdParameterType

    -- ** ChangeSetId
    , ChangeSetId (..)

    -- ** StackSetOperation
    , StackSetOperation (..)
    , mkStackSetOperation
    , ssoAction
    , ssoAdministrationRoleARN
    , ssoCreationTimestamp
    , ssoDeploymentTargets
    , ssoEndTimestamp
    , ssoExecutionRoleName
    , ssoOperationId
    , ssoOperationPreferences
    , ssoRetainStacks
    , ssoStackSetDriftDetectionDetails
    , ssoStackSetId
    , ssoStatus

    -- ** ResourceIdentifierPropertyKey
    , ResourceIdentifierPropertyKey (..)

    -- ** StackSetARN
    , StackSetARN (..)

    -- ** StackResourceDriftInformationSummary
    , StackResourceDriftInformationSummary (..)
    , mkStackResourceDriftInformationSummary
    , srdisStackResourceDriftStatus
    , srdisLastCheckTimestamp

    -- ** StackDriftDetectionId
    , StackDriftDetectionId (..)

    -- ** Account
    , Account (..)

    -- ** LogGroupName
    , LogGroupName (..)

    -- ** PropertyValue
    , PropertyValue (..)

    -- ** TypeSummary
    , TypeSummary (..)
    , mkTypeSummary
    , tsDefaultVersionId
    , tsDescription
    , tsLastUpdated
    , tsType
    , tsTypeArn
    , tsTypeName

    -- ** Reason
    , Reason (..)

    -- ** ModuleInfo
    , ModuleInfo (..)
    , mkModuleInfo
    , miLogicalIdHierarchy
    , miTypeHierarchy

    -- ** NextToken
    , NextToken (..)

    -- ** StackPolicyDuringUpdateBody
    , StackPolicyDuringUpdateBody (..)

    -- ** TemplateStage
    , TemplateStage (..)

    -- ** StackResource
    , StackResource (..)
    , mkStackResource
    , srLogicalResourceId
    , srResourceType
    , srTimestamp
    , srResourceStatus
    , srDescription
    , srDriftInformation
    , srModuleInfo
    , srPhysicalResourceId
    , srResourceStatusReason
    , srStackId
    , srStackName

    -- ** Output
    , Output (..)
    , mkOutput
    , oDescription
    , oExportName
    , oOutputKey
    , oOutputValue

    -- ** ParameterConstraints
    , ParameterConstraints (..)
    , mkParameterConstraints
    , pcAllowedValues

    -- ** StackSetOperationAction
    , StackSetOperationAction (..)

    -- ** Key
    , Key (..)

    -- ** StackInstance
    , StackInstance (..)
    , mkStackInstance
    , siAccount
    , siDriftStatus
    , siLastDriftCheckTimestamp
    , siOrganizationalUnitId
    , siParameterOverrides
    , siRegion
    , siStackId
    , siStackInstanceStatus
    , siStackSetId
    , siStatus
    , siStatusReason

    -- ** StatusMessage
    , StatusMessage (..)

    -- ** TypeVersionId
    , TypeVersionId (..)

    -- ** StackPolicyDuringUpdateURL
    , StackPolicyDuringUpdateURL (..)

    -- ** AccountGateResult
    , AccountGateResult (..)
    , mkAccountGateResult
    , agrStatus
    , agrStatusReason

    -- ** AccountGateStatus
    , AccountGateStatus (..)

    -- ** Version
    , Version (..)

    -- ** StackSetDriftDetectionStatus
    , StackSetDriftDetectionStatus (..)

    -- ** CapabilitiesReason
    , CapabilitiesReason (..)

    -- ** ResourceIdentifierPropertyValue
    , ResourceIdentifierPropertyValue (..)

    -- ** DeprecatedStatus
    , DeprecatedStatus (..)

    -- ** TypeArn
    , TypeArn (..)

    -- ** StackResourceDriftInformation
    , StackResourceDriftInformation (..)
    , mkStackResourceDriftInformation
    , srdiStackResourceDriftStatus
    , srdiLastCheckTimestamp

    -- ** ResourceChangeDetail
    , ResourceChangeDetail (..)
    , mkResourceChangeDetail
    , rcdCausingEntity
    , rcdChangeSource
    , rcdEvaluation
    , rcdTarget

    -- ** OrganizationalUnitId
    , OrganizationalUnitId (..)

    -- ** Metadata
    , Metadata (..)

    -- ** StackSetOperationSummary
    , StackSetOperationSummary (..)
    , mkStackSetOperationSummary
    , ssosAction
    , ssosCreationTimestamp
    , ssosEndTimestamp
    , ssosOperationId
    , ssosStatus

    -- ** StackResourceDriftStatus
    , StackResourceDriftStatus (..)

    -- ** StackPolicyURL
    , StackPolicyURL (..)

    -- ** RegistrationStatus
    , RegistrationStatus (..)

    -- ** StackDriftDetectionStatus
    , StackDriftDetectionStatus (..)

    -- ** StackResourceSummary
    , StackResourceSummary (..)
    , mkStackResourceSummary
    , srsLogicalResourceId
    , srsResourceType
    , srsLastUpdatedTimestamp
    , srsResourceStatus
    , srsDriftInformation
    , srsModuleInfo
    , srsPhysicalResourceId
    , srsResourceStatusReason

    -- ** Region
    , Region (..)

    -- ** NotificationARN
    , NotificationARN (..)

    -- ** Capability
    , Capability (..)

    -- ** PropertyDifference
    , PropertyDifference (..)
    , mkPropertyDifference
    , pdPropertyPath
    , pdExpectedValue
    , pdActualValue
    , pdDifferenceType

    -- ** ChangeSetSummary
    , ChangeSetSummary (..)
    , mkChangeSetSummary
    , cssChangeSetId
    , cssChangeSetName
    , cssCreationTime
    , cssDescription
    , cssExecutionStatus
    , cssIncludeNestedStacks
    , cssParentChangeSetId
    , cssRootChangeSetId
    , cssStackId
    , cssStackName
    , cssStatus
    , cssStatusReason

    -- ** Type
    , Type (..)

    -- ** TemplateBody
    , TemplateBody (..)

    -- ** PropertyName
    , PropertyName (..)

    -- ** OutputKey
    , OutputKey (..)

    -- ** StackInstanceFilterName
    , StackInstanceFilterName (..)

    -- ** ChangeSetStatus
    , ChangeSetStatus (..)

    -- ** ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- ** ResourceToSkip
    , ResourceToSkip (..)

    -- ** TemplateURL
    , TemplateURL (..)

    -- ** TransformName
    , TransformName (..)

    -- ** StackInstanceDetailedStatus
    , StackInstanceDetailedStatus (..)

    -- ** DeploymentTargets
    , DeploymentTargets (..)
    , mkDeploymentTargets
    , dtAccounts
    , dtOrganizationalUnitIds

    -- ** StackId
    , StackId (..)

    -- ** StackSetName
    , StackSetName (..)

    -- ** ExportName
    , ExportName (..)

    -- ** PhysicalResourceIdContextKeyValuePair
    , PhysicalResourceIdContextKeyValuePair (..)
    , mkPhysicalResourceIdContextKeyValuePair
    , prickvpKey
    , prickvpValue

    -- ** StackInstanceStatus
    , StackInstanceStatus (..)

    -- ** StackDriftInformationSummary
    , StackDriftInformationSummary (..)
    , mkStackDriftInformationSummary
    , sdisStackDriftStatus
    , sdisLastCheckTimestamp

    -- ** AllowedValue
    , AllowedValue (..)

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** Replacement
    , Replacement (..)

    -- ** TypeVersionSummary
    , TypeVersionSummary (..)
    , mkTypeVersionSummary
    , tvsArn
    , tvsDescription
    , tvsIsDefaultVersion
    , tvsTimeCreated
    , tvsType
    , tvsTypeName
    , tvsVersionId

    -- ** Description
    , Description (..)

    -- ** Stack
    , Stack (..)
    , mkStack
    , sfStackName
    , sfCreationTime
    , sfStackStatus
    , sfCapabilities
    , sfChangeSetId
    , sfDeletionTime
    , sfDescription
    , sfDisableRollback
    , sfDriftInformation
    , sfEnableTerminationProtection
    , sfLastUpdatedTime
    , sfNotificationARNs
    , sfOutputs
    , sfParameters
    , sfParentId
    , sfRoleARN
    , sfRollbackConfiguration
    , sfRootId
    , sfStackId
    , sfStackStatusReason
    , sfTags
    , sfTimeoutInMinutes

    -- ** ChangeType
    , ChangeType (..)

    -- ** RollbackConfiguration
    , RollbackConfiguration (..)
    , mkRollbackConfiguration
    , rcMonitoringTimeInMinutes
    , rcRollbackTriggers

    -- ** ResourceTargetDefinition
    , ResourceTargetDefinition (..)
    , mkResourceTargetDefinition
    , rtdAttribute
    , rtdName
    , rtdRequiresRecreation

    -- ** RollbackTrigger
    , RollbackTrigger (..)
    , mkRollbackTrigger
    , rtArn
    , rtType

    -- ** OnFailure
    , OnFailure (..)

    -- ** StackResourceDrift
    , StackResourceDrift (..)
    , mkStackResourceDrift
    , sStackId
    , sLogicalResourceId
    , sResourceType
    , sStackResourceDriftStatus
    , sTimestamp
    , sActualProperties
    , sExpectedProperties
    , sModuleInfo
    , sPhysicalResourceId
    , sPhysicalResourceIdContext
    , sPropertyDifferences

    -- ** StackSetOperationPreferences
    , StackSetOperationPreferences (..)
    , mkStackSetOperationPreferences
    , ssopFailureToleranceCount
    , ssopFailureTolerancePercentage
    , ssopMaxConcurrentCount
    , ssopMaxConcurrentPercentage
    , ssopRegionOrder

    -- ** Parameter
    , Parameter (..)
    , mkParameter
    , pParameterKey
    , pParameterValue
    , pResolvedValue
    , pUsePreviousValue

    -- ** ProvisioningType
    , ProvisioningType (..)

    -- ** StackSetId
    , StackSetId (..)

    -- ** RoleARN
    , RoleARN (..)

    -- ** LoggingConfig
    , LoggingConfig (..)
    , mkLoggingConfig
    , lcLogRoleArn
    , lcLogGroupName

    -- ** ExecutionRoleName
    , ExecutionRoleName (..)

    -- ** EventId
    , EventId (..)

    -- ** StackName
    , StackName (..)

    -- ** Name
    , Name (..)

    -- ** StatusReason
    , StatusReason (..)

    -- ** ExportingStackId
    , ExportingStackId (..)

    -- ** AdministrationRoleARN
    , AdministrationRoleARN (..)

    -- ** StackInstanceAccount
    , StackInstanceAccount (..)

    -- ** StackInstanceRegion
    , StackInstanceRegion (..)

    -- ** Values
    , Values (..)

    -- ** UniqueId
    , UniqueId (..)

    -- ** ParentId
    , ParentId (..)

    -- ** RootId
    , RootId (..)

    -- ** OperationId
    , OperationId (..)

    -- ** TypeVersionArn
    , TypeVersionArn (..)

    -- ** VersionId
    , VersionId (..)

    -- ** DefaultVersionId
    , DefaultVersionId (..)

    -- ** SchemaHandlerPackage
    , SchemaHandlerPackage (..)

    -- ** ExecutionRoleArn
    , ExecutionRoleArn (..)

    -- ** DocumentationUrl
    , DocumentationUrl (..)

    -- ** Schema
    , Schema (..)

    -- ** SourceUrl
    , SourceUrl (..)

    -- ** ActualProperties
    , ActualProperties (..)

    -- ** ExpectedProperties
    , ExpectedProperties (..)

    -- ** LogRoleArn
    , LogRoleArn (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Waiters
import Network.AWS.CloudFormation.DescribeStackSetOperation
import Network.AWS.CloudFormation.DeleteStack
import Network.AWS.CloudFormation.UpdateStack
import Network.AWS.CloudFormation.GetTemplateSummary
import Network.AWS.CloudFormation.ListChangeSets
import Network.AWS.CloudFormation.ListStackResources
import Network.AWS.CloudFormation.UpdateStackInstances
import Network.AWS.CloudFormation.DeleteStackInstances
import Network.AWS.CloudFormation.DescribeType
import Network.AWS.CloudFormation.CreateStackInstances
import Network.AWS.CloudFormation.ListTypeRegistrations
import Network.AWS.CloudFormation.GetStackPolicy
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.CreateChangeSet
import Network.AWS.CloudFormation.ListStackSetOperations
import Network.AWS.CloudFormation.ExecuteChangeSet
import Network.AWS.CloudFormation.ListStackInstances
import Network.AWS.CloudFormation.ContinueUpdateRollback
import Network.AWS.CloudFormation.ValidateTemplate
import Network.AWS.CloudFormation.CancelUpdateStack
import Network.AWS.CloudFormation.ListTypes
import Network.AWS.CloudFormation.DescribeTypeRegistration
import Network.AWS.CloudFormation.DetectStackDrift
import Network.AWS.CloudFormation.DescribeStackEvents
import Network.AWS.CloudFormation.SignalResource
import Network.AWS.CloudFormation.SetStackPolicy
import Network.AWS.CloudFormation.ListImports
import Network.AWS.CloudFormation.DescribeStackResourceDrifts
import Network.AWS.CloudFormation.ListStacks
import Network.AWS.CloudFormation.DescribeAccountLimits
import Network.AWS.CloudFormation.DescribeStackResources
import Network.AWS.CloudFormation.DescribeStackInstance
import Network.AWS.CloudFormation.CreateStack
import Network.AWS.CloudFormation.UpdateStackSet
import Network.AWS.CloudFormation.DeleteStackSet
import Network.AWS.CloudFormation.EstimateTemplateCost
import Network.AWS.CloudFormation.DeleteChangeSet
import Network.AWS.CloudFormation.ListStackSets
import Network.AWS.CloudFormation.ListExports
import Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus
import Network.AWS.CloudFormation.CreateStackSet
import Network.AWS.CloudFormation.DeregisterType
import Network.AWS.CloudFormation.RecordHandlerProgress
import Network.AWS.CloudFormation.ListTypeVersions
import Network.AWS.CloudFormation.SetTypeDefaultVersion
import Network.AWS.CloudFormation.UpdateTerminationProtection
import Network.AWS.CloudFormation.GetTemplate
import Network.AWS.CloudFormation.DetectStackSetDrift
import Network.AWS.CloudFormation.DetectStackResourceDrift
import Network.AWS.CloudFormation.DescribeChangeSet
import Network.AWS.CloudFormation.DescribeStackSet
import Network.AWS.CloudFormation.ListStackSetOperationResults
import Network.AWS.CloudFormation.RegisterType
import Network.AWS.CloudFormation.StopStackSetOperation
import Network.AWS.CloudFormation.DescribeStackResource
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudFormation'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
