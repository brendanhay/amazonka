{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CloudFormation__
--
-- AWS CloudFormation allows you to create and manage AWS infrastructure deployments predictably and repeatedly. You can use AWS CloudFormation to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple Notification Service, Elastic Load Balancing, and Auto Scaling to build highly-reliable, highly scalable, cost-effective applications without creating or configuring the underlying AWS infrastructure.
--
-- With AWS CloudFormation, you declare all of your resources and dependencies in a template file. The template defines a collection of resources as a single unit called a stack. AWS CloudFormation creates and deletes all member resources of the stack together and manages all dependencies between the resources for you.
--
-- For more information about AWS CloudFormation, see the <http://aws.amazon.com/cloudformation/ AWS CloudFormation Product Page> .
--
-- Amazon CloudFormation makes use of other AWS products. If you need additional technical information about a specific AWS product, you can find the product's technical documentation at <http://docs.aws.amazon.com/ docs.aws.amazon.com> .
--
-- /APIs for stacks /
--
-- When you use AWS CloudFormation, you manage related resources as a single unit called a stack. You create, update, and delete a collection of resources by creating, updating, and deleting stacks. All the resources in a stack are defined by the stack's AWS CloudFormation template.
--
-- Actions
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ContinueUpdateRollback.html ContinueUpdateRollback>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStack.html CreateStack>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeleteStack.html DeleteStack>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackEvents.html DescribeStackEvents>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackResource.html DescribeStackResource>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackResources.html DescribeStackResources>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStacks.html DescribeStacks>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_EstimateTemplateCost.html EstimateTemplateCost>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetStackPolicy.html GetStackPolicy>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetTemplate.html GetTemplate>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetTemplateSummary.html GetTemplateSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListExports.html ListExports>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListImports.html ListImports>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackResources.html ListStackResources>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStacks.html ListStacks>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SetStackPolicy.html SetStackPolicy>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStack.html UpdateStack>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ValidateTemplate.html ValidateTemplate>
--
--
--
-- Data Types
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Export.html Export>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ParameterConstraints.html ParameterConstraints>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ParameterDeclaration.html ParameterDeclaration>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Stack.html Stack>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackEvent.html StackEvent>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackResource.html StackResource>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackResourceDetail.html StackResourceDetail>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackResourceSummary.html StackResourceSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSummary.html StackSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Tag.html Tag>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_TemplateParameter.html TemplateParameter>
--
--
--
-- /APIs for change sets/
--
-- If you need to make changes to the running resources in a stack, you update the stack. Before making changes to your resources, you can generate a change set, which is summary of your proposed changes. Change sets allow you to see how your changes might impact your running resources, especially for critical resources, before implementing them.
--
-- Actions
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateChangeSet.html CreateChangeSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeleteChangeSet.html DeleteChangeSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeChangeSet.html DescribeChangeSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ExecuteChangeSet.html ExecuteChangeSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListChangeSets.html ListChangeSets>
--
--
--
-- Data Types
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Change.html Change>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ChangeSetSummary.html ChangeSetSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ResourceChange.html ResourceChange>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ResourceChangeDetail.html ResourceChangeDetail>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ResourceTargetDefinition.html ResourceTargetDefinition>
--
--
--
-- /APIs for stack sets/
--
-- AWS CloudFormation StackSets lets you create a collection, or stack set, of stacks that can automatically and safely provision a common set of AWS resources across multiple AWS accounts and multiple AWS regions from a single AWS CloudFormation template. When you create a stack set, AWS CloudFormation provisions a stack in each of the specified accounts and regions by using the supplied AWS CloudFormation template and parameters. Stack sets let you manage a common set of AWS resources in a selection of accounts and regions in a single operation.
--
-- Actions
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html CreateStackInstances>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackSet.html CreateStackSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeleteStackInstances.html DeleteStackInstances>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeleteStackSet.html DeleteStackSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackInstance.html DescribeStackInstance>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackSet.html DescribeStackSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackSetOperation.html DescribeStackSetOperation>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackInstances.html ListStackInstances>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackSetOperationResults ListStackSetOperationResults>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackSetOperations ListStackSetOperations>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackSets ListStackSets>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StopStackSetOperation.html StopStackSetOperation>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
--
--
--
-- Data Types
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackInstance.html.html StackInstance>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackInstanceSummary.html.html StackInstanceSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSet.html StackSet>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSetOperation.html.html StackSetOperation>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSetOperationPreferences.html.html StackSetOperationPreferences>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSetOperationResultSummary.html.html StackSetOperationResultSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSetOperationSummary.html.html StackSetOperationSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_StackSetSummary.html StackSetSummary>
--
--     * <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Tag.html Tag>
--
--
--
module Network.AWS.CloudFormation
    (
    -- * Service Configuration
      cloudFormation

    -- * Errors
    -- $errors

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

    -- ** StackSetNotEmptyException
    , _StackSetNotEmptyException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** NameAlreadyExistsException
    , _NameAlreadyExistsException

    -- ** StaleRequestException
    , _StaleRequestException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- ** StackCreateComplete
    , stackCreateComplete

    -- ** StackUpdateComplete
    , stackUpdateComplete

    -- ** StackExists
    , stackExists

    -- ** StackDeleteComplete
    , stackDeleteComplete

    -- ** ChangeSetCreateComplete
    , changeSetCreateComplete

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

    -- ** ListChangeSets
    , module Network.AWS.CloudFormation.ListChangeSets

    -- ** ListStackResources (Paginated)
    , module Network.AWS.CloudFormation.ListStackResources

    -- ** DeleteStackInstances
    , module Network.AWS.CloudFormation.DeleteStackInstances

    -- ** CreateStackInstances
    , module Network.AWS.CloudFormation.CreateStackInstances

    -- ** GetStackPolicy
    , module Network.AWS.CloudFormation.GetStackPolicy

    -- ** DescribeStacks (Paginated)
    , module Network.AWS.CloudFormation.DescribeStacks

    -- ** CreateChangeSet
    , module Network.AWS.CloudFormation.CreateChangeSet

    -- ** ListStackSetOperations
    , module Network.AWS.CloudFormation.ListStackSetOperations

    -- ** ExecuteChangeSet
    , module Network.AWS.CloudFormation.ExecuteChangeSet

    -- ** ListStackInstances
    , module Network.AWS.CloudFormation.ListStackInstances

    -- ** ContinueUpdateRollback
    , module Network.AWS.CloudFormation.ContinueUpdateRollback

    -- ** ValidateTemplate
    , module Network.AWS.CloudFormation.ValidateTemplate

    -- ** CancelUpdateStack
    , module Network.AWS.CloudFormation.CancelUpdateStack

    -- ** DescribeStackEvents (Paginated)
    , module Network.AWS.CloudFormation.DescribeStackEvents

    -- ** SignalResource
    , module Network.AWS.CloudFormation.SignalResource

    -- ** SetStackPolicy
    , module Network.AWS.CloudFormation.SetStackPolicy

    -- ** ListImports (Paginated)
    , module Network.AWS.CloudFormation.ListImports

    -- ** ListStacks (Paginated)
    , module Network.AWS.CloudFormation.ListStacks

    -- ** DescribeAccountLimits
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

    -- ** ListStackSets
    , module Network.AWS.CloudFormation.ListStackSets

    -- ** ListExports (Paginated)
    , module Network.AWS.CloudFormation.ListExports

    -- ** CreateStackSet
    , module Network.AWS.CloudFormation.CreateStackSet

    -- ** GetTemplate
    , module Network.AWS.CloudFormation.GetTemplate

    -- ** DescribeChangeSet
    , module Network.AWS.CloudFormation.DescribeChangeSet

    -- ** DescribeStackSet
    , module Network.AWS.CloudFormation.DescribeStackSet

    -- ** ListStackSetOperationResults
    , module Network.AWS.CloudFormation.ListStackSetOperationResults

    -- ** StopStackSetOperation
    , module Network.AWS.CloudFormation.StopStackSetOperation

    -- ** DescribeStackResource
    , module Network.AWS.CloudFormation.DescribeStackResource

    -- * Types

    -- ** AccountGateStatus
    , AccountGateStatus (..)

    -- ** Capability
    , Capability (..)

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** ChangeSetStatus
    , ChangeSetStatus (..)

    -- ** ChangeSetType
    , ChangeSetType (..)

    -- ** ChangeSource
    , ChangeSource (..)

    -- ** ChangeType
    , ChangeType (..)

    -- ** EvaluationType
    , EvaluationType (..)

    -- ** ExecutionStatus
    , ExecutionStatus (..)

    -- ** OnFailure
    , OnFailure (..)

    -- ** Replacement
    , Replacement (..)

    -- ** RequiresRecreation
    , RequiresRecreation (..)

    -- ** ResourceAttribute
    , ResourceAttribute (..)

    -- ** ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- ** ResourceStatus
    , ResourceStatus (..)

    -- ** StackInstanceStatus
    , StackInstanceStatus (..)

    -- ** StackSetOperationAction
    , StackSetOperationAction (..)

    -- ** StackSetOperationResultStatus
    , StackSetOperationResultStatus (..)

    -- ** StackSetOperationStatus
    , StackSetOperationStatus (..)

    -- ** StackSetStatus
    , StackSetStatus (..)

    -- ** StackStatus
    , StackStatus (..)

    -- ** TemplateStage
    , TemplateStage (..)

    -- ** AccountGateResult
    , AccountGateResult
    , accountGateResult
    , agrStatus
    , agrStatusReason

    -- ** AccountLimit
    , AccountLimit
    , accountLimit
    , alValue
    , alName

    -- ** Change
    , Change
    , change
    , cResourceChange
    , cType

    -- ** ChangeSetSummary
    , ChangeSetSummary
    , changeSetSummary
    , cCreationTime
    , cStatus
    , cChangeSetName
    , cExecutionStatus
    , cChangeSetId
    , cStatusReason
    , cStackId
    , cDescription
    , cStackName

    -- ** Export
    , Export
    , export'
    , eValue
    , eExportingStackId
    , eName

    -- ** Output
    , Output
    , output
    , oOutputValue
    , oOutputKey
    , oExportName
    , oDescription

    -- ** Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pParameterKey
    , pUsePreviousValue

    -- ** ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcAllowedValues

    -- ** ParameterDeclaration
    , ParameterDeclaration
    , parameterDeclaration
    , pdParameterKey
    , pdParameterType
    , pdParameterConstraints
    , pdDefaultValue
    , pdNoEcho
    , pdDescription

    -- ** ResourceChange
    , ResourceChange
    , resourceChange
    , rcLogicalResourceId
    , rcPhysicalResourceId
    , rcResourceType
    , rcAction
    , rcScope
    , rcDetails
    , rcReplacement

    -- ** ResourceChangeDetail
    , ResourceChangeDetail
    , resourceChangeDetail
    , rcdCausingEntity
    , rcdChangeSource
    , rcdEvaluation
    , rcdTarget

    -- ** ResourceTargetDefinition
    , ResourceTargetDefinition
    , resourceTargetDefinition
    , rtdAttribute
    , rtdRequiresRecreation
    , rtdName

    -- ** Stack
    , Stack
    , stack
    , sDisableRollback
    , sLastUpdatedTime
    , sNotificationARNs
    , sStackStatusReason
    , sChangeSetId
    , sOutputs
    , sParameters
    , sStackId
    , sDescription
    , sCapabilities
    , sTags
    , sTimeoutInMinutes
    , sRoleARN
    , sStackName
    , sCreationTime
    , sStackStatus

    -- ** StackEvent
    , StackEvent
    , stackEvent
    , seLogicalResourceId
    , sePhysicalResourceId
    , seResourceType
    , seResourceStatusReason
    , seResourceProperties
    , seResourceStatus
    , seClientRequestToken
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp

    -- ** StackInstance
    , StackInstance
    , stackInstance
    , siStatus
    , siAccount
    , siRegion
    , siStatusReason
    , siStackId
    , siStackSetId

    -- ** StackInstanceSummary
    , StackInstanceSummary
    , stackInstanceSummary
    , sisStatus
    , sisAccount
    , sisRegion
    , sisStatusReason
    , sisStackId
    , sisStackSetId

    -- ** StackResource
    , StackResource
    , stackResource
    , srPhysicalResourceId
    , srResourceStatusReason
    , srStackId
    , srDescription
    , srStackName
    , srLogicalResourceId
    , srResourceType
    , srTimestamp
    , srResourceStatus

    -- ** StackResourceDetail
    , StackResourceDetail
    , stackResourceDetail
    , srdPhysicalResourceId
    , srdResourceStatusReason
    , srdMetadata
    , srdStackId
    , srdDescription
    , srdStackName
    , srdLogicalResourceId
    , srdResourceType
    , srdLastUpdatedTimestamp
    , srdResourceStatus

    -- ** StackResourceSummary
    , StackResourceSummary
    , stackResourceSummary
    , srsPhysicalResourceId
    , srsResourceStatusReason
    , srsLogicalResourceId
    , srsResourceType
    , srsLastUpdatedTimestamp
    , srsResourceStatus

    -- ** StackSet
    , StackSet
    , stackSet
    , ssStatus
    , ssParameters
    , ssTemplateBody
    , ssStackSetName
    , ssDescription
    , ssCapabilities
    , ssTags
    , ssStackSetId

    -- ** StackSetOperation
    , StackSetOperation
    , stackSetOperation
    , ssoStatus
    , ssoAction
    , ssoEndTimestamp
    , ssoCreationTimestamp
    , ssoOperationPreferences
    , ssoOperationId
    , ssoRetainStacks
    , ssoStackSetId

    -- ** StackSetOperationPreferences
    , StackSetOperationPreferences
    , stackSetOperationPreferences
    , ssopRegionOrder
    , ssopMaxConcurrentCount
    , ssopMaxConcurrentPercentage
    , ssopFailureToleranceCount
    , ssopFailureTolerancePercentage

    -- ** StackSetOperationResultSummary
    , StackSetOperationResultSummary
    , stackSetOperationResultSummary
    , ssorsStatus
    , ssorsAccount
    , ssorsAccountGateResult
    , ssorsRegion
    , ssorsStatusReason

    -- ** StackSetOperationSummary
    , StackSetOperationSummary
    , stackSetOperationSummary
    , ssosStatus
    , ssosAction
    , ssosEndTimestamp
    , ssosCreationTimestamp
    , ssosOperationId

    -- ** StackSetSummary
    , StackSetSummary
    , stackSetSummary
    , sssStatus
    , sssStackSetName
    , sssDescription
    , sssStackSetId

    -- ** StackSummary
    , StackSummary
    , stackSummary
    , ssLastUpdatedTime
    , ssStackStatusReason
    , ssTemplateDescription
    , ssDeletionTime
    , ssStackId
    , ssStackName
    , ssCreationTime
    , ssStackStatus

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpParameterKey
    , tpDefaultValue
    , tpNoEcho
    , tpDescription
    ) where

import           Network.AWS.CloudFormation.CancelUpdateStack
import           Network.AWS.CloudFormation.ContinueUpdateRollback
import           Network.AWS.CloudFormation.CreateChangeSet
import           Network.AWS.CloudFormation.CreateStack
import           Network.AWS.CloudFormation.CreateStackInstances
import           Network.AWS.CloudFormation.CreateStackSet
import           Network.AWS.CloudFormation.DeleteChangeSet
import           Network.AWS.CloudFormation.DeleteStack
import           Network.AWS.CloudFormation.DeleteStackInstances
import           Network.AWS.CloudFormation.DeleteStackSet
import           Network.AWS.CloudFormation.DescribeAccountLimits
import           Network.AWS.CloudFormation.DescribeChangeSet
import           Network.AWS.CloudFormation.DescribeStackEvents
import           Network.AWS.CloudFormation.DescribeStackInstance
import           Network.AWS.CloudFormation.DescribeStackResource
import           Network.AWS.CloudFormation.DescribeStackResources
import           Network.AWS.CloudFormation.DescribeStacks
import           Network.AWS.CloudFormation.DescribeStackSet
import           Network.AWS.CloudFormation.DescribeStackSetOperation
import           Network.AWS.CloudFormation.EstimateTemplateCost
import           Network.AWS.CloudFormation.ExecuteChangeSet
import           Network.AWS.CloudFormation.GetStackPolicy
import           Network.AWS.CloudFormation.GetTemplate
import           Network.AWS.CloudFormation.GetTemplateSummary
import           Network.AWS.CloudFormation.ListChangeSets
import           Network.AWS.CloudFormation.ListExports
import           Network.AWS.CloudFormation.ListImports
import           Network.AWS.CloudFormation.ListStackInstances
import           Network.AWS.CloudFormation.ListStackResources
import           Network.AWS.CloudFormation.ListStacks
import           Network.AWS.CloudFormation.ListStackSetOperationResults
import           Network.AWS.CloudFormation.ListStackSetOperations
import           Network.AWS.CloudFormation.ListStackSets
import           Network.AWS.CloudFormation.SetStackPolicy
import           Network.AWS.CloudFormation.SignalResource
import           Network.AWS.CloudFormation.StopStackSetOperation
import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.UpdateStack
import           Network.AWS.CloudFormation.UpdateStackSet
import           Network.AWS.CloudFormation.ValidateTemplate
import           Network.AWS.CloudFormation.Waiters

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
