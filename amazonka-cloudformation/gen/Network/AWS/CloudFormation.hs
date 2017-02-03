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
-- Amazon CloudFormation makes use of other AWS products. For additional technical information about a specific AWS product, see its <http://docs.aws.amazon.com/ technical documentation> .
--
module Network.AWS.CloudFormation
    (
    -- * Service Configuration
      cloudFormation

    -- * Errors
    -- $errors

    -- ** ChangeSetNotFoundException
    , _ChangeSetNotFoundException

    -- ** InvalidChangeSetStatusException
    , _InvalidChangeSetStatusException

    -- ** InsufficientCapabilitiesException
    , _InsufficientCapabilitiesException

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

    -- ** GetStackPolicy
    , module Network.AWS.CloudFormation.GetStackPolicy

    -- ** DescribeStacks (Paginated)
    , module Network.AWS.CloudFormation.DescribeStacks

    -- ** CreateChangeSet
    , module Network.AWS.CloudFormation.CreateChangeSet

    -- ** ExecuteChangeSet
    , module Network.AWS.CloudFormation.ExecuteChangeSet

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

    -- ** ListImports
    , module Network.AWS.CloudFormation.ListImports

    -- ** ListStacks (Paginated)
    , module Network.AWS.CloudFormation.ListStacks

    -- ** DescribeAccountLimits
    , module Network.AWS.CloudFormation.DescribeAccountLimits

    -- ** DescribeStackResources
    , module Network.AWS.CloudFormation.DescribeStackResources

    -- ** CreateStack
    , module Network.AWS.CloudFormation.CreateStack

    -- ** EstimateTemplateCost
    , module Network.AWS.CloudFormation.EstimateTemplateCost

    -- ** DeleteChangeSet
    , module Network.AWS.CloudFormation.DeleteChangeSet

    -- ** ListExports
    , module Network.AWS.CloudFormation.ListExports

    -- ** GetTemplate
    , module Network.AWS.CloudFormation.GetTemplate

    -- ** DescribeChangeSet
    , module Network.AWS.CloudFormation.DescribeChangeSet

    -- ** DescribeStackResource
    , module Network.AWS.CloudFormation.DescribeStackResource

    -- * Types

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

    -- ** StackStatus
    , StackStatus (..)

    -- ** TemplateStage
    , TemplateStage (..)

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
    , cssCreationTime
    , cssStatus
    , cssChangeSetName
    , cssExecutionStatus
    , cssChangeSetId
    , cssStatusReason
    , cssStackId
    , cssDescription
    , cssStackName

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
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp

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
    , tagValue
    , tagKey

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
import           Network.AWS.CloudFormation.DeleteChangeSet
import           Network.AWS.CloudFormation.DeleteStack
import           Network.AWS.CloudFormation.DescribeAccountLimits
import           Network.AWS.CloudFormation.DescribeChangeSet
import           Network.AWS.CloudFormation.DescribeStackEvents
import           Network.AWS.CloudFormation.DescribeStackResource
import           Network.AWS.CloudFormation.DescribeStackResources
import           Network.AWS.CloudFormation.DescribeStacks
import           Network.AWS.CloudFormation.EstimateTemplateCost
import           Network.AWS.CloudFormation.ExecuteChangeSet
import           Network.AWS.CloudFormation.GetStackPolicy
import           Network.AWS.CloudFormation.GetTemplate
import           Network.AWS.CloudFormation.GetTemplateSummary
import           Network.AWS.CloudFormation.ListChangeSets
import           Network.AWS.CloudFormation.ListExports
import           Network.AWS.CloudFormation.ListImports
import           Network.AWS.CloudFormation.ListStackResources
import           Network.AWS.CloudFormation.ListStacks
import           Network.AWS.CloudFormation.SetStackPolicy
import           Network.AWS.CloudFormation.SignalResource
import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.UpdateStack
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
