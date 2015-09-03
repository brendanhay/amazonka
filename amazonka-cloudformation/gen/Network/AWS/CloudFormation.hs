{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CloudFormation
--
-- AWS CloudFormation enables you to create and manage AWS infrastructure
-- deployments predictably and repeatedly. AWS CloudFormation helps you
-- leverage AWS products such as Amazon EC2, EBS, Amazon SNS, ELB, and Auto
-- Scaling to build highly-reliable, highly scalable, cost effective
-- applications without worrying about creating and configuring the
-- underlying AWS infrastructure.
--
-- With AWS CloudFormation, you declare all of your resources and
-- dependencies in a template file. The template defines a collection of
-- resources as a single unit called a stack. AWS CloudFormation creates
-- and deletes all member resources of the stack together and manages all
-- dependencies between the resources for you.
--
-- For more information about this product, go to the
-- <http://aws.amazon.com/cloudformation/ CloudFormation Product Page>.
--
-- Amazon CloudFormation makes use of other AWS products. If you need
-- additional technical information about a specific AWS product, you can
-- find the product\'s technical documentation at
-- <http://aws.amazon.com/documentation/>.
--
-- /See:/ <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.CloudFormation
    (
    -- * Service Configuration
      cloudFormation

    -- * Errors
    -- $errors

    -- ** InsufficientCapabilitiesException
    , _InsufficientCapabilitiesException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteStack
    , module Network.AWS.CloudFormation.DeleteStack

    -- ** UpdateStack
    , module Network.AWS.CloudFormation.UpdateStack

    -- ** GetTemplateSummary
    , module Network.AWS.CloudFormation.GetTemplateSummary

    -- ** ListStackResources (Paginated)
    , module Network.AWS.CloudFormation.ListStackResources

    -- ** GetStackPolicy
    , module Network.AWS.CloudFormation.GetStackPolicy

    -- ** DescribeStacks (Paginated)
    , module Network.AWS.CloudFormation.DescribeStacks

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

    -- ** ListStacks (Paginated)
    , module Network.AWS.CloudFormation.ListStacks

    -- ** DescribeStackResources
    , module Network.AWS.CloudFormation.DescribeStackResources

    -- ** CreateStack
    , module Network.AWS.CloudFormation.CreateStack

    -- ** EstimateTemplateCost
    , module Network.AWS.CloudFormation.EstimateTemplateCost

    -- ** GetTemplate
    , module Network.AWS.CloudFormation.GetTemplate

    -- ** DescribeStackResource
    , module Network.AWS.CloudFormation.DescribeStackResource

    -- * Types

    -- ** Capability
    , Capability (..)

    -- ** OnFailure
    , OnFailure (..)

    -- ** ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- ** ResourceStatus
    , ResourceStatus (..)

    -- ** StackStatus
    , StackStatus (..)

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

    -- ** Stack
    , Stack
    , stack
    , sDisableRollback
    , sLastUpdatedTime
    , sNotificationARNs
    , sStackStatusReason
    , sOutputs
    , sParameters
    , sStackId
    , sDescription
    , sCapabilities
    , sTags
    , sTimeoutInMinutes
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
import           Network.AWS.CloudFormation.CreateStack
import           Network.AWS.CloudFormation.DeleteStack
import           Network.AWS.CloudFormation.DescribeStackEvents
import           Network.AWS.CloudFormation.DescribeStackResource
import           Network.AWS.CloudFormation.DescribeStackResources
import           Network.AWS.CloudFormation.DescribeStacks
import           Network.AWS.CloudFormation.EstimateTemplateCost
import           Network.AWS.CloudFormation.GetStackPolicy
import           Network.AWS.CloudFormation.GetTemplate
import           Network.AWS.CloudFormation.GetTemplateSummary
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
