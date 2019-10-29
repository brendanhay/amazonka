{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EventBridge
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EventBridge helps you to respond to state changes in your AWS resources. When your resources change state, they automatically send events into an event stream. You can create rules that match selected events in the stream and route them to targets to take action. You can also use rules to take action on a predetermined schedule. For example, you can configure rules to:
--
--
--     * Automatically invoke an AWS Lambda function to update DNS entries when an event notifies you that Amazon EC2 instance enters the running state
--
--     * Direct specific API records from AWS CloudTrail to an Amazon Kinesis data stream for detailed analysis of potential security or availability risks
--
--     * Periodically invoke a built-in target to create a snapshot of an Amazon EBS volume
--
--
--
-- For more information about the features of Amazon EventBridge, see the <https://docs.aws.amazon.com/eventbridge/latest/userguide/ Amazon EventBridge User Guide> .
--
module Network.AWS.EventBridge
    (
    -- * Service Configuration
      eventBridge

    -- * Errors
    -- $errors

    -- ** ManagedRuleException
    , _ManagedRuleException

    -- ** PolicyLengthExceededException
    , _PolicyLengthExceededException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InvalidEventPatternException
    , _InvalidEventPatternException

    -- ** InternalException
    , _InternalException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidStateException
    , _InvalidStateException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RemoveTargets
    , module Network.AWS.EventBridge.RemoveTargets

    -- ** DeleteRule
    , module Network.AWS.EventBridge.DeleteRule

    -- ** ListPartnerEventSourceAccounts
    , module Network.AWS.EventBridge.ListPartnerEventSourceAccounts

    -- ** ListRules (Paginated)
    , module Network.AWS.EventBridge.ListRules

    -- ** PutRule
    , module Network.AWS.EventBridge.PutRule

    -- ** DisableRule
    , module Network.AWS.EventBridge.DisableRule

    -- ** PutPermission
    , module Network.AWS.EventBridge.PutPermission

    -- ** ListTagsForResource
    , module Network.AWS.EventBridge.ListTagsForResource

    -- ** ListTargetsByRule (Paginated)
    , module Network.AWS.EventBridge.ListTargetsByRule

    -- ** RemovePermission
    , module Network.AWS.EventBridge.RemovePermission

    -- ** ActivateEventSource
    , module Network.AWS.EventBridge.ActivateEventSource

    -- ** PutPartnerEvents
    , module Network.AWS.EventBridge.PutPartnerEvents

    -- ** DescribeRule
    , module Network.AWS.EventBridge.DescribeRule

    -- ** DeletePartnerEventSource
    , module Network.AWS.EventBridge.DeletePartnerEventSource

    -- ** ListEventBuses
    , module Network.AWS.EventBridge.ListEventBuses

    -- ** CreateEventBus
    , module Network.AWS.EventBridge.CreateEventBus

    -- ** DescribeEventSource
    , module Network.AWS.EventBridge.DescribeEventSource

    -- ** EnableRule
    , module Network.AWS.EventBridge.EnableRule

    -- ** ListRuleNamesByTarget (Paginated)
    , module Network.AWS.EventBridge.ListRuleNamesByTarget

    -- ** TestEventPattern
    , module Network.AWS.EventBridge.TestEventPattern

    -- ** DescribePartnerEventSource
    , module Network.AWS.EventBridge.DescribePartnerEventSource

    -- ** DescribeEventBus
    , module Network.AWS.EventBridge.DescribeEventBus

    -- ** ListEventSources
    , module Network.AWS.EventBridge.ListEventSources

    -- ** TagResource
    , module Network.AWS.EventBridge.TagResource

    -- ** CreatePartnerEventSource
    , module Network.AWS.EventBridge.CreatePartnerEventSource

    -- ** PutTargets
    , module Network.AWS.EventBridge.PutTargets

    -- ** UntagResource
    , module Network.AWS.EventBridge.UntagResource

    -- ** PutEvents
    , module Network.AWS.EventBridge.PutEvents

    -- ** ListPartnerEventSources
    , module Network.AWS.EventBridge.ListPartnerEventSources

    -- ** DeactivateEventSource
    , module Network.AWS.EventBridge.DeactivateEventSource

    -- ** DeleteEventBus
    , module Network.AWS.EventBridge.DeleteEventBus

    -- * Types

    -- ** AssignPublicIP
    , AssignPublicIP (..)

    -- ** EventSourceState
    , EventSourceState (..)

    -- ** LaunchType
    , LaunchType (..)

    -- ** RuleState
    , RuleState (..)

    -- ** AWSVPCConfiguration
    , AWSVPCConfiguration
    , awsVPCConfiguration
    , avcSecurityGroups
    , avcAssignPublicIP
    , avcSubnets

    -- ** BatchArrayProperties
    , BatchArrayProperties
    , batchArrayProperties
    , bapSize

    -- ** BatchParameters
    , BatchParameters
    , batchParameters
    , bpRetryStrategy
    , bpArrayProperties
    , bpJobDefinition
    , bpJobName

    -- ** BatchRetryStrategy
    , BatchRetryStrategy
    , batchRetryStrategy
    , brsAttempts

    -- ** Condition
    , Condition
    , condition
    , cType
    , cKey
    , cValue

    -- ** EcsParameters
    , EcsParameters
    , ecsParameters
    , epGroup
    , epPlatformVersion
    , epLaunchType
    , epTaskCount
    , epNetworkConfiguration
    , epTaskDefinitionARN

    -- ** EventBus
    , EventBus
    , eventBus
    , ebARN
    , ebName
    , ebPolicy

    -- ** EventSource
    , EventSource
    , eventSource
    , esCreationTime
    , esState
    , esARN
    , esCreatedBy
    , esName
    , esExpirationTime

    -- ** InputTransformer
    , InputTransformer
    , inputTransformer
    , itInputPathsMap
    , itInputTemplate

    -- ** KinesisParameters
    , KinesisParameters
    , kinesisParameters
    , kpPartitionKeyPath

    -- ** NetworkConfiguration
    , NetworkConfiguration
    , networkConfiguration
    , ncAwsvpcConfiguration

    -- ** PartnerEventSource
    , PartnerEventSource
    , partnerEventSource
    , pesARN
    , pesName

    -- ** PartnerEventSourceAccount
    , PartnerEventSourceAccount
    , partnerEventSourceAccount
    , pesaCreationTime
    , pesaState
    , pesaAccount
    , pesaExpirationTime

    -- ** PutEventsRequestEntry
    , PutEventsRequestEntry
    , putEventsRequestEntry
    , pereTime
    , pereDetailType
    , pereResources
    , pereEventBusName
    , pereSource
    , pereDetail

    -- ** PutEventsResultEntry
    , PutEventsResultEntry
    , putEventsResultEntry
    , pereErrorCode
    , pereErrorMessage
    , pereEventId

    -- ** PutPartnerEventsRequestEntry
    , PutPartnerEventsRequestEntry
    , putPartnerEventsRequestEntry
    , ppereTime
    , ppereDetailType
    , ppereResources
    , ppereSource
    , ppereDetail

    -- ** PutPartnerEventsResultEntry
    , PutPartnerEventsResultEntry
    , putPartnerEventsResultEntry
    , ppereErrorCode
    , ppereErrorMessage
    , ppereEventId

    -- ** PutTargetsResultEntry
    , PutTargetsResultEntry
    , putTargetsResultEntry
    , ptreTargetId
    , ptreErrorCode
    , ptreErrorMessage

    -- ** RemoveTargetsResultEntry
    , RemoveTargetsResultEntry
    , removeTargetsResultEntry
    , rtreTargetId
    , rtreErrorCode
    , rtreErrorMessage

    -- ** Rule
    , Rule
    , rule
    , rEventPattern
    , rState
    , rARN
    , rEventBusName
    , rScheduleExpression
    , rName
    , rDescription
    , rManagedBy
    , rRoleARN

    -- ** RunCommandParameters
    , RunCommandParameters
    , runCommandParameters
    , rcpRunCommandTargets

    -- ** RunCommandTarget
    , RunCommandTarget
    , runCommandTarget
    , rctKey
    , rctValues

    -- ** SqsParameters
    , SqsParameters
    , sqsParameters
    , spMessageGroupId

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** Target
    , Target
    , target
    , tRunCommandParameters
    , tKinesisParameters
    , tInputTransformer
    , tSqsParameters
    , tInput
    , tBatchParameters
    , tEcsParameters
    , tInputPath
    , tRoleARN
    , tId
    , tARN
    ) where

import Network.AWS.EventBridge.ActivateEventSource
import Network.AWS.EventBridge.CreateEventBus
import Network.AWS.EventBridge.CreatePartnerEventSource
import Network.AWS.EventBridge.DeactivateEventSource
import Network.AWS.EventBridge.DeleteEventBus
import Network.AWS.EventBridge.DeletePartnerEventSource
import Network.AWS.EventBridge.DeleteRule
import Network.AWS.EventBridge.DescribeEventBus
import Network.AWS.EventBridge.DescribeEventSource
import Network.AWS.EventBridge.DescribePartnerEventSource
import Network.AWS.EventBridge.DescribeRule
import Network.AWS.EventBridge.DisableRule
import Network.AWS.EventBridge.EnableRule
import Network.AWS.EventBridge.ListEventBuses
import Network.AWS.EventBridge.ListEventSources
import Network.AWS.EventBridge.ListPartnerEventSourceAccounts
import Network.AWS.EventBridge.ListPartnerEventSources
import Network.AWS.EventBridge.ListRuleNamesByTarget
import Network.AWS.EventBridge.ListRules
import Network.AWS.EventBridge.ListTagsForResource
import Network.AWS.EventBridge.ListTargetsByRule
import Network.AWS.EventBridge.PutEvents
import Network.AWS.EventBridge.PutPartnerEvents
import Network.AWS.EventBridge.PutPermission
import Network.AWS.EventBridge.PutRule
import Network.AWS.EventBridge.PutTargets
import Network.AWS.EventBridge.RemovePermission
import Network.AWS.EventBridge.RemoveTargets
import Network.AWS.EventBridge.TagResource
import Network.AWS.EventBridge.TestEventPattern
import Network.AWS.EventBridge.Types
import Network.AWS.EventBridge.UntagResource
import Network.AWS.EventBridge.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'EventBridge'.
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
