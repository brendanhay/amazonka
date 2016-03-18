{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT
--
-- AWS IoT provides secure, bi-directional communication between
-- Internet-connected things (such as sensors, actuators, embedded devices,
-- or smart appliances) and the AWS cloud. You can discover your custom
-- IoT-Data endpoint to communicate with, configure rules for data
-- processing and integration with other services, organize resources
-- associated with each thing (Thing Registry), configure logging, and
-- create and manage policies and credentials to authenticate things.
--
-- For more information about how AWS IoT works, see the
-- <http://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html Developer Guide>.
--
-- /See:/ <https://aws.amazon.com/iot AWS API Reference>
module Network.AWS.IoT
    (
    -- * Service Configuration
      ioT

    -- * Errors
    -- $errors

    -- ** SqlParseException
    , _SqlParseException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TransferConflictException
    , _TransferConflictException

    -- ** CertificateStateException
    , _CertificateStateException

    -- ** MalformedPolicyException
    , _MalformedPolicyException

    -- ** DeleteConflictException
    , _DeleteConflictException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** TransferAlreadyCompletedException
    , _TransferAlreadyCompletedException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** VersionsLimitExceededException
    , _VersionsLimitExceededException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** InternalException
    , _InternalException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPolicies
    , module Network.AWS.IoT.ListPolicies

    -- ** CreatePolicy
    , module Network.AWS.IoT.CreatePolicy

    -- ** ListThingPrincipals
    , module Network.AWS.IoT.ListThingPrincipals

    -- ** ListPrincipalThings
    , module Network.AWS.IoT.ListPrincipalThings

    -- ** GetLoggingOptions
    , module Network.AWS.IoT.GetLoggingOptions

    -- ** CreateCertificateFromCsr
    , module Network.AWS.IoT.CreateCertificateFromCsr

    -- ** DeleteThing
    , module Network.AWS.IoT.DeleteThing

    -- ** UpdateThing
    , module Network.AWS.IoT.UpdateThing

    -- ** CancelCertificateTransfer
    , module Network.AWS.IoT.CancelCertificateTransfer

    -- ** DeletePolicyVersion
    , module Network.AWS.IoT.DeletePolicyVersion

    -- ** DisableTopicRule
    , module Network.AWS.IoT.DisableTopicRule

    -- ** CreateTopicRule
    , module Network.AWS.IoT.CreateTopicRule

    -- ** CreatePolicyVersion
    , module Network.AWS.IoT.CreatePolicyVersion

    -- ** DeleteTopicRule
    , module Network.AWS.IoT.DeleteTopicRule

    -- ** ListPrincipalPolicies
    , module Network.AWS.IoT.ListPrincipalPolicies

    -- ** ListTopicRules
    , module Network.AWS.IoT.ListTopicRules

    -- ** TransferCertificate
    , module Network.AWS.IoT.TransferCertificate

    -- ** GetTopicRule
    , module Network.AWS.IoT.GetTopicRule

    -- ** DescribeThing
    , module Network.AWS.IoT.DescribeThing

    -- ** DeletePolicy
    , module Network.AWS.IoT.DeletePolicy

    -- ** ListCertificates
    , module Network.AWS.IoT.ListCertificates

    -- ** GetPolicyVersion
    , module Network.AWS.IoT.GetPolicyVersion

    -- ** DeleteCertificate
    , module Network.AWS.IoT.DeleteCertificate

    -- ** UpdateCertificate
    , module Network.AWS.IoT.UpdateCertificate

    -- ** AttachThingPrincipal
    , module Network.AWS.IoT.AttachThingPrincipal

    -- ** ListThings
    , module Network.AWS.IoT.ListThings

    -- ** DetachPrincipalPolicy
    , module Network.AWS.IoT.DetachPrincipalPolicy

    -- ** CreateThing
    , module Network.AWS.IoT.CreateThing

    -- ** DescribeCertificate
    , module Network.AWS.IoT.DescribeCertificate

    -- ** ReplaceTopicRule
    , module Network.AWS.IoT.ReplaceTopicRule

    -- ** SetDefaultPolicyVersion
    , module Network.AWS.IoT.SetDefaultPolicyVersion

    -- ** ListPolicyVersions
    , module Network.AWS.IoT.ListPolicyVersions

    -- ** CreateKeysAndCertificate
    , module Network.AWS.IoT.CreateKeysAndCertificate

    -- ** EnableTopicRule
    , module Network.AWS.IoT.EnableTopicRule

    -- ** AcceptCertificateTransfer
    , module Network.AWS.IoT.AcceptCertificateTransfer

    -- ** GetPolicy
    , module Network.AWS.IoT.GetPolicy

    -- ** DescribeEndpoint
    , module Network.AWS.IoT.DescribeEndpoint

    -- ** SetLoggingOptions
    , module Network.AWS.IoT.SetLoggingOptions

    -- ** AttachPrincipalPolicy
    , module Network.AWS.IoT.AttachPrincipalPolicy

    -- ** RejectCertificateTransfer
    , module Network.AWS.IoT.RejectCertificateTransfer

    -- ** DetachThingPrincipal
    , module Network.AWS.IoT.DetachThingPrincipal

    -- * Types

    -- ** CertificateStatus
    , CertificateStatus (..)

    -- ** LogLevel
    , LogLevel (..)

    -- ** MessageFormat
    , MessageFormat (..)

    -- ** Action
    , Action
    , action
    , aCloudwatchMetric
    , aCloudwatchAlarm
    , aSns
    , aDynamoDB
    , aFirehose
    , aLambda
    , aKinesis
    , aS3
    , aElasticsearch
    , aRepublish
    , aSqs

    -- ** AttributePayload
    , AttributePayload
    , attributePayload
    , apAttributes

    -- ** Certificate
    , Certificate
    , certificate
    , cStatus
    , cCertificateARN
    , cCertificateId
    , cCreationDate

    -- ** CertificateDescription
    , CertificateDescription
    , certificateDescription
    , cdStatus
    , cdOwnedBy
    , cdLastModifiedDate
    , cdCertificatePem
    , cdCertificateARN
    , cdCertificateId
    , cdCreationDate

    -- ** CloudwatchAlarmAction
    , CloudwatchAlarmAction
    , cloudwatchAlarmAction
    , caaRoleARN
    , caaAlarmName
    , caaStateReason
    , caaStateValue

    -- ** CloudwatchMetricAction
    , CloudwatchMetricAction
    , cloudwatchMetricAction
    , cmaMetricTimestamp
    , cmaRoleARN
    , cmaMetricNamespace
    , cmaMetricName
    , cmaMetricValue
    , cmaMetricUnit

    -- ** DynamoDBAction
    , DynamoDBAction
    , dynamoDBAction
    , ddaPayloadField
    , ddaTableName
    , ddaRoleARN
    , ddaHashKeyField
    , ddaHashKeyValue
    , ddaRangeKeyField
    , ddaRangeKeyValue

    -- ** ElasticsearchAction
    , ElasticsearchAction
    , elasticsearchAction
    , eaRoleARN
    , eaEndpoint
    , eaIndex
    , eaType
    , eaId

    -- ** FirehoseAction
    , FirehoseAction
    , firehoseAction
    , faRoleARN
    , faDeliveryStreamName

    -- ** KeyPair
    , KeyPair
    , keyPair
    , kpPrivateKey
    , kpPublicKey

    -- ** KinesisAction
    , KinesisAction
    , kinesisAction
    , kaPartitionKey
    , kaRoleARN
    , kaStreamName

    -- ** LambdaAction
    , LambdaAction
    , lambdaAction
    , laFunctionARN

    -- ** LoggingOptionsPayload
    , LoggingOptionsPayload
    , loggingOptionsPayload
    , lopLogLevel
    , lopRoleARN

    -- ** Policy
    , Policy
    , policy
    , pPolicyName
    , pPolicyARN

    -- ** PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvIsDefaultVersion

    -- ** RepublishAction
    , RepublishAction
    , republishAction
    , raRoleARN
    , raTopic

    -- ** S3Action
    , S3Action
    , s3Action
    , sRoleARN
    , sBucketName
    , sKey

    -- ** SNSAction
    , SNSAction
    , snsAction
    , snsaMessageFormat
    , snsaTargetARN
    , snsaRoleARN

    -- ** SqsAction
    , SqsAction
    , sqsAction
    , saUseBase64
    , saRoleARN
    , saQueueURL

    -- ** ThingAttribute
    , ThingAttribute
    , thingAttribute
    , taAttributes
    , taThingName

    -- ** TopicRule
    , TopicRule
    , topicRule
    , trCreatedAt
    , trActions
    , trRuleDisabled
    , trRuleName
    , trSql
    , trDescription

    -- ** TopicRuleListItem
    , TopicRuleListItem
    , topicRuleListItem
    , trliCreatedAt
    , trliRuleDisabled
    , trliRuleName
    , trliRuleARN
    , trliTopicPattern

    -- ** TopicRulePayload
    , TopicRulePayload
    , topicRulePayload
    , trpRuleDisabled
    , trpDescription
    , trpSql
    , trpActions
    ) where

import           Network.AWS.IoT.AcceptCertificateTransfer
import           Network.AWS.IoT.AttachPrincipalPolicy
import           Network.AWS.IoT.AttachThingPrincipal
import           Network.AWS.IoT.CancelCertificateTransfer
import           Network.AWS.IoT.CreateCertificateFromCsr
import           Network.AWS.IoT.CreateKeysAndCertificate
import           Network.AWS.IoT.CreatePolicy
import           Network.AWS.IoT.CreatePolicyVersion
import           Network.AWS.IoT.CreateThing
import           Network.AWS.IoT.CreateTopicRule
import           Network.AWS.IoT.DeleteCertificate
import           Network.AWS.IoT.DeletePolicy
import           Network.AWS.IoT.DeletePolicyVersion
import           Network.AWS.IoT.DeleteThing
import           Network.AWS.IoT.DeleteTopicRule
import           Network.AWS.IoT.DescribeCertificate
import           Network.AWS.IoT.DescribeEndpoint
import           Network.AWS.IoT.DescribeThing
import           Network.AWS.IoT.DetachPrincipalPolicy
import           Network.AWS.IoT.DetachThingPrincipal
import           Network.AWS.IoT.DisableTopicRule
import           Network.AWS.IoT.EnableTopicRule
import           Network.AWS.IoT.GetLoggingOptions
import           Network.AWS.IoT.GetPolicy
import           Network.AWS.IoT.GetPolicyVersion
import           Network.AWS.IoT.GetTopicRule
import           Network.AWS.IoT.ListCertificates
import           Network.AWS.IoT.ListPolicies
import           Network.AWS.IoT.ListPolicyVersions
import           Network.AWS.IoT.ListPrincipalPolicies
import           Network.AWS.IoT.ListPrincipalThings
import           Network.AWS.IoT.ListThingPrincipals
import           Network.AWS.IoT.ListThings
import           Network.AWS.IoT.ListTopicRules
import           Network.AWS.IoT.RejectCertificateTransfer
import           Network.AWS.IoT.ReplaceTopicRule
import           Network.AWS.IoT.SetDefaultPolicyVersion
import           Network.AWS.IoT.SetLoggingOptions
import           Network.AWS.IoT.TransferCertificate
import           Network.AWS.IoT.Types
import           Network.AWS.IoT.UpdateCertificate
import           Network.AWS.IoT.UpdateThing
import           Network.AWS.IoT.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'IoT'.
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
