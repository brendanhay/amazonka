{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types
    (
    -- * Service Configuration
      ioT

    -- * Errors
    , _CertificateConflictException
    , _SqlParseException
    , _InvalidRequestException
    , _TransferConflictException
    , _CertificateStateException
    , _RegistrationCodeValidationException
    , _MalformedPolicyException
    , _DeleteConflictException
    , _ResourceAlreadyExistsException
    , _CertificateValidationException
    , _TransferAlreadyCompletedException
    , _ThrottlingException
    , _InternalFailureException
    , _VersionsLimitExceededException
    , _ServiceUnavailableException
    , _InternalException
    , _VersionConflictException
    , _UnauthorizedException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * AutoRegistrationStatus
    , AutoRegistrationStatus (..)

    -- * CACertificateStatus
    , CACertificateStatus (..)

    -- * CannedAccessControlList
    , CannedAccessControlList (..)

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * DynamoKeyType
    , DynamoKeyType (..)

    -- * LogLevel
    , LogLevel (..)

    -- * MessageFormat
    , MessageFormat (..)

    -- * Action
    , Action
    , action
    , aCloudwatchMetric
    , aDynamoDBv2
    , aCloudwatchAlarm
    , aSns
    , aDynamoDB
    , aFirehose
    , aLambda
    , aSalesforce
    , aKinesis
    , aS3
    , aElasticsearch
    , aRepublish
    , aSqs

    -- * AttributePayload
    , AttributePayload
    , attributePayload
    , apAttributes
    , apMerge

    -- * CACertificate
    , CACertificate
    , cACertificate
    , cacStatus
    , cacCertificateARN
    , cacCertificateId
    , cacCreationDate

    -- * CACertificateDescription
    , CACertificateDescription
    , cACertificateDescription
    , cacdStatus
    , cacdOwnedBy
    , cacdCertificatePem
    , cacdCertificateARN
    , cacdCertificateId
    , cacdAutoRegistrationStatus
    , cacdCreationDate

    -- * Certificate
    , Certificate
    , certificate
    , cStatus
    , cCertificateARN
    , cCertificateId
    , cCreationDate

    -- * CertificateDescription
    , CertificateDescription
    , certificateDescription
    , cdStatus
    , cdOwnedBy
    , cdLastModifiedDate
    , cdCaCertificateId
    , cdPreviousOwnedBy
    , cdCertificatePem
    , cdCertificateARN
    , cdCertificateId
    , cdCreationDate
    , cdTransferData

    -- * CloudwatchAlarmAction
    , CloudwatchAlarmAction
    , cloudwatchAlarmAction
    , caaRoleARN
    , caaAlarmName
    , caaStateReason
    , caaStateValue

    -- * CloudwatchMetricAction
    , CloudwatchMetricAction
    , cloudwatchMetricAction
    , cmaMetricTimestamp
    , cmaRoleARN
    , cmaMetricNamespace
    , cmaMetricName
    , cmaMetricValue
    , cmaMetricUnit

    -- * DynamoDBAction
    , DynamoDBAction
    , dynamoDBAction
    , ddbaHashKeyType
    , ddbaOperation
    , ddbaRangeKeyType
    , ddbaPayloadField
    , ddbaRangeKeyField
    , ddbaRangeKeyValue
    , ddbaTableName
    , ddbaRoleARN
    , ddbaHashKeyField
    , ddbaHashKeyValue

    -- * DynamoDBv2Action
    , DynamoDBv2Action
    , dynamoDBv2Action
    , ddaPutItem
    , ddaRoleARN

    -- * ElasticsearchAction
    , ElasticsearchAction
    , elasticsearchAction
    , eaRoleARN
    , eaEndpoint
    , eaIndex
    , eaType
    , eaId

    -- * FirehoseAction
    , FirehoseAction
    , firehoseAction
    , faSeparator
    , faRoleARN
    , faDeliveryStreamName

    -- * KeyPair
    , KeyPair
    , keyPair
    , kpPrivateKey
    , kpPublicKey

    -- * KinesisAction
    , KinesisAction
    , kinesisAction
    , kaPartitionKey
    , kaRoleARN
    , kaStreamName

    -- * LambdaAction
    , LambdaAction
    , lambdaAction
    , laFunctionARN

    -- * LoggingOptionsPayload
    , LoggingOptionsPayload
    , loggingOptionsPayload
    , lopLogLevel
    , lopRoleARN

    -- * OutgoingCertificate
    , OutgoingCertificate
    , outgoingCertificate
    , ocTransferDate
    , ocCertificateARN
    , ocCertificateId
    , ocTransferredTo
    , ocCreationDate
    , ocTransferMessage

    -- * Policy
    , Policy
    , policy
    , pPolicyName
    , pPolicyARN

    -- * PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvIsDefaultVersion

    -- * PutItemInput
    , PutItemInput
    , putItemInput
    , piiTableName

    -- * RepublishAction
    , RepublishAction
    , republishAction
    , raRoleARN
    , raTopic

    -- * S3Action
    , S3Action
    , s3Action
    , sCannedACL
    , sRoleARN
    , sBucketName
    , sKey

    -- * SNSAction
    , SNSAction
    , snsAction
    , snsaMessageFormat
    , snsaTargetARN
    , snsaRoleARN

    -- * SalesforceAction
    , SalesforceAction
    , salesforceAction
    , saToken
    , saUrl

    -- * SqsAction
    , SqsAction
    , sqsAction
    , saUseBase64
    , saRoleARN
    , saQueueURL

    -- * ThingAttribute
    , ThingAttribute
    , thingAttribute
    , taThingTypeName
    , taAttributes
    , taVersion
    , taThingName

    -- * ThingTypeDefinition
    , ThingTypeDefinition
    , thingTypeDefinition
    , ttdThingTypeProperties
    , ttdThingTypeName
    , ttdThingTypeMetadata

    -- * ThingTypeMetadata
    , ThingTypeMetadata
    , thingTypeMetadata
    , ttmDeprecationDate
    , ttmCreationDate
    , ttmDeprecated

    -- * ThingTypeProperties
    , ThingTypeProperties
    , thingTypeProperties
    , ttpSearchableAttributes
    , ttpThingTypeDescription

    -- * TopicRule
    , TopicRule
    , topicRule
    , trCreatedAt
    , trActions
    , trAwsIotSqlVersion
    , trRuleDisabled
    , trRuleName
    , trSql
    , trDescription

    -- * TopicRuleListItem
    , TopicRuleListItem
    , topicRuleListItem
    , trliCreatedAt
    , trliRuleDisabled
    , trliRuleName
    , trliRuleARN
    , trliTopicPattern

    -- * TopicRulePayload
    , TopicRulePayload
    , topicRulePayload
    , trpAwsIotSqlVersion
    , trpRuleDisabled
    , trpDescription
    , trpSql
    , trpActions

    -- * TransferData
    , TransferData
    , transferData
    , tdTransferDate
    , tdAcceptDate
    , tdTransferMessage
    , tdRejectDate
    , tdRejectReason
    ) where

import           Network.AWS.IoT.Types.Product
import           Network.AWS.IoT.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
ioT :: Service
ioT =
    Service
    { _svcAbbrev = "IoT"
    , _svcSigner = v4
    , _svcPrefix = "iot"
    , _svcVersion = "2015-05-28"
    , _svcEndpoint = defaultEndpoint ioT
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "IoT"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.
--
--
_CertificateConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateConflictException =
    _MatchServiceError ioT "CertificateConflictException" . hasStatus 409

-- | The Rule-SQL expression can't be parsed correctly.
--
--
_SqlParseException :: AsError a => Getting (First ServiceError) a ServiceError
_SqlParseException = _MatchServiceError ioT "SqlParseException" . hasStatus 400

-- | The request is not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
    _MatchServiceError ioT "InvalidRequestException" . hasStatus 400

-- | You can't transfer the certificate because authorization policies are still attached.
--
--
_TransferConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_TransferConflictException =
    _MatchServiceError ioT "TransferConflictException" . hasStatus 409

-- | The certificate operation is not allowed.
--
--
_CertificateStateException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateStateException =
    _MatchServiceError ioT "CertificateStateException" . hasStatus 406

-- | The registration code is invalid.
--
--
_RegistrationCodeValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_RegistrationCodeValidationException =
    _MatchServiceError ioT "RegistrationCodeValidationException" .
    hasStatus 400

-- | The policy documentation is not valid.
--
--
_MalformedPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyException =
    _MatchServiceError ioT "MalformedPolicyException" . hasStatus 400

-- | You can't delete the resource because it is attached to one or more resources.
--
--
_DeleteConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_DeleteConflictException =
    _MatchServiceError ioT "DeleteConflictException" . hasStatus 409

-- | The resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
    _MatchServiceError ioT "ResourceAlreadyExistsException" . hasStatus 409

-- | The certificate is invalid.
--
--
_CertificateValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateValidationException =
    _MatchServiceError ioT "CertificateValidationException" . hasStatus 400

-- | You can't revert the certificate transfer because the transfer is already complete.
--
--
_TransferAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_TransferAlreadyCompletedException =
    _MatchServiceError ioT "TransferAlreadyCompletedException" . hasStatus 410

-- | The rate exceeds the limit.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
    _MatchServiceError ioT "ThrottlingException" . hasStatus 429

-- | An unexpected error has occurred.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
    _MatchServiceError ioT "InternalFailureException" . hasStatus 500

-- | The number of policy versions exceeds the limit.
--
--
_VersionsLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_VersionsLimitExceededException =
    _MatchServiceError ioT "VersionsLimitExceededException" . hasStatus 409

-- | The service is temporarily unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _MatchServiceError ioT "ServiceUnavailableException" . hasStatus 503

-- | An unexpected error has occurred.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException = _MatchServiceError ioT "InternalException" . hasStatus 500

-- | An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.
--
--
_VersionConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_VersionConflictException =
    _MatchServiceError ioT "VersionConflictException" . hasStatus 409

-- | You are not authorized to perform this operation.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
    _MatchServiceError ioT "UnauthorizedException" . hasStatus 401

-- | The specified resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _MatchServiceError ioT "ResourceNotFoundException" . hasStatus 404

-- | The number of attached entities exceeds the limit.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _MatchServiceError ioT "LimitExceededException" . hasStatus 410
