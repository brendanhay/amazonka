{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types
    (
    -- * Service Configuration
      ssm

    -- * Errors
    , _UnsupportedPlatformType
    , _InvalidPermissionType
    , _AssociatedInstances
    , _InvalidInstanceId
    , _StatusUnchanged
    , _InvalidNextToken
    , _InvalidOutputFolder
    , _InvalidActivationId
    , _InvalidCommandId
    , _DuplicateInstanceId
    , _InvalidResourceType
    , _InvalidDocument
    , _InvalidFilterKey
    , _InvalidInstanceInformationFilterValue
    , _AssociationAlreadyExists
    , _InvalidDocumentContent
    , _AssociationLimitExceeded
    , _AssociationDoesNotExist
    , _InternalServerError
    , _InvalidRole
    , _TooManyUpdates
    , _InvalidActivation
    , _MaxDocumentSizeExceeded
    , _InvalidDocumentOperation
    , _InvalidParameters
    , _InvalidResourceId
    , _InvalidNotificationConfig
    , _DocumentPermissionLimit
    , _DocumentAlreadyExists
    , _DocumentLimitExceeded
    , _InvalidFilter

    -- * AssociationFilterKey
    , AssociationFilterKey (..)

    -- * AssociationStatusName
    , AssociationStatusName (..)

    -- * CommandFilterKey
    , CommandFilterKey (..)

    -- * CommandInvocationStatus
    , CommandInvocationStatus (..)

    -- * CommandPluginStatus
    , CommandPluginStatus (..)

    -- * CommandStatus
    , CommandStatus (..)

    -- * DescribeActivationsFilterKeys
    , DescribeActivationsFilterKeys (..)

    -- * DocumentFilterKey
    , DocumentFilterKey (..)

    -- * DocumentHashType
    , DocumentHashType (..)

    -- * DocumentParameterType
    , DocumentParameterType (..)

    -- * DocumentPermissionType
    , DocumentPermissionType (..)

    -- * DocumentStatus
    , DocumentStatus (..)

    -- * Fault
    , Fault (..)

    -- * InstanceInformationFilterKey
    , InstanceInformationFilterKey (..)

    -- * NotificationEvent
    , NotificationEvent (..)

    -- * NotificationType
    , NotificationType (..)

    -- * PingStatus
    , PingStatus (..)

    -- * PlatformType
    , PlatformType (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ResourceTypeForTagging
    , ResourceTypeForTagging (..)

    -- * Activation
    , Activation
    , activation
    , aExpired
    , aDefaultInstanceName
    , aActivationId
    , aCreatedDate
    , aRegistrationLimit
    , aExpirationDate
    , aDescription
    , aRegistrationsCount
    , aIAMRole

    -- * Association
    , Association
    , association
    , aInstanceId
    , aName

    -- * AssociationDescription
    , AssociationDescription
    , associationDescription
    , adInstanceId
    , adStatus
    , adDate
    , adName
    , adParameters

    -- * AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- * AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asName
    , asMessage

    -- * Command
    , Command
    , command
    , cStatus
    , cExpiresAfter
    , cNotificationConfig
    , cOutputS3KeyPrefix
    , cDocumentName
    , cInstanceIds
    , cCommandId
    , cParameters
    , cComment
    , cOutputS3BucketName
    , cRequestedDateTime
    , cServiceRole

    -- * CommandFilter
    , CommandFilter
    , commandFilter
    , cfKey
    , cfValue

    -- * CommandInvocation
    , CommandInvocation
    , commandInvocation
    , ciInstanceId
    , ciStatus
    , ciNotificationConfig
    , ciCommandPlugins
    , ciDocumentName
    , ciCommandId
    , ciComment
    , ciTraceOutput
    , ciRequestedDateTime
    , ciServiceRole

    -- * CommandPlugin
    , CommandPlugin
    , commandPlugin
    , cpStatus
    , cpResponseStartDateTime
    , cpOutputS3KeyPrefix
    , cpResponseCode
    , cpOutput
    , cpName
    , cpOutputS3BucketName
    , cpResponseFinishDateTime

    -- * CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreName
    , cabreParameters

    -- * DescribeActivationsFilter
    , DescribeActivationsFilter
    , describeActivationsFilter
    , dafFilterKey
    , dafFilterValues

    -- * DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dHash
    , dSha1
    , dOwner
    , dPlatformTypes
    , dCreatedDate
    , dName
    , dHashType
    , dParameters
    , dDescription

    -- * DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- * DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diOwner
    , diPlatformTypes
    , diName

    -- * DocumentParameter
    , DocumentParameter
    , documentParameter
    , dpName
    , dpDefaultValue
    , dpType
    , dpDescription

    -- * FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage

    -- * InstanceInformation
    , InstanceInformation
    , instanceInformation
    , iiInstanceId
    , iiPingStatus
    , iiIPAddress
    , iiResourceType
    , iiRegistrationDate
    , iiPlatformVersion
    , iiIsLatestVersion
    , iiAgentVersion
    , iiLastPingDateTime
    , iiActivationId
    , iiName
    , iiPlatformType
    , iiPlatformName
    , iiComputerName
    , iiIAMRole

    -- * InstanceInformationFilter
    , InstanceInformationFilter
    , instanceInformationFilter
    , iifKey
    , iifValueSet

    -- * NotificationConfig
    , NotificationConfig
    , notificationConfig
    , ncNotificationEvents
    , ncNotificationType
    , ncNotificationARN

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.SSM.Types.Product
import           Network.AWS.SSM.Types.Sum

-- | API version '2014-11-06' of the Amazon Simple Systems Management Service SDK configuration.
ssm :: Service
ssm =
    Service
    { _svcAbbrev = "SSM"
    , _svcSigner = v4
    , _svcPrefix = "ssm"
    , _svcVersion = "2014-11-06"
    , _svcEndpoint = defaultEndpoint ssm
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SSM"
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

-- | The document does not support the platform type of the given instance ID(s). For example, you sent an SSM document for a Windows instance to a Linux instance.
_UnsupportedPlatformType :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlatformType = _ServiceError . hasCode "UnsupportedPlatformType"

-- | The permission type is not supported. /Share/ is the only supported permission type.
_InvalidPermissionType :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPermissionType = _ServiceError . hasCode "InvalidPermissionType"

-- | You must disassociate an SSM document from all instances before you can delete it.
_AssociatedInstances :: AsError a => Getting (First ServiceError) a ServiceError
_AssociatedInstances = _ServiceError . hasCode "AssociatedInstances"

-- | The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.
_InvalidInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceId = _ServiceError . hasCode "InvalidInstanceId"

-- | The updated status is the same as the current status.
_StatusUnchanged :: AsError a => Getting (First ServiceError) a ServiceError
_StatusUnchanged = _ServiceError . hasCode "StatusUnchanged"

-- | The specified token is not valid.
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasCode "InvalidNextToken"

-- | The S3 bucket does not exist.
_InvalidOutputFolder :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOutputFolder = _ServiceError . hasCode "InvalidOutputFolder"

-- | The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.
_InvalidActivationId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActivationId = _ServiceError . hasCode "InvalidActivationId"

-- | Prism for InvalidCommandId' errors.
_InvalidCommandId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommandId = _ServiceError . hasCode "InvalidCommandId"

-- | You cannot specify an instance ID in more than one association.
_DuplicateInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateInstanceId = _ServiceError . hasCode "DuplicateInstanceId"

-- | The resource type is not valid. If you are attempting to tag an instance, the instance must be a registered, managed instance.
_InvalidResourceType :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceType = _ServiceError . hasCode "InvalidResourceType"

-- | The specified document does not exist.
_InvalidDocument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocument = _ServiceError . hasCode "InvalidDocument"

-- | The specified key is not valid.
_InvalidFilterKey :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilterKey = _ServiceError . hasCode "InvalidFilterKey"

-- | The specified filter value is not valid.
_InvalidInstanceInformationFilterValue :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceInformationFilterValue =
    _ServiceError . hasCode "InvalidInstanceInformationFilterValue"

-- | The specified association already exists.
_AssociationAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationAlreadyExists = _ServiceError . hasCode "AssociationAlreadyExists"

-- | The content for the SSM document is not valid.
_InvalidDocumentContent :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentContent = _ServiceError . hasCode "InvalidDocumentContent"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationLimitExceeded = _ServiceError . hasCode "AssociationLimitExceeded"

-- | The specified association does not exist.
_AssociationDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationDoesNotExist = _ServiceError . hasCode "AssociationDoesNotExist"

-- | An error occurred on the server side.
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | The role name can\'t contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for SSM notifications, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/rc-sns.html Configuring SNS Notifications SSM> in the /Amazon Elastic Compute Cloud User Guide/ .
_InvalidRole :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRole = _ServiceError . hasCode "InvalidRole"

-- | There are concurrent updates for a resource that supports one update at a time.
_TooManyUpdates :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyUpdates = _ServiceError . hasCode "TooManyUpdates"

-- | The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.
_InvalidActivation :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActivation = _ServiceError . hasCode "InvalidActivation"

-- | The size limit of an SSM document is 64 KB.
_MaxDocumentSizeExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_MaxDocumentSizeExceeded = _ServiceError . hasCode "MaxDocumentSizeExceeded"

-- | You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.
_InvalidDocumentOperation :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentOperation = _ServiceError . hasCode "InvalidDocumentOperation"

-- | You must specify values for all required parameters in the SSM document. You can only supply values to parameters defined in the SSM document.
_InvalidParameters :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameters = _ServiceError . hasCode "InvalidParameters"

-- | The resource ID is not valid. Verify that you entered the correct ID and try again.
_InvalidResourceId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceId = _ServiceError . hasCode "InvalidResourceId"

-- | One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.
_InvalidNotificationConfig :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNotificationConfig =
    _ServiceError . hasCode "InvalidNotificationConfig"

-- | The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.
_DocumentPermissionLimit :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentPermissionLimit = _ServiceError . hasCode "DocumentPermissionLimit"

-- | The specified SSM document already exists.
_DocumentAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentAlreadyExists = _ServiceError . hasCode "DocumentAlreadyExists"

-- | You can have at most 200 active SSM documents.
_DocumentLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentLimitExceeded = _ServiceError . hasCode "DocumentLimitExceeded"

-- | The filter name is not valid. Verify the you entered the correct name and try again.
_InvalidFilter :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilter = _ServiceError . hasCode "InvalidFilter"
