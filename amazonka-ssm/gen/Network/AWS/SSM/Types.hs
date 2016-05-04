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
    , _AssociatedInstances
    , _InvalidInstanceId
    , _StatusUnchanged
    , _InvalidNextToken
    , _InvalidOutputFolder
    , _InvalidCommandId
    , _DuplicateInstanceId
    , _InvalidDocument
    , _InvalidFilterKey
    , _InvalidInstanceInformationFilterValue
    , _AssociationAlreadyExists
    , _InvalidDocumentContent
    , _AssociationLimitExceeded
    , _AssociationDoesNotExist
    , _InternalServerError
    , _TooManyUpdates
    , _MaxDocumentSizeExceeded
    , _InvalidParameters
    , _DocumentAlreadyExists
    , _DocumentLimitExceeded

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

    -- * DocumentFilterKey
    , DocumentFilterKey (..)

    -- * DocumentParameterType
    , DocumentParameterType (..)

    -- * DocumentStatus
    , DocumentStatus (..)

    -- * Fault
    , Fault (..)

    -- * InstanceInformationFilterKey
    , InstanceInformationFilterKey (..)

    -- * PingStatus
    , PingStatus (..)

    -- * PlatformType
    , PlatformType (..)

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
    , cOutputS3KeyPrefix
    , cDocumentName
    , cInstanceIds
    , cCommandId
    , cParameters
    , cComment
    , cOutputS3BucketName
    , cRequestedDateTime

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
    , ciCommandPlugins
    , ciDocumentName
    , ciCommandId
    , ciComment
    , ciTraceOutput
    , ciRequestedDateTime

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

    -- * DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dSha1
    , dPlatformTypes
    , dCreatedDate
    , dName
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
    , iiPlatformVersion
    , iiIsLatestVersion
    , iiAgentVersion
    , iiLastPingDateTime
    , iiPlatformType
    , iiPlatformName

    -- * InstanceInformationFilter
    , InstanceInformationFilter
    , instanceInformationFilter
    , iifKey
    , iifValueSet
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
    , _svcError = parseJSONError
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

-- | The document does not support the platform type of the given instance
-- ID(s).
_UnsupportedPlatformType :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlatformType = _ServiceError . hasCode "UnsupportedPlatformType"

-- | You must disassociate an SSM document from all instances before you can
-- delete it.
_AssociatedInstances :: AsError a => Getting (First ServiceError) a ServiceError
_AssociatedInstances = _ServiceError . hasCode "AssociatedInstances"

-- | The instance is not in valid state. Valid states are: Running, Pending,
-- Stopped, Stopping. Invalid states are: Shutting-down and Terminated.
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

-- | Prism for InvalidCommandId' errors.
_InvalidCommandId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommandId = _ServiceError . hasCode "InvalidCommandId"

-- | You cannot specify an instance ID in more than one association.
_DuplicateInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateInstanceId = _ServiceError . hasCode "DuplicateInstanceId"

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

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyUpdates = _ServiceError . hasCode "TooManyUpdates"

-- | The size limit of an SSM document is 64 KB.
_MaxDocumentSizeExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_MaxDocumentSizeExceeded = _ServiceError . hasCode "MaxDocumentSizeExceeded"

-- | You must specify values for all required parameters in the SSM document.
-- You can only supply values to parameters defined in the SSM document.
_InvalidParameters :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameters = _ServiceError . hasCode "InvalidParameters"

-- | The specified SSM document already exists.
_DocumentAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentAlreadyExists = _ServiceError . hasCode "DocumentAlreadyExists"

-- | You can have at most 100 active SSM documents.
_DocumentLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentLimitExceeded = _ServiceError . hasCode "DocumentLimitExceeded"
