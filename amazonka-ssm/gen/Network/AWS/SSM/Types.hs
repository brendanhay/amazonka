{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types
    (
    -- * Service Decription
      SSM

    -- * Error Matchers
    , _AssociatedInstances
    , _InvalidNextToken
    , _InvalidInstanceId
    , _StatusUnchanged
    , _DuplicateInstanceId
    , _InvalidDocument
    , _AssociationLimitExceeded
    , _InvalidDocumentContent
    , _AssociationAlreadyExists
    , _AssociationDoesNotExist
    , _InternalServerError
    , _MaxDocumentSizeExceeded
    , _TooManyUpdates
    , _DocumentAlreadyExists
    , _DocumentLimitExceeded

    -- * AssociationFilterKey
    , AssociationFilterKey (..)

    -- * AssociationStatusName
    , AssociationStatusName (..)

    -- * DocumentFilterKey
    , DocumentFilterKey (..)

    -- * DocumentStatus
    , DocumentStatus (..)

    -- * Fault
    , Fault (..)

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

    -- * CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreName

    -- * DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dSha1
    , dCreatedDate
    , dName

    -- * DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- * DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diName

    -- * FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.SSM.Types.Product
import           Network.AWS.SSM.Types.Sum

-- | Version @2014-11-06@ of the Amazon Simple Systems Management Service SDK.
data SSM

instance AWSService SSM where
    type Sg SSM = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SSM"
            , _svcPrefix = "ssm"
            , _svcVersion = "2014-11-06"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | You must disassociate a configuration document from all instances before
-- you can delete it.
_AssociatedInstances :: AsError a => Getting (First ServiceError) a ServiceError
_AssociatedInstances =
    _ServiceError . hasStatus 400 . hasCode "AssociatedInstances"

-- | The specified token is not valid.
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | You must specify the ID of a running instance.
_InvalidInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceId =
    _ServiceError . hasStatus 404 . hasCode "InvalidInstanceId"

-- | The updated status is the same as the current status.
_StatusUnchanged :: AsError a => Getting (First ServiceError) a ServiceError
_StatusUnchanged = _ServiceError . hasStatus 400 . hasCode "StatusUnchanged"

-- | You cannot specify an instance ID in more than one association.
_DuplicateInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateInstanceId =
    _ServiceError . hasStatus 404 . hasCode "DuplicateInstanceId"

-- | The configuration document is not valid.
_InvalidDocument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocument = _ServiceError . hasStatus 404 . hasCode "InvalidDocument"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "AssociationLimitExceeded"

-- | The content for the configuration document is not valid.
_InvalidDocumentContent :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentContent =
    _ServiceError . hasStatus 400 . hasCode "InvalidDocumentContent"

-- | The specified association already exists.
_AssociationAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationAlreadyExists =
    _ServiceError . hasStatus 400 . hasCode "AssociationAlreadyExists"

-- | The specified association does not exist.
_AssociationDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationDoesNotExist =
    _ServiceError . hasStatus 404 . hasCode "AssociationDoesNotExist"

-- | An error occurred on the server side.
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError =
    _ServiceError . hasStatus 500 . hasCode "InternalServerError"

-- | The size limit of a configuration document is 64 KB.
_MaxDocumentSizeExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_MaxDocumentSizeExceeded =
    _ServiceError . hasStatus 400 . hasCode "MaxDocumentSizeExceeded"

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyUpdates = _ServiceError . hasStatus 429 . hasCode "TooManyUpdates"

-- | The specified configuration document already exists.
_DocumentAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentAlreadyExists =
    _ServiceError . hasStatus 400 . hasCode "DocumentAlreadyExists"

-- | You can have at most 100 active configuration documents.
_DocumentLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "DocumentLimitExceeded"
