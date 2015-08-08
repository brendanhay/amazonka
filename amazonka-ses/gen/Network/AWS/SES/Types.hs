{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types
    (
    -- * Service Decription
      SES

    -- * Error Matchers
    , _MessageRejected
    , _InvalidPolicyException

    -- * IdentityType
    , IdentityType (..)

    -- * NotificationType
    , NotificationType (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * Body
    , Body
    , body
    , bText
    , bHTML

    -- * Content
    , Content
    , content
    , cCharset
    , cData

    -- * Destination
    , Destination
    , destination
    , dBCCAddresses
    , dCCAddresses
    , dToAddresses

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes
    , identityDkimAttributes
    , idaDkimTokens
    , idaDkimEnabled
    , idaDkimVerificationStatus

    -- * IdentityNotificationAttributes
    , IdentityNotificationAttributes
    , identityNotificationAttributes
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes
    , identityVerificationAttributes
    , ivaVerificationToken
    , ivaVerificationStatus

    -- * Message
    , Message
    , message
    , mSubject
    , mBody

    -- * RawMessage
    , RawMessage
    , rawMessage
    , rmData

    -- * SendDataPoint
    , SendDataPoint
    , sendDataPoint
    , sdpRejects
    , sdpComplaints
    , sdpDeliveryAttempts
    , sdpBounces
    , sdpTimestamp
    ) where

import           Network.AWS.Prelude
import           Network.AWS.SES.Types.Product
import           Network.AWS.SES.Types.Sum
import           Network.AWS.Sign.V4

-- | Version @2010-12-01@ of the Amazon Simple Email Service SDK.
data SES

instance AWSService SES where
    type Sg SES = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SES"
            , _svcPrefix = "email"
            , _svcVersion = "2010-12-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
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

-- | Indicates that the action failed, and the message could not be sent.
-- Check the error stack for more information about what caused the error.
_MessageRejected :: AsError a => Getting (First ServiceError) a ServiceError
_MessageRejected = _ServiceError . hasStatus 400 . hasCode "MessageRejected"

-- | Indicates that the provided policy is invalid. Check the error stack for
-- more information about what caused the error.
_InvalidPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPolicyException =
    _ServiceError . hasStatus 400 . hasCode "InvalidPolicy"
