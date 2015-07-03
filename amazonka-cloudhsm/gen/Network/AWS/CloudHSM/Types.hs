{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudHSM.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudHSM.Types
    (
    -- * Service
      CloudHSM

    -- * Errors
    , _InvalidRequestException
    , _CloudHSMServiceException
    , _CloudHSMInternalException

    -- * ClientVersion
    , ClientVersion (..)

    -- * CloudHSMObjectState
    , CloudHSMObjectState (..)

    -- * HSMStatus
    , HSMStatus (..)

    -- * SubscriptionType
    , SubscriptionType (..)
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-05-30@ of the Amazon CloudHSM SDK.
data CloudHSM

instance AWSService CloudHSM where
    type Sg CloudHSM = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudHSM"
            , _svcPrefix = "cloudhsm"
            , _svcVersion = "2014-05-30"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
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

-- | Indicates that one or more of the request parameters are not valid.
_InvalidRequestException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _ServiceError . hasCode "InvalidRequestException"

-- | Indicates that an exception occurred in the AWS CloudHSM service.
_CloudHSMServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_CloudHSMServiceException = _ServiceError . hasCode "CloudHsmServiceException"

-- | Indicates that an internal error occurred.
_CloudHSMInternalException :: AWSError a => Getting (First ServiceError) a ServiceError
_CloudHSMInternalException =
    _ServiceError . hasCode "CloudHsmInternalException"

data ClientVersion
    = V51
    | V53
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ClientVersion where
    parser = takeLowerText >>= \case
        "5.1" -> pure V51
        "5.3" -> pure V53
        e -> fromTextError $ "Failure parsing ClientVersion from value: '" <> e
           <> "'. Accepted values: 5.1, 5.3"

instance ToText ClientVersion where
    toText = \case
        V51 -> "5.1"
        V53 -> "5.3"

instance Hashable ClientVersion where
    hashWithSalt = hashUsing fromEnum

instance ToQuery ClientVersion
instance ToHeader ClientVersion

instance ToJSON ClientVersion where
    toJSON = toJSONText

data CloudHSMObjectState
    = Updating
    | Degraded
    | Ready
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText CloudHSMObjectState where
    parser = takeLowerText >>= \case
        "degraded" -> pure Degraded
        "ready" -> pure Ready
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing CloudHSMObjectState from value: '" <> e
           <> "'. Accepted values: degraded, ready, updating"

instance ToText CloudHSMObjectState where
    toText = \case
        Degraded -> "degraded"
        Ready -> "ready"
        Updating -> "updating"

instance Hashable CloudHSMObjectState where
    hashWithSalt = hashUsing fromEnum

instance ToQuery CloudHSMObjectState
instance ToHeader CloudHSMObjectState

instance FromJSON CloudHSMObjectState where
    parseJSON = parseJSONText "CloudHSMObjectState"

data HSMStatus
    = HSRunning
    | HSUpdating
    | HSTerminated
    | HSPending
    | HSTerminating
    | HSSuspended
    | HSDegraded
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText HSMStatus where
    parser = takeLowerText >>= \case
        "degraded" -> pure HSDegraded
        "pending" -> pure HSPending
        "running" -> pure HSRunning
        "suspended" -> pure HSSuspended
        "terminated" -> pure HSTerminated
        "terminating" -> pure HSTerminating
        "updating" -> pure HSUpdating
        e -> fromTextError $ "Failure parsing HSMStatus from value: '" <> e
           <> "'. Accepted values: degraded, pending, running, suspended, terminated, terminating, updating"

instance ToText HSMStatus where
    toText = \case
        HSDegraded -> "degraded"
        HSPending -> "pending"
        HSRunning -> "running"
        HSSuspended -> "suspended"
        HSTerminated -> "terminated"
        HSTerminating -> "terminating"
        HSUpdating -> "updating"

instance Hashable HSMStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery HSMStatus
instance ToHeader HSMStatus

instance FromJSON HSMStatus where
    parseJSON = parseJSONText "HSMStatus"

data SubscriptionType =
    Production
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText SubscriptionType where
    parser = takeLowerText >>= \case
        "production" -> pure Production
        e -> fromTextError $ "Failure parsing SubscriptionType from value: '" <> e
           <> "'. Accepted values: production"

instance ToText SubscriptionType where
    toText = \case
        Production -> "production"

instance Hashable SubscriptionType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery SubscriptionType
instance ToHeader SubscriptionType

instance ToJSON SubscriptionType where
    toJSON = toJSONText

instance FromJSON SubscriptionType where
    parseJSON = parseJSONText "SubscriptionType"
