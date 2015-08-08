{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types
    (
    -- * Service Decription
      CloudHSM

    -- * Error Matchers
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

import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.CloudHSM.Types.Sum
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

-- | Indicates that one or more of the request parameters are not valid.
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _ServiceError . hasCode "InvalidRequestException"

-- | Indicates that an exception occurred in the AWS CloudHSM service.
_CloudHSMServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_CloudHSMServiceException = _ServiceError . hasCode "CloudHsmServiceException"

-- | Indicates that an internal error occurred.
_CloudHSMInternalException :: AsError a => Getting (First ServiceError) a ServiceError
_CloudHSMInternalException =
    _ServiceError . hasCode "CloudHsmInternalException"
