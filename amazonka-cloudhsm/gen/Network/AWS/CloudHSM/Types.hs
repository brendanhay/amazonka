{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudHSM.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , JSONError

    -- * CloudHsmObjectState
    , CloudHsmObjectState (..)

    -- * SubscriptionType
    , SubscriptionType (..)

    -- * HsmStatus
    , HsmStatus (..)

    -- * ClientVersion
    , ClientVersion (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-05-30@ of the Amazon CloudHSM service.
data CloudHSM

instance AWSService CloudHSM where
    type Sg CloudHSM = V4
    type Er CloudHSM = JSONError

    service = service'
      where
        service' :: Service CloudHSM
        service' = Service
            { _svcAbbrev       = "CloudHSM"
            , _svcPrefix       = "cloudhsm"
            , _svcVersion      = "2014-05-30"
            , _svcTargetPrefix = Just "CloudHsmFrontendService"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CloudHSM
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data CloudHsmObjectState
    = Degraded -- ^ DEGRADED
    | Ready    -- ^ READY
    | Updating -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CloudHsmObjectState

instance FromText CloudHsmObjectState where
    parser = takeLowerText >>= \case
        "degraded" -> pure Degraded
        "ready"    -> pure Ready
        "updating" -> pure Updating
        e          -> fail $
            "Failure parsing CloudHsmObjectState from " ++ show e

instance ToText CloudHsmObjectState where
    toText = \case
        Degraded -> "DEGRADED"
        Ready    -> "READY"
        Updating -> "UPDATING"

instance ToByteString CloudHsmObjectState
instance ToHeader     CloudHsmObjectState
instance ToQuery      CloudHsmObjectState

instance FromJSON CloudHsmObjectState where
    parseJSON = parseJSONText "CloudHsmObjectState"

instance ToJSON CloudHsmObjectState where
    toJSON = toJSONText

data SubscriptionType
    = Production -- ^ PRODUCTION
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SubscriptionType

instance FromText SubscriptionType where
    parser = takeLowerText >>= \case
        "production" -> pure Production
        e            -> fail $
            "Failure parsing SubscriptionType from " ++ show e

instance ToText SubscriptionType where
    toText Production = "PRODUCTION"

instance ToByteString SubscriptionType
instance ToHeader     SubscriptionType
instance ToQuery      SubscriptionType

instance FromJSON SubscriptionType where
    parseJSON = parseJSONText "SubscriptionType"

instance ToJSON SubscriptionType where
    toJSON = toJSONText

data HsmStatus
    = HSDegraded    -- ^ DEGRADED
    | HSPending     -- ^ PENDING
    | HSRunning     -- ^ RUNNING
    | HSSuspended   -- ^ SUSPENDED
    | HSTerminated  -- ^ TERMINATED
    | HSTerminating -- ^ TERMINATING
    | HSUpdating    -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable HsmStatus

instance FromText HsmStatus where
    parser = takeLowerText >>= \case
        "degraded"    -> pure HSDegraded
        "pending"     -> pure HSPending
        "running"     -> pure HSRunning
        "suspended"   -> pure HSSuspended
        "terminated"  -> pure HSTerminated
        "terminating" -> pure HSTerminating
        "updating"    -> pure HSUpdating
        e             -> fail $
            "Failure parsing HsmStatus from " ++ show e

instance ToText HsmStatus where
    toText = \case
        HSDegraded    -> "DEGRADED"
        HSPending     -> "PENDING"
        HSRunning     -> "RUNNING"
        HSSuspended   -> "SUSPENDED"
        HSTerminated  -> "TERMINATED"
        HSTerminating -> "TERMINATING"
        HSUpdating    -> "UPDATING"

instance ToByteString HsmStatus
instance ToHeader     HsmStatus
instance ToQuery      HsmStatus

instance FromJSON HsmStatus where
    parseJSON = parseJSONText "HsmStatus"

instance ToJSON HsmStatus where
    toJSON = toJSONText

data ClientVersion
    = 51 -- ^ 5.1
    | 53 -- ^ 5.3
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ClientVersion

instance FromText ClientVersion where
    parser = takeLowerText >>= \case
        "5.1" -> pure 51
        "5.3" -> pure 53
        e     -> fail $
            "Failure parsing ClientVersion from " ++ show e

instance ToText ClientVersion where
    toText = \case
        51 -> "5.1"
        53 -> "5.3"

instance ToByteString ClientVersion
instance ToHeader     ClientVersion
instance ToQuery      ClientVersion

instance FromJSON ClientVersion where
    parseJSON = parseJSONText "ClientVersion"

instance ToJSON ClientVersion where
    toJSON = toJSONText
