{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ConnectionErrorCodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ConnectionErrorCodeType where

import Network.AWS.Prelude

data ConnectionErrorCodeType
  = ClusterNotFound
  | InsufficientCloudhsmHSMs
  | InternalError
  | InvalidCredentials
  | NetworkErrors
  | SubnetNotFound
  | UserLockedOut
  | UserLoggedIn
  | UserNotFound
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ConnectionErrorCodeType where
  parser =
    takeLowerText >>= \case
      "cluster_not_found" -> pure ClusterNotFound
      "insufficient_cloudhsm_hsms" -> pure InsufficientCloudhsmHSMs
      "internal_error" -> pure InternalError
      "invalid_credentials" -> pure InvalidCredentials
      "network_errors" -> pure NetworkErrors
      "subnet_not_found" -> pure SubnetNotFound
      "user_locked_out" -> pure UserLockedOut
      "user_logged_in" -> pure UserLoggedIn
      "user_not_found" -> pure UserNotFound
      e ->
        fromTextError $
          "Failure parsing ConnectionErrorCodeType from value: '" <> e
            <> "'. Accepted values: cluster_not_found, insufficient_cloudhsm_hsms, internal_error, invalid_credentials, network_errors, subnet_not_found, user_locked_out, user_logged_in, user_not_found"

instance ToText ConnectionErrorCodeType where
  toText = \case
    ClusterNotFound -> "CLUSTER_NOT_FOUND"
    InsufficientCloudhsmHSMs -> "INSUFFICIENT_CLOUDHSM_HSMS"
    InternalError -> "INTERNAL_ERROR"
    InvalidCredentials -> "INVALID_CREDENTIALS"
    NetworkErrors -> "NETWORK_ERRORS"
    SubnetNotFound -> "SUBNET_NOT_FOUND"
    UserLockedOut -> "USER_LOCKED_OUT"
    UserLoggedIn -> "USER_LOGGED_IN"
    UserNotFound -> "USER_NOT_FOUND"

instance Hashable ConnectionErrorCodeType

instance NFData ConnectionErrorCodeType

instance ToByteString ConnectionErrorCodeType

instance ToQuery ConnectionErrorCodeType

instance ToHeader ConnectionErrorCodeType

instance FromJSON ConnectionErrorCodeType where
  parseJSON = parseJSONText "ConnectionErrorCodeType"
