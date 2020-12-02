{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.ActiveDirectoryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ActiveDirectoryStatus where

import Network.AWS.Prelude

data ActiveDirectoryStatus
  = AccessDenied
  | Detached
  | Joined
  | Joining
  | NetworkError
  | Timeout
  | UnknownError
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

instance FromText ActiveDirectoryStatus where
  parser =
    takeLowerText >>= \case
      "access_denied" -> pure AccessDenied
      "detached" -> pure Detached
      "joined" -> pure Joined
      "joining" -> pure Joining
      "network_error" -> pure NetworkError
      "timeout" -> pure Timeout
      "unknown_error" -> pure UnknownError
      e ->
        fromTextError $
          "Failure parsing ActiveDirectoryStatus from value: '" <> e
            <> "'. Accepted values: access_denied, detached, joined, joining, network_error, timeout, unknown_error"

instance ToText ActiveDirectoryStatus where
  toText = \case
    AccessDenied -> "ACCESS_DENIED"
    Detached -> "DETACHED"
    Joined -> "JOINED"
    Joining -> "JOINING"
    NetworkError -> "NETWORK_ERROR"
    Timeout -> "TIMEOUT"
    UnknownError -> "UNKNOWN_ERROR"

instance Hashable ActiveDirectoryStatus

instance NFData ActiveDirectoryStatus

instance ToByteString ActiveDirectoryStatus

instance ToQuery ActiveDirectoryStatus

instance ToHeader ActiveDirectoryStatus

instance FromJSON ActiveDirectoryStatus where
  parseJSON = parseJSONText "ActiveDirectoryStatus"
