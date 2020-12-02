{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetHealthReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealthReason where

import Network.AWS.Prelude

data TargetHealthReason
  = AuthFailure
  | ConnectionFailed
  | PendingProxyCapacity
  | Unreachable
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

instance FromText TargetHealthReason where
  parser =
    takeLowerText >>= \case
      "auth_failure" -> pure AuthFailure
      "connection_failed" -> pure ConnectionFailed
      "pending_proxy_capacity" -> pure PendingProxyCapacity
      "unreachable" -> pure Unreachable
      e ->
        fromTextError $
          "Failure parsing TargetHealthReason from value: '" <> e
            <> "'. Accepted values: auth_failure, connection_failed, pending_proxy_capacity, unreachable"

instance ToText TargetHealthReason where
  toText = \case
    AuthFailure -> "AUTH_FAILURE"
    ConnectionFailed -> "CONNECTION_FAILED"
    PendingProxyCapacity -> "PENDING_PROXY_CAPACITY"
    Unreachable -> "UNREACHABLE"

instance Hashable TargetHealthReason

instance NFData TargetHealthReason

instance ToByteString TargetHealthReason

instance ToQuery TargetHealthReason

instance ToHeader TargetHealthReason

instance FromXML TargetHealthReason where
  parseXML = parseXMLText "TargetHealthReason"
