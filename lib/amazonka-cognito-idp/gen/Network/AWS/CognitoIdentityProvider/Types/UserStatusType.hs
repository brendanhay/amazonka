{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserStatusType where

import Network.AWS.Prelude

data UserStatusType
  = Archived
  | Compromised
  | Confirmed
  | ForceChangePassword
  | ResetRequired
  | Unconfirmed
  | Unknown
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

instance FromText UserStatusType where
  parser =
    takeLowerText >>= \case
      "archived" -> pure Archived
      "compromised" -> pure Compromised
      "confirmed" -> pure Confirmed
      "force_change_password" -> pure ForceChangePassword
      "reset_required" -> pure ResetRequired
      "unconfirmed" -> pure Unconfirmed
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing UserStatusType from value: '" <> e
            <> "'. Accepted values: archived, compromised, confirmed, force_change_password, reset_required, unconfirmed, unknown"

instance ToText UserStatusType where
  toText = \case
    Archived -> "ARCHIVED"
    Compromised -> "COMPROMISED"
    Confirmed -> "CONFIRMED"
    ForceChangePassword -> "FORCE_CHANGE_PASSWORD"
    ResetRequired -> "RESET_REQUIRED"
    Unconfirmed -> "UNCONFIRMED"
    Unknown -> "UNKNOWN"

instance Hashable UserStatusType

instance NFData UserStatusType

instance ToByteString UserStatusType

instance ToQuery UserStatusType

instance ToHeader UserStatusType

instance FromJSON UserStatusType where
  parseJSON = parseJSONText "UserStatusType"
