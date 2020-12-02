{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum where

import Network.AWS.Prelude

data AuthenticateCognitoActionConditionalBehaviorEnum
  = Allow
  | Authenticate
  | Deny
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

instance FromText AuthenticateCognitoActionConditionalBehaviorEnum where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "authenticate" -> pure Authenticate
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing AuthenticateCognitoActionConditionalBehaviorEnum from value: '" <> e
            <> "'. Accepted values: allow, authenticate, deny"

instance ToText AuthenticateCognitoActionConditionalBehaviorEnum where
  toText = \case
    Allow -> "allow"
    Authenticate -> "authenticate"
    Deny -> "deny"

instance Hashable AuthenticateCognitoActionConditionalBehaviorEnum

instance NFData AuthenticateCognitoActionConditionalBehaviorEnum

instance ToByteString AuthenticateCognitoActionConditionalBehaviorEnum

instance ToQuery AuthenticateCognitoActionConditionalBehaviorEnum

instance ToHeader AuthenticateCognitoActionConditionalBehaviorEnum

instance FromXML AuthenticateCognitoActionConditionalBehaviorEnum where
  parseXML = parseXMLText "AuthenticateCognitoActionConditionalBehaviorEnum"
