{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType where

import Network.AWS.Prelude

data AccountTakeoverEventActionType
  = ATEATBlock
  | ATEATMFAIfConfigured
  | ATEATMFARequired
  | ATEATNoAction
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

instance FromText AccountTakeoverEventActionType where
  parser =
    takeLowerText >>= \case
      "block" -> pure ATEATBlock
      "mfa_if_configured" -> pure ATEATMFAIfConfigured
      "mfa_required" -> pure ATEATMFARequired
      "no_action" -> pure ATEATNoAction
      e ->
        fromTextError $
          "Failure parsing AccountTakeoverEventActionType from value: '" <> e
            <> "'. Accepted values: block, mfa_if_configured, mfa_required, no_action"

instance ToText AccountTakeoverEventActionType where
  toText = \case
    ATEATBlock -> "BLOCK"
    ATEATMFAIfConfigured -> "MFA_IF_CONFIGURED"
    ATEATMFARequired -> "MFA_REQUIRED"
    ATEATNoAction -> "NO_ACTION"

instance Hashable AccountTakeoverEventActionType

instance NFData AccountTakeoverEventActionType

instance ToByteString AccountTakeoverEventActionType

instance ToQuery AccountTakeoverEventActionType

instance ToHeader AccountTakeoverEventActionType

instance ToJSON AccountTakeoverEventActionType where
  toJSON = toJSONText

instance FromJSON AccountTakeoverEventActionType where
  parseJSON = parseJSONText "AccountTakeoverEventActionType"
