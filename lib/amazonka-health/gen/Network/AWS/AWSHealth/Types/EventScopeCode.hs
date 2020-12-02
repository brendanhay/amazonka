{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventScopeCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventScopeCode where

import Network.AWS.Prelude

data EventScopeCode
  = AccountSpecific
  | None
  | Public
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

instance FromText EventScopeCode where
  parser =
    takeLowerText >>= \case
      "account_specific" -> pure AccountSpecific
      "none" -> pure None
      "public" -> pure Public
      e ->
        fromTextError $
          "Failure parsing EventScopeCode from value: '" <> e
            <> "'. Accepted values: account_specific, none, public"

instance ToText EventScopeCode where
  toText = \case
    AccountSpecific -> "ACCOUNT_SPECIFIC"
    None -> "NONE"
    Public -> "PUBLIC"

instance Hashable EventScopeCode

instance NFData EventScopeCode

instance ToByteString EventScopeCode

instance ToQuery EventScopeCode

instance ToHeader EventScopeCode

instance FromJSON EventScopeCode where
  parseJSON = parseJSONText "EventScopeCode"
