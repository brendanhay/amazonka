{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RuleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RuleAction where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RuleAction
  = Allow
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

instance FromText RuleAction where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing RuleAction from value: '" <> e
            <> "'. Accepted values: allow, deny"

instance ToText RuleAction where
  toText = \case
    Allow -> "allow"
    Deny -> "deny"

instance Hashable RuleAction

instance NFData RuleAction

instance ToByteString RuleAction

instance ToQuery RuleAction

instance ToHeader RuleAction

instance FromXML RuleAction where
  parseXML = parseXMLText "RuleAction"
