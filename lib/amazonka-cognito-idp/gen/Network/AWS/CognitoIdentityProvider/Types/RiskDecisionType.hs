{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType where

import Network.AWS.Prelude

data RiskDecisionType
  = AccountTakeover
  | Block
  | NoRisk
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

instance FromText RiskDecisionType where
  parser =
    takeLowerText >>= \case
      "accounttakeover" -> pure AccountTakeover
      "block" -> pure Block
      "norisk" -> pure NoRisk
      e ->
        fromTextError $
          "Failure parsing RiskDecisionType from value: '" <> e
            <> "'. Accepted values: accounttakeover, block, norisk"

instance ToText RiskDecisionType where
  toText = \case
    AccountTakeover -> "AccountTakeover"
    Block -> "Block"
    NoRisk -> "NoRisk"

instance Hashable RiskDecisionType

instance NFData RiskDecisionType

instance ToByteString RiskDecisionType

instance ToQuery RiskDecisionType

instance ToHeader RiskDecisionType

instance FromJSON RiskDecisionType where
  parseJSON = parseJSONText "RiskDecisionType"
