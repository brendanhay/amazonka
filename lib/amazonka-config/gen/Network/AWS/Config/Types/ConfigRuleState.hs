{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleState where

import Network.AWS.Prelude

data ConfigRuleState
  = Active
  | Deleting
  | DeletingResults
  | Evaluating
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

instance FromText ConfigRuleState where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deleting" -> pure Deleting
      "deleting_results" -> pure DeletingResults
      "evaluating" -> pure Evaluating
      e ->
        fromTextError $
          "Failure parsing ConfigRuleState from value: '" <> e
            <> "'. Accepted values: active, deleting, deleting_results, evaluating"

instance ToText ConfigRuleState where
  toText = \case
    Active -> "ACTIVE"
    Deleting -> "DELETING"
    DeletingResults -> "DELETING_RESULTS"
    Evaluating -> "EVALUATING"

instance Hashable ConfigRuleState

instance NFData ConfigRuleState

instance ToByteString ConfigRuleState

instance ToQuery ConfigRuleState

instance ToHeader ConfigRuleState

instance ToJSON ConfigRuleState where
  toJSON = toJSONText

instance FromJSON ConfigRuleState where
  parseJSON = parseJSONText "ConfigRuleState"
