{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionType where

import Network.AWS.Prelude

data ActionType
  = ApplyIAMPolicy
  | ApplyScpPolicy
  | RunSsmDocuments
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

instance FromText ActionType where
  parser =
    takeLowerText >>= \case
      "apply_iam_policy" -> pure ApplyIAMPolicy
      "apply_scp_policy" -> pure ApplyScpPolicy
      "run_ssm_documents" -> pure RunSsmDocuments
      e ->
        fromTextError $
          "Failure parsing ActionType from value: '" <> e
            <> "'. Accepted values: apply_iam_policy, apply_scp_policy, run_ssm_documents"

instance ToText ActionType where
  toText = \case
    ApplyIAMPolicy -> "APPLY_IAM_POLICY"
    ApplyScpPolicy -> "APPLY_SCP_POLICY"
    RunSsmDocuments -> "RUN_SSM_DOCUMENTS"

instance Hashable ActionType

instance NFData ActionType

instance ToByteString ActionType

instance ToQuery ActionType

instance ToHeader ActionType

instance ToJSON ActionType where
  toJSON = toJSONText

instance FromJSON ActionType where
  parseJSON = parseJSONText "ActionType"
