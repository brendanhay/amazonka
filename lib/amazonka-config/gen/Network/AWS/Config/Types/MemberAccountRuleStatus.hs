{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MemberAccountRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MemberAccountRuleStatus where

import Network.AWS.Prelude

data MemberAccountRuleStatus
  = MARSCreateFailed
  | MARSCreateInProgress
  | MARSCreateSuccessful
  | MARSDeleteFailed
  | MARSDeleteInProgress
  | MARSDeleteSuccessful
  | MARSUpdateFailed
  | MARSUpdateInProgress
  | MARSUpdateSuccessful
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

instance FromText MemberAccountRuleStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure MARSCreateFailed
      "create_in_progress" -> pure MARSCreateInProgress
      "create_successful" -> pure MARSCreateSuccessful
      "delete_failed" -> pure MARSDeleteFailed
      "delete_in_progress" -> pure MARSDeleteInProgress
      "delete_successful" -> pure MARSDeleteSuccessful
      "update_failed" -> pure MARSUpdateFailed
      "update_in_progress" -> pure MARSUpdateInProgress
      "update_successful" -> pure MARSUpdateSuccessful
      e ->
        fromTextError $
          "Failure parsing MemberAccountRuleStatus from value: '" <> e
            <> "'. Accepted values: create_failed, create_in_progress, create_successful, delete_failed, delete_in_progress, delete_successful, update_failed, update_in_progress, update_successful"

instance ToText MemberAccountRuleStatus where
  toText = \case
    MARSCreateFailed -> "CREATE_FAILED"
    MARSCreateInProgress -> "CREATE_IN_PROGRESS"
    MARSCreateSuccessful -> "CREATE_SUCCESSFUL"
    MARSDeleteFailed -> "DELETE_FAILED"
    MARSDeleteInProgress -> "DELETE_IN_PROGRESS"
    MARSDeleteSuccessful -> "DELETE_SUCCESSFUL"
    MARSUpdateFailed -> "UPDATE_FAILED"
    MARSUpdateInProgress -> "UPDATE_IN_PROGRESS"
    MARSUpdateSuccessful -> "UPDATE_SUCCESSFUL"

instance Hashable MemberAccountRuleStatus

instance NFData MemberAccountRuleStatus

instance ToByteString MemberAccountRuleStatus

instance ToQuery MemberAccountRuleStatus

instance ToHeader MemberAccountRuleStatus

instance ToJSON MemberAccountRuleStatus where
  toJSON = toJSONText

instance FromJSON MemberAccountRuleStatus where
  parseJSON = parseJSONText "MemberAccountRuleStatus"
