{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationRuleStatus where

import Network.AWS.Prelude

data OrganizationRuleStatus
  = OCreateFailed
  | OCreateInProgress
  | OCreateSuccessful
  | ODeleteFailed
  | ODeleteInProgress
  | ODeleteSuccessful
  | OUpdateFailed
  | OUpdateInProgress
  | OUpdateSuccessful
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

instance FromText OrganizationRuleStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure OCreateFailed
      "create_in_progress" -> pure OCreateInProgress
      "create_successful" -> pure OCreateSuccessful
      "delete_failed" -> pure ODeleteFailed
      "delete_in_progress" -> pure ODeleteInProgress
      "delete_successful" -> pure ODeleteSuccessful
      "update_failed" -> pure OUpdateFailed
      "update_in_progress" -> pure OUpdateInProgress
      "update_successful" -> pure OUpdateSuccessful
      e ->
        fromTextError $
          "Failure parsing OrganizationRuleStatus from value: '" <> e
            <> "'. Accepted values: create_failed, create_in_progress, create_successful, delete_failed, delete_in_progress, delete_successful, update_failed, update_in_progress, update_successful"

instance ToText OrganizationRuleStatus where
  toText = \case
    OCreateFailed -> "CREATE_FAILED"
    OCreateInProgress -> "CREATE_IN_PROGRESS"
    OCreateSuccessful -> "CREATE_SUCCESSFUL"
    ODeleteFailed -> "DELETE_FAILED"
    ODeleteInProgress -> "DELETE_IN_PROGRESS"
    ODeleteSuccessful -> "DELETE_SUCCESSFUL"
    OUpdateFailed -> "UPDATE_FAILED"
    OUpdateInProgress -> "UPDATE_IN_PROGRESS"
    OUpdateSuccessful -> "UPDATE_SUCCESSFUL"

instance Hashable OrganizationRuleStatus

instance NFData OrganizationRuleStatus

instance ToByteString OrganizationRuleStatus

instance ToQuery OrganizationRuleStatus

instance ToHeader OrganizationRuleStatus

instance FromJSON OrganizationRuleStatus where
  parseJSON = parseJSONText "OrganizationRuleStatus"
