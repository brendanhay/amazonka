{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceDetailedStatus where

import Network.AWS.Prelude

data OrganizationResourceDetailedStatus
  = CreateFailed
  | CreateInProgress
  | CreateSuccessful
  | DeleteFailed
  | DeleteInProgress
  | DeleteSuccessful
  | UpdateFailed
  | UpdateInProgress
  | UpdateSuccessful
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

instance FromText OrganizationResourceDetailedStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure CreateFailed
      "create_in_progress" -> pure CreateInProgress
      "create_successful" -> pure CreateSuccessful
      "delete_failed" -> pure DeleteFailed
      "delete_in_progress" -> pure DeleteInProgress
      "delete_successful" -> pure DeleteSuccessful
      "update_failed" -> pure UpdateFailed
      "update_in_progress" -> pure UpdateInProgress
      "update_successful" -> pure UpdateSuccessful
      e ->
        fromTextError $
          "Failure parsing OrganizationResourceDetailedStatus from value: '" <> e
            <> "'. Accepted values: create_failed, create_in_progress, create_successful, delete_failed, delete_in_progress, delete_successful, update_failed, update_in_progress, update_successful"

instance ToText OrganizationResourceDetailedStatus where
  toText = \case
    CreateFailed -> "CREATE_FAILED"
    CreateInProgress -> "CREATE_IN_PROGRESS"
    CreateSuccessful -> "CREATE_SUCCESSFUL"
    DeleteFailed -> "DELETE_FAILED"
    DeleteInProgress -> "DELETE_IN_PROGRESS"
    DeleteSuccessful -> "DELETE_SUCCESSFUL"
    UpdateFailed -> "UPDATE_FAILED"
    UpdateInProgress -> "UPDATE_IN_PROGRESS"
    UpdateSuccessful -> "UPDATE_SUCCESSFUL"

instance Hashable OrganizationResourceDetailedStatus

instance NFData OrganizationResourceDetailedStatus

instance ToByteString OrganizationResourceDetailedStatus

instance ToQuery OrganizationResourceDetailedStatus

instance ToHeader OrganizationResourceDetailedStatus

instance ToJSON OrganizationResourceDetailedStatus where
  toJSON = toJSONText

instance FromJSON OrganizationResourceDetailedStatus where
  parseJSON = parseJSONText "OrganizationResourceDetailedStatus"
