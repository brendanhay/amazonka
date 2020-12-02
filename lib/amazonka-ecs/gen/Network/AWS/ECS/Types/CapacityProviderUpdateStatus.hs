{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderUpdateStatus where

import Network.AWS.Prelude

data CapacityProviderUpdateStatus
  = DeleteComplete
  | DeleteFailed
  | DeleteInProgress
  | UpdateComplete
  | UpdateFailed
  | UpdateInProgress
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

instance FromText CapacityProviderUpdateStatus where
  parser =
    takeLowerText >>= \case
      "delete_complete" -> pure DeleteComplete
      "delete_failed" -> pure DeleteFailed
      "delete_in_progress" -> pure DeleteInProgress
      "update_complete" -> pure UpdateComplete
      "update_failed" -> pure UpdateFailed
      "update_in_progress" -> pure UpdateInProgress
      e ->
        fromTextError $
          "Failure parsing CapacityProviderUpdateStatus from value: '" <> e
            <> "'. Accepted values: delete_complete, delete_failed, delete_in_progress, update_complete, update_failed, update_in_progress"

instance ToText CapacityProviderUpdateStatus where
  toText = \case
    DeleteComplete -> "DELETE_COMPLETE"
    DeleteFailed -> "DELETE_FAILED"
    DeleteInProgress -> "DELETE_IN_PROGRESS"
    UpdateComplete -> "UPDATE_COMPLETE"
    UpdateFailed -> "UPDATE_FAILED"
    UpdateInProgress -> "UPDATE_IN_PROGRESS"

instance Hashable CapacityProviderUpdateStatus

instance NFData CapacityProviderUpdateStatus

instance ToByteString CapacityProviderUpdateStatus

instance ToQuery CapacityProviderUpdateStatus

instance ToHeader CapacityProviderUpdateStatus

instance FromJSON CapacityProviderUpdateStatus where
  parseJSON = parseJSONText "CapacityProviderUpdateStatus"
