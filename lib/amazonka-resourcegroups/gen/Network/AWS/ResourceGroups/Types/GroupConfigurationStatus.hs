{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationStatus where

import Network.AWS.Prelude

data GroupConfigurationStatus
  = UpdateComplete
  | UpdateFailed
  | Updating
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

instance FromText GroupConfigurationStatus where
  parser =
    takeLowerText >>= \case
      "update_complete" -> pure UpdateComplete
      "update_failed" -> pure UpdateFailed
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing GroupConfigurationStatus from value: '" <> e
            <> "'. Accepted values: update_complete, update_failed, updating"

instance ToText GroupConfigurationStatus where
  toText = \case
    UpdateComplete -> "UPDATE_COMPLETE"
    UpdateFailed -> "UPDATE_FAILED"
    Updating -> "UPDATING"

instance Hashable GroupConfigurationStatus

instance NFData GroupConfigurationStatus

instance ToByteString GroupConfigurationStatus

instance ToQuery GroupConfigurationStatus

instance ToHeader GroupConfigurationStatus

instance FromJSON GroupConfigurationStatus where
  parseJSON = parseJSONText "GroupConfigurationStatus"
