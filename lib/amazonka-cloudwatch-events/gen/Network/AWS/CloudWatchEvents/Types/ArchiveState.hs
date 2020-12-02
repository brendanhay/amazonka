{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ArchiveState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ArchiveState where

import Network.AWS.Prelude

data ArchiveState
  = ASCreateFailed
  | ASCreating
  | ASDisabled
  | ASEnabled
  | ASUpdateFailed
  | ASUpdating
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

instance FromText ArchiveState where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure ASCreateFailed
      "creating" -> pure ASCreating
      "disabled" -> pure ASDisabled
      "enabled" -> pure ASEnabled
      "update_failed" -> pure ASUpdateFailed
      "updating" -> pure ASUpdating
      e ->
        fromTextError $
          "Failure parsing ArchiveState from value: '" <> e
            <> "'. Accepted values: create_failed, creating, disabled, enabled, update_failed, updating"

instance ToText ArchiveState where
  toText = \case
    ASCreateFailed -> "CREATE_FAILED"
    ASCreating -> "CREATING"
    ASDisabled -> "DISABLED"
    ASEnabled -> "ENABLED"
    ASUpdateFailed -> "UPDATE_FAILED"
    ASUpdating -> "UPDATING"

instance Hashable ArchiveState

instance NFData ArchiveState

instance ToByteString ArchiveState

instance ToQuery ArchiveState

instance ToHeader ArchiveState

instance ToJSON ArchiveState where
  toJSON = toJSONText

instance FromJSON ArchiveState where
  parseJSON = parseJSONText "ArchiveState"
