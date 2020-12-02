{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectState where

import Network.AWS.Prelude

-- | Synchronization state for a project.
data ProjectState
  = Importing
  | Normal
  | Syncing
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

instance FromText ProjectState where
  parser =
    takeLowerText >>= \case
      "importing" -> pure Importing
      "normal" -> pure Normal
      "syncing" -> pure Syncing
      e ->
        fromTextError $
          "Failure parsing ProjectState from value: '" <> e
            <> "'. Accepted values: importing, normal, syncing"

instance ToText ProjectState where
  toText = \case
    Importing -> "IMPORTING"
    Normal -> "NORMAL"
    Syncing -> "SYNCING"

instance Hashable ProjectState

instance NFData ProjectState

instance ToByteString ProjectState

instance ToQuery ProjectState

instance ToHeader ProjectState

instance FromJSON ProjectState where
  parseJSON = parseJSONText "ProjectState"
