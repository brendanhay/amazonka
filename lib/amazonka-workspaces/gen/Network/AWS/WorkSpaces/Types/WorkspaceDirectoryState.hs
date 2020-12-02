{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState where

import Network.AWS.Prelude

data WorkspaceDirectoryState
  = Deregistered
  | Deregistering
  | Error'
  | Registered
  | Registering
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

instance FromText WorkspaceDirectoryState where
  parser =
    takeLowerText >>= \case
      "deregistered" -> pure Deregistered
      "deregistering" -> pure Deregistering
      "error" -> pure Error'
      "registered" -> pure Registered
      "registering" -> pure Registering
      e ->
        fromTextError $
          "Failure parsing WorkspaceDirectoryState from value: '" <> e
            <> "'. Accepted values: deregistered, deregistering, error, registered, registering"

instance ToText WorkspaceDirectoryState where
  toText = \case
    Deregistered -> "DEREGISTERED"
    Deregistering -> "DEREGISTERING"
    Error' -> "ERROR"
    Registered -> "REGISTERED"
    Registering -> "REGISTERING"

instance Hashable WorkspaceDirectoryState

instance NFData WorkspaceDirectoryState

instance ToByteString WorkspaceDirectoryState

instance ToQuery WorkspaceDirectoryState

instance ToHeader WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
  parseJSON = parseJSONText "WorkspaceDirectoryState"
