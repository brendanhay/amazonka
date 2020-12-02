{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImageState where

import Network.AWS.Prelude

data WorkspaceImageState
  = WISAvailable
  | WISError'
  | WISPending
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

instance FromText WorkspaceImageState where
  parser =
    takeLowerText >>= \case
      "available" -> pure WISAvailable
      "error" -> pure WISError'
      "pending" -> pure WISPending
      e ->
        fromTextError $
          "Failure parsing WorkspaceImageState from value: '" <> e
            <> "'. Accepted values: available, error, pending"

instance ToText WorkspaceImageState where
  toText = \case
    WISAvailable -> "AVAILABLE"
    WISError' -> "ERROR"
    WISPending -> "PENDING"

instance Hashable WorkspaceImageState

instance NFData WorkspaceImageState

instance ToByteString WorkspaceImageState

instance ToQuery WorkspaceImageState

instance ToHeader WorkspaceImageState

instance FromJSON WorkspaceImageState where
  parseJSON = parseJSONText "WorkspaceImageState"
