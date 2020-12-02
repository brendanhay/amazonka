{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.DirectoryState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.DirectoryState where

import Network.AWS.Prelude

data DirectoryState
  = Deleted
  | Disabled
  | Enabled
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

instance FromText DirectoryState where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure Deleted
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing DirectoryState from value: '" <> e
            <> "'. Accepted values: deleted, disabled, enabled"

instance ToText DirectoryState where
  toText = \case
    Deleted -> "DELETED"
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable DirectoryState

instance NFData DirectoryState

instance ToByteString DirectoryState

instance ToQuery DirectoryState

instance ToHeader DirectoryState

instance ToJSON DirectoryState where
  toJSON = toJSONText

instance FromJSON DirectoryState where
  parseJSON = parseJSONText "DirectoryState"
