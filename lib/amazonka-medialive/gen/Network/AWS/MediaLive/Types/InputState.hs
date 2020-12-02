{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputState where

import Network.AWS.Prelude

-- | Placeholder documentation for InputState
data InputState
  = Attached
  | Creating
  | Deleted
  | Deleting
  | Detached
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

instance FromText InputState where
  parser =
    takeLowerText >>= \case
      "attached" -> pure Attached
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "detached" -> pure Detached
      e ->
        fromTextError $
          "Failure parsing InputState from value: '" <> e
            <> "'. Accepted values: attached, creating, deleted, deleting, detached"

instance ToText InputState where
  toText = \case
    Attached -> "ATTACHED"
    Creating -> "CREATING"
    Deleted -> "DELETED"
    Deleting -> "DELETING"
    Detached -> "DETACHED"

instance Hashable InputState

instance NFData InputState

instance ToByteString InputState

instance ToQuery InputState

instance ToHeader InputState

instance FromJSON InputState where
  parseJSON = parseJSONText "InputState"
