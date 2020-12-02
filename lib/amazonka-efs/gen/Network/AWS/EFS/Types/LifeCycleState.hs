{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifeCycleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifeCycleState where

import Network.AWS.Prelude

data LifeCycleState
  = Available
  | Creating
  | Deleted
  | Deleting
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

instance FromText LifeCycleState where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing LifeCycleState from value: '" <> e
            <> "'. Accepted values: available, creating, deleted, deleting, updating"

instance ToText LifeCycleState where
  toText = \case
    Available -> "available"
    Creating -> "creating"
    Deleted -> "deleted"
    Deleting -> "deleting"
    Updating -> "updating"

instance Hashable LifeCycleState

instance NFData LifeCycleState

instance ToByteString LifeCycleState

instance ToQuery LifeCycleState

instance ToHeader LifeCycleState

instance FromJSON LifeCycleState where
  parseJSON = parseJSONText "LifeCycleState"
