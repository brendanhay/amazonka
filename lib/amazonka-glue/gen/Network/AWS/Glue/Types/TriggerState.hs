{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerState where

import Network.AWS.Prelude

data TriggerState
  = TSActivated
  | TSActivating
  | TSCreated
  | TSCreating
  | TSDeactivated
  | TSDeactivating
  | TSDeleting
  | TSUpdating
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

instance FromText TriggerState where
  parser =
    takeLowerText >>= \case
      "activated" -> pure TSActivated
      "activating" -> pure TSActivating
      "created" -> pure TSCreated
      "creating" -> pure TSCreating
      "deactivated" -> pure TSDeactivated
      "deactivating" -> pure TSDeactivating
      "deleting" -> pure TSDeleting
      "updating" -> pure TSUpdating
      e ->
        fromTextError $
          "Failure parsing TriggerState from value: '" <> e
            <> "'. Accepted values: activated, activating, created, creating, deactivated, deactivating, deleting, updating"

instance ToText TriggerState where
  toText = \case
    TSActivated -> "ACTIVATED"
    TSActivating -> "ACTIVATING"
    TSCreated -> "CREATED"
    TSCreating -> "CREATING"
    TSDeactivated -> "DEACTIVATED"
    TSDeactivating -> "DEACTIVATING"
    TSDeleting -> "DELETING"
    TSUpdating -> "UPDATING"

instance Hashable TriggerState

instance NFData TriggerState

instance ToByteString TriggerState

instance ToQuery TriggerState

instance ToHeader TriggerState

instance FromJSON TriggerState where
  parseJSON = parseJSONText "TriggerState"
