{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataStatus where

import Network.AWS.Prelude

data ParallelDataStatus
  = Active
  | Creating
  | Deleting
  | Failed
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

instance FromText ParallelDataStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing ParallelDataStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, failed, updating"

instance ToText ParallelDataStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    Deleting -> "DELETING"
    Failed -> "FAILED"
    Updating -> "UPDATING"

instance Hashable ParallelDataStatus

instance NFData ParallelDataStatus

instance ToByteString ParallelDataStatus

instance ToQuery ParallelDataStatus

instance ToHeader ParallelDataStatus

instance FromJSON ParallelDataStatus where
  parseJSON = parseJSONText "ParallelDataStatus"
