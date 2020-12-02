{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.State
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.State where

import Network.AWS.Prelude

data State
  = Active
  | Failed
  | Inactive
  | Pending
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

instance FromText State where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "failed" -> pure Failed
      "inactive" -> pure Inactive
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing State from value: '" <> e
            <> "'. Accepted values: active, failed, inactive, pending"

instance ToText State where
  toText = \case
    Active -> "Active"
    Failed -> "Failed"
    Inactive -> "Inactive"
    Pending -> "Pending"

instance Hashable State

instance NFData State

instance ToByteString State

instance ToQuery State

instance ToHeader State

instance FromJSON State where
  parseJSON = parseJSONText "State"
