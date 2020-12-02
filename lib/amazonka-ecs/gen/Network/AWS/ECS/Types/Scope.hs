{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Scope where

import Network.AWS.Prelude

data Scope
  = Shared
  | Task
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

instance FromText Scope where
  parser =
    takeLowerText >>= \case
      "shared" -> pure Shared
      "task" -> pure Task
      e ->
        fromTextError $
          "Failure parsing Scope from value: '" <> e
            <> "'. Accepted values: shared, task"

instance ToText Scope where
  toText = \case
    Shared -> "shared"
    Task -> "task"

instance Hashable Scope

instance NFData Scope

instance ToByteString Scope

instance ToQuery Scope

instance ToHeader Scope

instance ToJSON Scope where
  toJSON = toJSONText

instance FromJSON Scope where
  parseJSON = parseJSONText "Scope"
