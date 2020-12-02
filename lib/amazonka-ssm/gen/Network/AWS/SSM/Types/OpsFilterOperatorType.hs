{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilterOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsFilterOperatorType where

import Network.AWS.Prelude

data OpsFilterOperatorType
  = BeginWith
  | Equal
  | Exists
  | GreaterThan
  | LessThan
  | NotEqual
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

instance FromText OpsFilterOperatorType where
  parser =
    takeLowerText >>= \case
      "beginwith" -> pure BeginWith
      "equal" -> pure Equal
      "exists" -> pure Exists
      "greaterthan" -> pure GreaterThan
      "lessthan" -> pure LessThan
      "notequal" -> pure NotEqual
      e ->
        fromTextError $
          "Failure parsing OpsFilterOperatorType from value: '" <> e
            <> "'. Accepted values: beginwith, equal, exists, greaterthan, lessthan, notequal"

instance ToText OpsFilterOperatorType where
  toText = \case
    BeginWith -> "BeginWith"
    Equal -> "Equal"
    Exists -> "Exists"
    GreaterThan -> "GreaterThan"
    LessThan -> "LessThan"
    NotEqual -> "NotEqual"

instance Hashable OpsFilterOperatorType

instance NFData OpsFilterOperatorType

instance ToByteString OpsFilterOperatorType

instance ToQuery OpsFilterOperatorType

instance ToHeader OpsFilterOperatorType

instance ToJSON OpsFilterOperatorType where
  toJSON = toJSONText
