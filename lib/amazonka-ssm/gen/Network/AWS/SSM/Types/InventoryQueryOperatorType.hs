{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryQueryOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryQueryOperatorType where

import Network.AWS.Prelude

data InventoryQueryOperatorType
  = IQOTBeginWith
  | IQOTEqual
  | IQOTExists
  | IQOTGreaterThan
  | IQOTLessThan
  | IQOTNotEqual
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

instance FromText InventoryQueryOperatorType where
  parser =
    takeLowerText >>= \case
      "beginwith" -> pure IQOTBeginWith
      "equal" -> pure IQOTEqual
      "exists" -> pure IQOTExists
      "greaterthan" -> pure IQOTGreaterThan
      "lessthan" -> pure IQOTLessThan
      "notequal" -> pure IQOTNotEqual
      e ->
        fromTextError $
          "Failure parsing InventoryQueryOperatorType from value: '" <> e
            <> "'. Accepted values: beginwith, equal, exists, greaterthan, lessthan, notequal"

instance ToText InventoryQueryOperatorType where
  toText = \case
    IQOTBeginWith -> "BeginWith"
    IQOTEqual -> "Equal"
    IQOTExists -> "Exists"
    IQOTGreaterThan -> "GreaterThan"
    IQOTLessThan -> "LessThan"
    IQOTNotEqual -> "NotEqual"

instance Hashable InventoryQueryOperatorType

instance NFData InventoryQueryOperatorType

instance ToByteString InventoryQueryOperatorType

instance ToQuery InventoryQueryOperatorType

instance ToHeader InventoryQueryOperatorType

instance ToJSON InventoryQueryOperatorType where
  toJSON = toJSONText
