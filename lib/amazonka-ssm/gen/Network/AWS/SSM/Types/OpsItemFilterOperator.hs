{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilterOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilterOperator where

import Network.AWS.Prelude

data OpsItemFilterOperator
  = OIFOContains
  | OIFOEqual
  | OIFOGreaterThan
  | OIFOLessThan
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

instance FromText OpsItemFilterOperator where
  parser =
    takeLowerText >>= \case
      "contains" -> pure OIFOContains
      "equal" -> pure OIFOEqual
      "greaterthan" -> pure OIFOGreaterThan
      "lessthan" -> pure OIFOLessThan
      e ->
        fromTextError $
          "Failure parsing OpsItemFilterOperator from value: '" <> e
            <> "'. Accepted values: contains, equal, greaterthan, lessthan"

instance ToText OpsItemFilterOperator where
  toText = \case
    OIFOContains -> "Contains"
    OIFOEqual -> "Equal"
    OIFOGreaterThan -> "GreaterThan"
    OIFOLessThan -> "LessThan"

instance Hashable OpsItemFilterOperator

instance NFData OpsItemFilterOperator

instance ToByteString OpsItemFilterOperator

instance ToQuery OpsItemFilterOperator

instance ToHeader OpsItemFilterOperator

instance ToJSON OpsItemFilterOperator where
  toJSON = toJSONText
