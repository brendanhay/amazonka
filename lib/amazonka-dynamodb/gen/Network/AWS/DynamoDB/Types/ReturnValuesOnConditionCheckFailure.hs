{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure where

import Network.AWS.Prelude

data ReturnValuesOnConditionCheckFailure
  = AllOld
  | None
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

instance FromText ReturnValuesOnConditionCheckFailure where
  parser =
    takeLowerText >>= \case
      "all_old" -> pure AllOld
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing ReturnValuesOnConditionCheckFailure from value: '" <> e
            <> "'. Accepted values: all_old, none"

instance ToText ReturnValuesOnConditionCheckFailure where
  toText = \case
    AllOld -> "ALL_OLD"
    None -> "NONE"

instance Hashable ReturnValuesOnConditionCheckFailure

instance NFData ReturnValuesOnConditionCheckFailure

instance ToByteString ReturnValuesOnConditionCheckFailure

instance ToQuery ReturnValuesOnConditionCheckFailure

instance ToHeader ReturnValuesOnConditionCheckFailure

instance ToJSON ReturnValuesOnConditionCheckFailure where
  toJSON = toJSONText
