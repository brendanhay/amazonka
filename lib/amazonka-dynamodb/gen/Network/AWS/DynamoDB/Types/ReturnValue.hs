{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnValue where

import Network.AWS.Prelude

data ReturnValue
  = RVAllNew
  | RVAllOld
  | RVNone
  | RVUpdatedNew
  | RVUpdatedOld
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

instance FromText ReturnValue where
  parser =
    takeLowerText >>= \case
      "all_new" -> pure RVAllNew
      "all_old" -> pure RVAllOld
      "none" -> pure RVNone
      "updated_new" -> pure RVUpdatedNew
      "updated_old" -> pure RVUpdatedOld
      e ->
        fromTextError $
          "Failure parsing ReturnValue from value: '" <> e
            <> "'. Accepted values: all_new, all_old, none, updated_new, updated_old"

instance ToText ReturnValue where
  toText = \case
    RVAllNew -> "ALL_NEW"
    RVAllOld -> "ALL_OLD"
    RVNone -> "NONE"
    RVUpdatedNew -> "UPDATED_NEW"
    RVUpdatedOld -> "UPDATED_OLD"

instance Hashable ReturnValue

instance NFData ReturnValue

instance ToByteString ReturnValue

instance ToQuery ReturnValue

instance ToHeader ReturnValue

instance ToJSON ReturnValue where
  toJSON = toJSONText
