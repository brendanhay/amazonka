{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExistCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExistCondition where

import Network.AWS.Prelude

data ExistCondition
  = MustExist
  | None
  | NotExist
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

instance FromText ExistCondition where
  parser =
    takeLowerText >>= \case
      "must_exist" -> pure MustExist
      "none" -> pure None
      "not_exist" -> pure NotExist
      e ->
        fromTextError $
          "Failure parsing ExistCondition from value: '" <> e
            <> "'. Accepted values: must_exist, none, not_exist"

instance ToText ExistCondition where
  toText = \case
    MustExist -> "MUST_EXIST"
    None -> "NONE"
    NotExist -> "NOT_EXIST"

instance Hashable ExistCondition

instance NFData ExistCondition

instance ToByteString ExistCondition

instance ToQuery ExistCondition

instance ToHeader ExistCondition

instance ToJSON ExistCondition where
  toJSON = toJSONText
