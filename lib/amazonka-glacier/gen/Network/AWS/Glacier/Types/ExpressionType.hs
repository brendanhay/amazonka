{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ExpressionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ExpressionType where

import Network.AWS.Prelude

data ExpressionType = Sql
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

instance FromText ExpressionType where
  parser =
    takeLowerText >>= \case
      "sql" -> pure Sql
      e ->
        fromTextError $
          "Failure parsing ExpressionType from value: '" <> e
            <> "'. Accepted values: sql"

instance ToText ExpressionType where
  toText = \case
    Sql -> "SQL"

instance Hashable ExpressionType

instance NFData ExpressionType

instance ToByteString ExpressionType

instance ToQuery ExpressionType

instance ToHeader ExpressionType

instance ToJSON ExpressionType where
  toJSON = toJSONText

instance FromJSON ExpressionType where
  parseJSON = parseJSONText "ExpressionType"
