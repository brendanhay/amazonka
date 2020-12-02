{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ScalarAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ScalarAttributeType where

import Network.AWS.Prelude

data ScalarAttributeType
  = B
  | N
  | S
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

instance FromText ScalarAttributeType where
  parser =
    takeLowerText >>= \case
      "b" -> pure B
      "n" -> pure N
      "s" -> pure S
      e ->
        fromTextError $
          "Failure parsing ScalarAttributeType from value: '" <> e
            <> "'. Accepted values: b, n, s"

instance ToText ScalarAttributeType where
  toText = \case
    B -> "B"
    N -> "N"
    S -> "S"

instance Hashable ScalarAttributeType

instance NFData ScalarAttributeType

instance ToByteString ScalarAttributeType

instance ToQuery ScalarAttributeType

instance ToHeader ScalarAttributeType

instance ToJSON ScalarAttributeType where
  toJSON = toJSONText

instance FromJSON ScalarAttributeType where
  parseJSON = parseJSONText "ScalarAttributeType"
