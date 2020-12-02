{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConditionalOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConditionalOperator where

import Network.AWS.Prelude

data ConditionalOperator
  = And
  | OR
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

instance FromText ConditionalOperator where
  parser =
    takeLowerText >>= \case
      "and" -> pure And
      "or" -> pure OR
      e ->
        fromTextError $
          "Failure parsing ConditionalOperator from value: '" <> e
            <> "'. Accepted values: and, or"

instance ToText ConditionalOperator where
  toText = \case
    And -> "AND"
    OR -> "OR"

instance Hashable ConditionalOperator

instance NFData ConditionalOperator

instance ToByteString ConditionalOperator

instance ToQuery ConditionalOperator

instance ToHeader ConditionalOperator

instance ToJSON ConditionalOperator where
  toJSON = toJSONText
