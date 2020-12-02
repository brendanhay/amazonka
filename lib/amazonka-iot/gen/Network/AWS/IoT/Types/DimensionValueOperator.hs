{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DimensionValueOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DimensionValueOperator where

import Network.AWS.Prelude

data DimensionValueOperator
  = IN
  | NotIn
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

instance FromText DimensionValueOperator where
  parser =
    takeLowerText >>= \case
      "in" -> pure IN
      "not_in" -> pure NotIn
      e ->
        fromTextError $
          "Failure parsing DimensionValueOperator from value: '" <> e
            <> "'. Accepted values: in, not_in"

instance ToText DimensionValueOperator where
  toText = \case
    IN -> "IN"
    NotIn -> "NOT_IN"

instance Hashable DimensionValueOperator

instance NFData DimensionValueOperator

instance ToByteString DimensionValueOperator

instance ToQuery DimensionValueOperator

instance ToHeader DimensionValueOperator

instance ToJSON DimensionValueOperator where
  toJSON = toJSONText

instance FromJSON DimensionValueOperator where
  parseJSON = parseJSONText "DimensionValueOperator"
