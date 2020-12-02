{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.FilterCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.FilterCondition where

import Network.AWS.Prelude

data FilterCondition
  = Between
  | EQ'
  | IN
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

instance FromText FilterCondition where
  parser =
    takeLowerText >>= \case
      "between" -> pure Between
      "eq" -> pure EQ'
      "in" -> pure IN
      e ->
        fromTextError $
          "Failure parsing FilterCondition from value: '" <> e
            <> "'. Accepted values: between, eq, in"

instance ToText FilterCondition where
  toText = \case
    Between -> "BETWEEN"
    EQ' -> "EQ"
    IN -> "IN"

instance Hashable FilterCondition

instance NFData FilterCondition

instance ToByteString FilterCondition

instance ToQuery FilterCondition

instance ToHeader FilterCondition

instance ToJSON FilterCondition where
  toJSON = toJSONText
