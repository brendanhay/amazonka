{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributeType where

import Network.AWS.Prelude

data AttributeType
  = Exclusive
  | Inclusive
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

instance FromText AttributeType where
  parser =
    takeLowerText >>= \case
      "exclusive" -> pure Exclusive
      "inclusive" -> pure Inclusive
      e ->
        fromTextError $
          "Failure parsing AttributeType from value: '" <> e
            <> "'. Accepted values: exclusive, inclusive"

instance ToText AttributeType where
  toText = \case
    Exclusive -> "EXCLUSIVE"
    Inclusive -> "INCLUSIVE"

instance Hashable AttributeType

instance NFData AttributeType

instance ToByteString AttributeType

instance ToQuery AttributeType

instance ToHeader AttributeType

instance ToJSON AttributeType where
  toJSON = toJSONText

instance FromJSON AttributeType where
  parseJSON = parseJSONText "AttributeType"
