{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Logical
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Logical where

import Network.AWS.Prelude

data Logical
  = And
  | Any
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

instance FromText Logical where
  parser =
    takeLowerText >>= \case
      "and" -> pure And
      "any" -> pure Any
      e ->
        fromTextError $
          "Failure parsing Logical from value: '" <> e
            <> "'. Accepted values: and, any"

instance ToText Logical where
  toText = \case
    And -> "AND"
    Any -> "ANY"

instance Hashable Logical

instance NFData Logical

instance ToByteString Logical

instance ToQuery Logical

instance ToHeader Logical

instance ToJSON Logical where
  toJSON = toJSONText

instance FromJSON Logical where
  parseJSON = parseJSONText "Logical"
