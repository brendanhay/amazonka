{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.BooleanEnumType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.BooleanEnumType where

import Network.AWS.Prelude

data BooleanEnumType
  = False'
  | True'
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

instance FromText BooleanEnumType where
  parser =
    takeLowerText >>= \case
      "false" -> pure False'
      "true" -> pure True'
      e ->
        fromTextError $
          "Failure parsing BooleanEnumType from value: '" <> e
            <> "'. Accepted values: false, true"

instance ToText BooleanEnumType where
  toText = \case
    False' -> "FALSE"
    True' -> "TRUE"

instance Hashable BooleanEnumType

instance NFData BooleanEnumType

instance ToByteString BooleanEnumType

instance ToQuery BooleanEnumType

instance ToHeader BooleanEnumType

instance ToJSON BooleanEnumType where
  toJSON = toJSONText
