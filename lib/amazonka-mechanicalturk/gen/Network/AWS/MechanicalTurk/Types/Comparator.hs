{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Comparator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Comparator where

import Network.AWS.Prelude

data Comparator
  = DoesNotExist
  | EqualTo
  | Exists
  | GreaterThan
  | GreaterThanOrEqualTo
  | IN
  | LessThan
  | LessThanOrEqualTo
  | NotEqualTo
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

instance FromText Comparator where
  parser =
    takeLowerText >>= \case
      "doesnotexist" -> pure DoesNotExist
      "equalto" -> pure EqualTo
      "exists" -> pure Exists
      "greaterthan" -> pure GreaterThan
      "greaterthanorequalto" -> pure GreaterThanOrEqualTo
      "in" -> pure IN
      "lessthan" -> pure LessThan
      "lessthanorequalto" -> pure LessThanOrEqualTo
      "notequalto" -> pure NotEqualTo
      "notin" -> pure NotIn
      e ->
        fromTextError $
          "Failure parsing Comparator from value: '" <> e
            <> "'. Accepted values: doesnotexist, equalto, exists, greaterthan, greaterthanorequalto, in, lessthan, lessthanorequalto, notequalto, notin"

instance ToText Comparator where
  toText = \case
    DoesNotExist -> "DoesNotExist"
    EqualTo -> "EqualTo"
    Exists -> "Exists"
    GreaterThan -> "GreaterThan"
    GreaterThanOrEqualTo -> "GreaterThanOrEqualTo"
    IN -> "In"
    LessThan -> "LessThan"
    LessThanOrEqualTo -> "LessThanOrEqualTo"
    NotEqualTo -> "NotEqualTo"
    NotIn -> "NotIn"

instance Hashable Comparator

instance NFData Comparator

instance ToByteString Comparator

instance ToQuery Comparator

instance ToHeader Comparator

instance ToJSON Comparator where
  toJSON = toJSONText

instance FromJSON Comparator where
  parseJSON = parseJSONText "Comparator"
