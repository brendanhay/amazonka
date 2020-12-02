{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Operator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Operator where

import Network.AWS.Prelude

data Operator
  = Contains
  | Equals
  | Exists
  | GreaterThan
  | GreaterThanOrEqualTo
  | IN
  | LessThan
  | LessThanOrEqualTo
  | NotEquals
  | NotExists
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

instance FromText Operator where
  parser =
    takeLowerText >>= \case
      "contains" -> pure Contains
      "equals" -> pure Equals
      "exists" -> pure Exists
      "greaterthan" -> pure GreaterThan
      "greaterthanorequalto" -> pure GreaterThanOrEqualTo
      "in" -> pure IN
      "lessthan" -> pure LessThan
      "lessthanorequalto" -> pure LessThanOrEqualTo
      "notequals" -> pure NotEquals
      "notexists" -> pure NotExists
      e ->
        fromTextError $
          "Failure parsing Operator from value: '" <> e
            <> "'. Accepted values: contains, equals, exists, greaterthan, greaterthanorequalto, in, lessthan, lessthanorequalto, notequals, notexists"

instance ToText Operator where
  toText = \case
    Contains -> "Contains"
    Equals -> "Equals"
    Exists -> "Exists"
    GreaterThan -> "GreaterThan"
    GreaterThanOrEqualTo -> "GreaterThanOrEqualTo"
    IN -> "In"
    LessThan -> "LessThan"
    LessThanOrEqualTo -> "LessThanOrEqualTo"
    NotEquals -> "NotEquals"
    NotExists -> "NotExists"

instance Hashable Operator

instance NFData Operator

instance ToByteString Operator

instance ToQuery Operator

instance ToHeader Operator

instance ToJSON Operator where
  toJSON = toJSONText
