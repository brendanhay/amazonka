{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.BooleanOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.BooleanOperator where

import Network.AWS.Prelude

data BooleanOperator
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

instance FromText BooleanOperator where
  parser =
    takeLowerText >>= \case
      "and" -> pure And
      "or" -> pure OR
      e ->
        fromTextError $
          "Failure parsing BooleanOperator from value: '" <> e
            <> "'. Accepted values: and, or"

instance ToText BooleanOperator where
  toText = \case
    And -> "And"
    OR -> "Or"

instance Hashable BooleanOperator

instance NFData BooleanOperator

instance ToByteString BooleanOperator

instance ToQuery BooleanOperator

instance ToHeader BooleanOperator

instance ToJSON BooleanOperator where
  toJSON = toJSONText
