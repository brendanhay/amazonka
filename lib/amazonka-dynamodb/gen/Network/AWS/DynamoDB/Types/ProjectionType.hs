{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProjectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProjectionType where

import Network.AWS.Prelude

data ProjectionType
  = PTAll
  | PTInclude
  | PTKeysOnly
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

instance FromText ProjectionType where
  parser =
    takeLowerText >>= \case
      "all" -> pure PTAll
      "include" -> pure PTInclude
      "keys_only" -> pure PTKeysOnly
      e ->
        fromTextError $
          "Failure parsing ProjectionType from value: '" <> e
            <> "'. Accepted values: all, include, keys_only"

instance ToText ProjectionType where
  toText = \case
    PTAll -> "ALL"
    PTInclude -> "INCLUDE"
    PTKeysOnly -> "KEYS_ONLY"

instance Hashable ProjectionType

instance NFData ProjectionType

instance ToByteString ProjectionType

instance ToQuery ProjectionType

instance ToHeader ProjectionType

instance ToJSON ProjectionType where
  toJSON = toJSONText

instance FromJSON ProjectionType where
  parseJSON = parseJSONText "ProjectionType"
