{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformType where

import Network.AWS.Prelude

data TransformType = FindMatches
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

instance FromText TransformType where
  parser =
    takeLowerText >>= \case
      "find_matches" -> pure FindMatches
      e ->
        fromTextError $
          "Failure parsing TransformType from value: '" <> e
            <> "'. Accepted values: find_matches"

instance ToText TransformType where
  toText = \case
    FindMatches -> "FIND_MATCHES"

instance Hashable TransformType

instance NFData TransformType

instance ToByteString TransformType

instance ToQuery TransformType

instance ToHeader TransformType

instance ToJSON TransformType where
  toJSON = toJSONText

instance FromJSON TransformType where
  parseJSON = parseJSONText "TransformType"
