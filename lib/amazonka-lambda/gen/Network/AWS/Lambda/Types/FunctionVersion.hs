{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionVersion where

import Network.AWS.Prelude

data FunctionVersion = All
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

instance FromText FunctionVersion where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      e ->
        fromTextError $
          "Failure parsing FunctionVersion from value: '" <> e
            <> "'. Accepted values: all"

instance ToText FunctionVersion where
  toText = \case
    All -> "ALL"

instance Hashable FunctionVersion

instance NFData FunctionVersion

instance ToByteString FunctionVersion

instance ToQuery FunctionVersion

instance ToHeader FunctionVersion

instance ToJSON FunctionVersion where
  toJSON = toJSONText
