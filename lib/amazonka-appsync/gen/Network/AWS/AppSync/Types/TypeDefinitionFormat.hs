{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.TypeDefinitionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.TypeDefinitionFormat where

import Network.AWS.Prelude

data TypeDefinitionFormat
  = JSON
  | Sdl
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

instance FromText TypeDefinitionFormat where
  parser =
    takeLowerText >>= \case
      "json" -> pure JSON
      "sdl" -> pure Sdl
      e ->
        fromTextError $
          "Failure parsing TypeDefinitionFormat from value: '" <> e
            <> "'. Accepted values: json, sdl"

instance ToText TypeDefinitionFormat where
  toText = \case
    JSON -> "JSON"
    Sdl -> "SDL"

instance Hashable TypeDefinitionFormat

instance NFData TypeDefinitionFormat

instance ToByteString TypeDefinitionFormat

instance ToQuery TypeDefinitionFormat

instance ToHeader TypeDefinitionFormat

instance ToJSON TypeDefinitionFormat where
  toJSON = toJSONText

instance FromJSON TypeDefinitionFormat where
  parseJSON = parseJSONText "TypeDefinitionFormat"
