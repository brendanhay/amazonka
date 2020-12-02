{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.OutputFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.OutputFormat where

import Network.AWS.Prelude

data OutputFormat
  = JSON
  | Yaml
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

instance FromText OutputFormat where
  parser =
    takeLowerText >>= \case
      "json" -> pure JSON
      "yaml" -> pure Yaml
      e ->
        fromTextError $
          "Failure parsing OutputFormat from value: '" <> e
            <> "'. Accepted values: json, yaml"

instance ToText OutputFormat where
  toText = \case
    JSON -> "JSON"
    Yaml -> "YAML"

instance Hashable OutputFormat

instance NFData OutputFormat

instance ToByteString OutputFormat

instance ToQuery OutputFormat

instance ToHeader OutputFormat

instance ToJSON OutputFormat where
  toJSON = toJSONText
