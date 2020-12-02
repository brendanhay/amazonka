{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PresetListBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PresetListBy where

import Network.AWS.Prelude

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
data PresetListBy
  = PLBCreationDate
  | PLBName
  | PLBSystem
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

instance FromText PresetListBy where
  parser =
    takeLowerText >>= \case
      "creation_date" -> pure PLBCreationDate
      "name" -> pure PLBName
      "system" -> pure PLBSystem
      e ->
        fromTextError $
          "Failure parsing PresetListBy from value: '" <> e
            <> "'. Accepted values: creation_date, name, system"

instance ToText PresetListBy where
  toText = \case
    PLBCreationDate -> "CREATION_DATE"
    PLBName -> "NAME"
    PLBSystem -> "SYSTEM"

instance Hashable PresetListBy

instance NFData PresetListBy

instance ToByteString PresetListBy

instance ToQuery PresetListBy

instance ToHeader PresetListBy

instance ToJSON PresetListBy where
  toJSON = toJSONText
