{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.OutputType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.OutputType where

import Network.AWS.Prelude

data OutputType
  = OTJSON
  | OTSdl
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

instance FromText OutputType where
  parser =
    takeLowerText >>= \case
      "json" -> pure OTJSON
      "sdl" -> pure OTSdl
      e ->
        fromTextError $
          "Failure parsing OutputType from value: '" <> e
            <> "'. Accepted values: json, sdl"

instance ToText OutputType where
  toText = \case
    OTJSON -> "JSON"
    OTSdl -> "SDL"

instance Hashable OutputType

instance NFData OutputType

instance ToByteString OutputType

instance ToQuery OutputType

instance ToHeader OutputType

instance ToJSON OutputType where
  toJSON = toJSONText
