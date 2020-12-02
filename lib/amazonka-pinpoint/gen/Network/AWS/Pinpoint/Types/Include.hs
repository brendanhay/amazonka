{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Include
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Include where

import Network.AWS.Prelude

data Include
  = All
  | Any
  | None
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

instance FromText Include where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "any" -> pure Any
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing Include from value: '" <> e
            <> "'. Accepted values: all, any, none"

instance ToText Include where
  toText = \case
    All -> "ALL"
    Any -> "ANY"
    None -> "NONE"

instance Hashable Include

instance NFData Include

instance ToByteString Include

instance ToQuery Include

instance ToHeader Include

instance ToJSON Include where
  toJSON = toJSONText

instance FromJSON Include where
  parseJSON = parseJSONText "Include"
