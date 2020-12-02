{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowballCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.SnowballCapacity where

import Network.AWS.Prelude

data SnowballCapacity
  = NoPreference
  | T100
  | T42
  | T50
  | T8
  | T80
  | T98
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

instance FromText SnowballCapacity where
  parser =
    takeLowerText >>= \case
      "nopreference" -> pure NoPreference
      "t100" -> pure T100
      "t42" -> pure T42
      "t50" -> pure T50
      "t8" -> pure T8
      "t80" -> pure T80
      "t98" -> pure T98
      e ->
        fromTextError $
          "Failure parsing SnowballCapacity from value: '" <> e
            <> "'. Accepted values: nopreference, t100, t42, t50, t8, t80, t98"

instance ToText SnowballCapacity where
  toText = \case
    NoPreference -> "NoPreference"
    T100 -> "T100"
    T42 -> "T42"
    T50 -> "T50"
    T8 -> "T8"
    T80 -> "T80"
    T98 -> "T98"

instance Hashable SnowballCapacity

instance NFData SnowballCapacity

instance ToByteString SnowballCapacity

instance ToQuery SnowballCapacity

instance ToHeader SnowballCapacity

instance ToJSON SnowballCapacity where
  toJSON = toJSONText

instance FromJSON SnowballCapacity where
  parseJSON = parseJSONText "SnowballCapacity"
