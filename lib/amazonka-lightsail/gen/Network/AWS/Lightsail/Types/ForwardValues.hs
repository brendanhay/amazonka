{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ForwardValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ForwardValues where

import Network.AWS.Prelude

data ForwardValues
  = FVAll
  | FVAllowList
  | FVNone
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

instance FromText ForwardValues where
  parser =
    takeLowerText >>= \case
      "all" -> pure FVAll
      "allow-list" -> pure FVAllowList
      "none" -> pure FVNone
      e ->
        fromTextError $
          "Failure parsing ForwardValues from value: '" <> e
            <> "'. Accepted values: all, allow-list, none"

instance ToText ForwardValues where
  toText = \case
    FVAll -> "all"
    FVAllowList -> "allow-list"
    FVNone -> "none"

instance Hashable ForwardValues

instance NFData ForwardValues

instance ToByteString ForwardValues

instance ToQuery ForwardValues

instance ToHeader ForwardValues

instance ToJSON ForwardValues where
  toJSON = toJSONText

instance FromJSON ForwardValues where
  parseJSON = parseJSONText "ForwardValues"
