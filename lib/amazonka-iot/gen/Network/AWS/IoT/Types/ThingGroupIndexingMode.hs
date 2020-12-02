{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupIndexingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupIndexingMode where

import Network.AWS.Prelude

data ThingGroupIndexingMode
  = ON
  | Off
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

instance FromText ThingGroupIndexingMode where
  parser =
    takeLowerText >>= \case
      "on" -> pure ON
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing ThingGroupIndexingMode from value: '" <> e
            <> "'. Accepted values: on, off"

instance ToText ThingGroupIndexingMode where
  toText = \case
    ON -> "ON"
    Off -> "OFF"

instance Hashable ThingGroupIndexingMode

instance NFData ThingGroupIndexingMode

instance ToByteString ThingGroupIndexingMode

instance ToQuery ThingGroupIndexingMode

instance ToHeader ThingGroupIndexingMode

instance ToJSON ThingGroupIndexingMode where
  toJSON = toJSONText

instance FromJSON ThingGroupIndexingMode where
  parseJSON = parseJSONText "ThingGroupIndexingMode"
