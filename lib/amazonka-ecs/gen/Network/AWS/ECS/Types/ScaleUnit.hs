{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ScaleUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ScaleUnit where

import Network.AWS.Prelude

data ScaleUnit = Percent
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

instance FromText ScaleUnit where
  parser =
    takeLowerText >>= \case
      "percent" -> pure Percent
      e ->
        fromTextError $
          "Failure parsing ScaleUnit from value: '" <> e
            <> "'. Accepted values: percent"

instance ToText ScaleUnit where
  toText = \case
    Percent -> "PERCENT"

instance Hashable ScaleUnit

instance NFData ScaleUnit

instance ToByteString ScaleUnit

instance ToQuery ScaleUnit

instance ToHeader ScaleUnit

instance ToJSON ScaleUnit where
  toJSON = toJSONText

instance FromJSON ScaleUnit where
  parseJSON = parseJSONText "ScaleUnit"
