{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.NetworkProfileType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.NetworkProfileType where

import Network.AWS.Prelude

data NetworkProfileType
  = Curated
  | Private
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

instance FromText NetworkProfileType where
  parser =
    takeLowerText >>= \case
      "curated" -> pure Curated
      "private" -> pure Private
      e ->
        fromTextError $
          "Failure parsing NetworkProfileType from value: '" <> e
            <> "'. Accepted values: curated, private"

instance ToText NetworkProfileType where
  toText = \case
    Curated -> "CURATED"
    Private -> "PRIVATE"

instance Hashable NetworkProfileType

instance NFData NetworkProfileType

instance ToByteString NetworkProfileType

instance ToQuery NetworkProfileType

instance ToHeader NetworkProfileType

instance ToJSON NetworkProfileType where
  toJSON = toJSONText

instance FromJSON NetworkProfileType where
  parseJSON = parseJSONText "NetworkProfileType"
