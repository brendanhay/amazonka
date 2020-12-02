{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Connectivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Connectivity where

import Network.AWS.Prelude

data Connectivity
  = Connected
  | Disconnected
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

instance FromText Connectivity where
  parser =
    takeLowerText >>= \case
      "connected" -> pure Connected
      "disconnected" -> pure Disconnected
      e ->
        fromTextError $
          "Failure parsing Connectivity from value: '" <> e
            <> "'. Accepted values: connected, disconnected"

instance ToText Connectivity where
  toText = \case
    Connected -> "CONNECTED"
    Disconnected -> "DISCONNECTED"

instance Hashable Connectivity

instance NFData Connectivity

instance ToByteString Connectivity

instance ToQuery Connectivity

instance ToHeader Connectivity

instance FromJSON Connectivity where
  parseJSON = parseJSONText "Connectivity"
