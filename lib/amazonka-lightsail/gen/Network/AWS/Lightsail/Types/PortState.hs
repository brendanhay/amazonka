{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortState where

import Network.AWS.Prelude

data PortState
  = Closed
  | Open
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

instance FromText PortState where
  parser =
    takeLowerText >>= \case
      "closed" -> pure Closed
      "open" -> pure Open
      e ->
        fromTextError $
          "Failure parsing PortState from value: '" <> e
            <> "'. Accepted values: closed, open"

instance ToText PortState where
  toText = \case
    Closed -> "closed"
    Open -> "open"

instance Hashable PortState

instance NFData PortState

instance ToByteString PortState

instance ToQuery PortState

instance ToHeader PortState

instance FromJSON PortState where
  parseJSON = parseJSONText "PortState"
