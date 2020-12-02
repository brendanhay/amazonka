{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AccessDirection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AccessDirection where

import Network.AWS.Prelude

data AccessDirection
  = Inbound
  | Outbound
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

instance FromText AccessDirection where
  parser =
    takeLowerText >>= \case
      "inbound" -> pure Inbound
      "outbound" -> pure Outbound
      e ->
        fromTextError $
          "Failure parsing AccessDirection from value: '" <> e
            <> "'. Accepted values: inbound, outbound"

instance ToText AccessDirection where
  toText = \case
    Inbound -> "inbound"
    Outbound -> "outbound"

instance Hashable AccessDirection

instance NFData AccessDirection

instance ToByteString AccessDirection

instance ToQuery AccessDirection

instance ToHeader AccessDirection

instance FromJSON AccessDirection where
  parseJSON = parseJSONText "AccessDirection"
