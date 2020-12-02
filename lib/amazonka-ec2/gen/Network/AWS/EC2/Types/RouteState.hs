{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RouteState
  = RActive
  | RBlackhole
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

instance FromText RouteState where
  parser =
    takeLowerText >>= \case
      "active" -> pure RActive
      "blackhole" -> pure RBlackhole
      e ->
        fromTextError $
          "Failure parsing RouteState from value: '" <> e
            <> "'. Accepted values: active, blackhole"

instance ToText RouteState where
  toText = \case
    RActive -> "active"
    RBlackhole -> "blackhole"

instance Hashable RouteState

instance NFData RouteState

instance ToByteString RouteState

instance ToQuery RouteState

instance ToHeader RouteState

instance FromXML RouteState where
  parseXML = parseXMLText "RouteState"
