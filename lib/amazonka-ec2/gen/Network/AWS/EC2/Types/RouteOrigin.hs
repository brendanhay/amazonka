{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteOrigin where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RouteOrigin
  = CreateRoute
  | CreateRouteTable
  | EnableVGWRoutePropagation
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

instance FromText RouteOrigin where
  parser =
    takeLowerText >>= \case
      "createroute" -> pure CreateRoute
      "createroutetable" -> pure CreateRouteTable
      "enablevgwroutepropagation" -> pure EnableVGWRoutePropagation
      e ->
        fromTextError $
          "Failure parsing RouteOrigin from value: '" <> e
            <> "'. Accepted values: createroute, createroutetable, enablevgwroutepropagation"

instance ToText RouteOrigin where
  toText = \case
    CreateRoute -> "CreateRoute"
    CreateRouteTable -> "CreateRouteTable"
    EnableVGWRoutePropagation -> "EnableVgwRoutePropagation"

instance Hashable RouteOrigin

instance NFData RouteOrigin

instance ToByteString RouteOrigin

instance ToQuery RouteOrigin

instance ToHeader RouteOrigin

instance FromXML RouteOrigin where
  parseXML = parseXMLText "RouteOrigin"
