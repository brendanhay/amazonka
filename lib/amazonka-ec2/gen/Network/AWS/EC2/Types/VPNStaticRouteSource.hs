{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNStaticRouteSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNStaticRouteSource where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNStaticRouteSource = Static
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

instance FromText VPNStaticRouteSource where
  parser =
    takeLowerText >>= \case
      "static" -> pure Static
      e ->
        fromTextError $
          "Failure parsing VPNStaticRouteSource from value: '" <> e
            <> "'. Accepted values: static"

instance ToText VPNStaticRouteSource where
  toText = \case
    Static -> "Static"

instance Hashable VPNStaticRouteSource

instance NFData VPNStaticRouteSource

instance ToByteString VPNStaticRouteSource

instance ToQuery VPNStaticRouteSource

instance ToHeader VPNStaticRouteSource

instance FromXML VPNStaticRouteSource where
  parseXML = parseXMLText "VPNStaticRouteSource"
