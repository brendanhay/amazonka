{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AccessEndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AccessEndpointType where

import Network.AWS.Prelude

data AccessEndpointType = Streaming
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

instance FromText AccessEndpointType where
  parser =
    takeLowerText >>= \case
      "streaming" -> pure Streaming
      e ->
        fromTextError $
          "Failure parsing AccessEndpointType from value: '" <> e
            <> "'. Accepted values: streaming"

instance ToText AccessEndpointType where
  toText = \case
    Streaming -> "STREAMING"

instance Hashable AccessEndpointType

instance NFData AccessEndpointType

instance ToByteString AccessEndpointType

instance ToQuery AccessEndpointType

instance ToHeader AccessEndpointType

instance ToJSON AccessEndpointType where
  toJSON = toJSONText

instance FromJSON AccessEndpointType where
  parseJSON = parseJSONText "AccessEndpointType"
