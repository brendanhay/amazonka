{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.EndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.EndpointType where

import Network.AWS.Prelude

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API setup, most suitable for mobile applications; @REGIONAL@ for regional API endpoint setup, most suitable for calling from AWS Region; and @PRIVATE@ for private APIs.
data EndpointType
  = Edge
  | Private
  | Regional
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

instance FromText EndpointType where
  parser =
    takeLowerText >>= \case
      "edge" -> pure Edge
      "private" -> pure Private
      "regional" -> pure Regional
      e ->
        fromTextError $
          "Failure parsing EndpointType from value: '" <> e
            <> "'. Accepted values: edge, private, regional"

instance ToText EndpointType where
  toText = \case
    Edge -> "EDGE"
    Private -> "PRIVATE"
    Regional -> "REGIONAL"

instance Hashable EndpointType

instance NFData EndpointType

instance ToByteString EndpointType

instance ToQuery EndpointType

instance ToHeader EndpointType

instance ToJSON EndpointType where
  toJSON = toJSONText

instance FromJSON EndpointType where
  parseJSON = parseJSONText "EndpointType"
