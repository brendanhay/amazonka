{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HECEndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HECEndpointType where

import Network.AWS.Prelude

data HECEndpointType
  = Event
  | Raw
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

instance FromText HECEndpointType where
  parser =
    takeLowerText >>= \case
      "event" -> pure Event
      "raw" -> pure Raw
      e ->
        fromTextError $
          "Failure parsing HECEndpointType from value: '" <> e
            <> "'. Accepted values: event, raw"

instance ToText HECEndpointType where
  toText = \case
    Event -> "Event"
    Raw -> "Raw"

instance Hashable HECEndpointType

instance NFData HECEndpointType

instance ToByteString HECEndpointType

instance ToQuery HECEndpointType

instance ToHeader HECEndpointType

instance ToJSON HECEndpointType where
  toJSON = toJSONText

instance FromJSON HECEndpointType where
  parseJSON = parseJSONText "HECEndpointType"
