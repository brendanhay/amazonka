{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPropagationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayPropagationState
  = Disabled
  | Disabling
  | Enabled
  | Enabling
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

instance FromText TransitGatewayPropagationState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "disabling" -> pure Disabling
      "enabled" -> pure Enabled
      "enabling" -> pure Enabling
      e ->
        fromTextError $
          "Failure parsing TransitGatewayPropagationState from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText TransitGatewayPropagationState where
  toText = \case
    Disabled -> "disabled"
    Disabling -> "disabling"
    Enabled -> "enabled"
    Enabling -> "enabling"

instance Hashable TransitGatewayPropagationState

instance NFData TransitGatewayPropagationState

instance ToByteString TransitGatewayPropagationState

instance ToQuery TransitGatewayPropagationState

instance ToHeader TransitGatewayPropagationState

instance FromXML TransitGatewayPropagationState where
  parseXML = parseXMLText "TransitGatewayPropagationState"
