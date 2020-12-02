{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState where

import Network.AWS.Prelude

data InstanceMetadataEndpointState
  = Disabled
  | Enabled
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

instance FromText InstanceMetadataEndpointState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing InstanceMetadataEndpointState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText InstanceMetadataEndpointState where
  toText = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

instance Hashable InstanceMetadataEndpointState

instance NFData InstanceMetadataEndpointState

instance ToByteString InstanceMetadataEndpointState

instance ToQuery InstanceMetadataEndpointState

instance ToHeader InstanceMetadataEndpointState

instance FromXML InstanceMetadataEndpointState where
  parseXML = parseXMLText "InstanceMetadataEndpointState"
