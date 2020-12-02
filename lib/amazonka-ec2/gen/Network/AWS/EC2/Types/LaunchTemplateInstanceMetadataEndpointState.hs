{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateInstanceMetadataEndpointState
  = LTIMESDisabled
  | LTIMESEnabled
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

instance FromText LaunchTemplateInstanceMetadataEndpointState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure LTIMESDisabled
      "enabled" -> pure LTIMESEnabled
      e ->
        fromTextError $
          "Failure parsing LaunchTemplateInstanceMetadataEndpointState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText LaunchTemplateInstanceMetadataEndpointState where
  toText = \case
    LTIMESDisabled -> "disabled"
    LTIMESEnabled -> "enabled"

instance Hashable LaunchTemplateInstanceMetadataEndpointState

instance NFData LaunchTemplateInstanceMetadataEndpointState

instance ToByteString LaunchTemplateInstanceMetadataEndpointState

instance ToQuery LaunchTemplateInstanceMetadataEndpointState

instance ToHeader LaunchTemplateInstanceMetadataEndpointState

instance FromXML LaunchTemplateInstanceMetadataEndpointState where
  parseXML = parseXMLText "LaunchTemplateInstanceMetadataEndpointState"
