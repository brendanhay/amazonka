{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayPrefixListReferenceState
  = TGPLRSAvailable
  | TGPLRSDeleting
  | TGPLRSModifying
  | TGPLRSPending
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

instance FromText TransitGatewayPrefixListReferenceState where
  parser =
    takeLowerText >>= \case
      "available" -> pure TGPLRSAvailable
      "deleting" -> pure TGPLRSDeleting
      "modifying" -> pure TGPLRSModifying
      "pending" -> pure TGPLRSPending
      e ->
        fromTextError $
          "Failure parsing TransitGatewayPrefixListReferenceState from value: '" <> e
            <> "'. Accepted values: available, deleting, modifying, pending"

instance ToText TransitGatewayPrefixListReferenceState where
  toText = \case
    TGPLRSAvailable -> "available"
    TGPLRSDeleting -> "deleting"
    TGPLRSModifying -> "modifying"
    TGPLRSPending -> "pending"

instance Hashable TransitGatewayPrefixListReferenceState

instance NFData TransitGatewayPrefixListReferenceState

instance ToByteString TransitGatewayPrefixListReferenceState

instance ToQuery TransitGatewayPrefixListReferenceState

instance ToHeader TransitGatewayPrefixListReferenceState

instance FromXML TransitGatewayPrefixListReferenceState where
  parseXML = parseXMLText "TransitGatewayPrefixListReferenceState"
