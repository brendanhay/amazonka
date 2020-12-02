{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayMulticastDomainState
  = TGMDSAvailable
  | TGMDSDeleted
  | TGMDSDeleting
  | TGMDSPending
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

instance FromText TransitGatewayMulticastDomainState where
  parser =
    takeLowerText >>= \case
      "available" -> pure TGMDSAvailable
      "deleted" -> pure TGMDSDeleted
      "deleting" -> pure TGMDSDeleting
      "pending" -> pure TGMDSPending
      e ->
        fromTextError $
          "Failure parsing TransitGatewayMulticastDomainState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText TransitGatewayMulticastDomainState where
  toText = \case
    TGMDSAvailable -> "available"
    TGMDSDeleted -> "deleted"
    TGMDSDeleting -> "deleting"
    TGMDSPending -> "pending"

instance Hashable TransitGatewayMulticastDomainState

instance NFData TransitGatewayMulticastDomainState

instance ToByteString TransitGatewayMulticastDomainState

instance ToQuery TransitGatewayMulticastDomainState

instance ToHeader TransitGatewayMulticastDomainState

instance FromXML TransitGatewayMulticastDomainState where
  parseXML = parseXMLText "TransitGatewayMulticastDomainState"
