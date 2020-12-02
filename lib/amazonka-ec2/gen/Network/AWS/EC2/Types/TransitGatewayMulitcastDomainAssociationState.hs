{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayMulitcastDomainAssociationState
  = Associated
  | Associating
  | Disassociated
  | Disassociating
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

instance FromText TransitGatewayMulitcastDomainAssociationState where
  parser =
    takeLowerText >>= \case
      "associated" -> pure Associated
      "associating" -> pure Associating
      "disassociated" -> pure Disassociated
      "disassociating" -> pure Disassociating
      e ->
        fromTextError $
          "Failure parsing TransitGatewayMulitcastDomainAssociationState from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText TransitGatewayMulitcastDomainAssociationState where
  toText = \case
    Associated -> "associated"
    Associating -> "associating"
    Disassociated -> "disassociated"
    Disassociating -> "disassociating"

instance Hashable TransitGatewayMulitcastDomainAssociationState

instance NFData TransitGatewayMulitcastDomainAssociationState

instance ToByteString TransitGatewayMulitcastDomainAssociationState

instance ToQuery TransitGatewayMulitcastDomainAssociationState

instance ToHeader TransitGatewayMulitcastDomainAssociationState

instance FromXML TransitGatewayMulitcastDomainAssociationState where
  parseXML = parseXMLText "TransitGatewayMulitcastDomainAssociationState"
