{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAssociationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayAssociationState
  = TGASAssociated
  | TGASAssociating
  | TGASDisassociated
  | TGASDisassociating
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

instance FromText TransitGatewayAssociationState where
  parser =
    takeLowerText >>= \case
      "associated" -> pure TGASAssociated
      "associating" -> pure TGASAssociating
      "disassociated" -> pure TGASDisassociated
      "disassociating" -> pure TGASDisassociating
      e ->
        fromTextError $
          "Failure parsing TransitGatewayAssociationState from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText TransitGatewayAssociationState where
  toText = \case
    TGASAssociated -> "associated"
    TGASAssociating -> "associating"
    TGASDisassociated -> "disassociated"
    TGASDisassociating -> "disassociating"

instance Hashable TransitGatewayAssociationState

instance NFData TransitGatewayAssociationState

instance ToByteString TransitGatewayAssociationState

instance ToQuery TransitGatewayAssociationState

instance ToHeader TransitGatewayAssociationState

instance FromXML TransitGatewayAssociationState where
  parseXML = parseXMLText "TransitGatewayAssociationState"
