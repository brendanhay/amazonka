{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState where

import Network.AWS.Prelude

data DirectConnectGatewayAssociationProposalState
  = DCGAPSAccepted
  | DCGAPSDeleted
  | DCGAPSRequested
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

instance FromText DirectConnectGatewayAssociationProposalState where
  parser =
    takeLowerText >>= \case
      "accepted" -> pure DCGAPSAccepted
      "deleted" -> pure DCGAPSDeleted
      "requested" -> pure DCGAPSRequested
      e ->
        fromTextError $
          "Failure parsing DirectConnectGatewayAssociationProposalState from value: '" <> e
            <> "'. Accepted values: accepted, deleted, requested"

instance ToText DirectConnectGatewayAssociationProposalState where
  toText = \case
    DCGAPSAccepted -> "accepted"
    DCGAPSDeleted -> "deleted"
    DCGAPSRequested -> "requested"

instance Hashable DirectConnectGatewayAssociationProposalState

instance NFData DirectConnectGatewayAssociationProposalState

instance ToByteString DirectConnectGatewayAssociationProposalState

instance ToQuery DirectConnectGatewayAssociationProposalState

instance ToHeader DirectConnectGatewayAssociationProposalState

instance FromJSON DirectConnectGatewayAssociationProposalState where
  parseJSON = parseJSONText "DirectConnectGatewayAssociationProposalState"
