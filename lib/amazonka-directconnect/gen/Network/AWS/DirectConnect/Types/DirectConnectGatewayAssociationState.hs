{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState where

import Network.AWS.Prelude

data DirectConnectGatewayAssociationState
  = Associated
  | Associating
  | Disassociated
  | Disassociating
  | Updating
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

instance FromText DirectConnectGatewayAssociationState where
  parser =
    takeLowerText >>= \case
      "associated" -> pure Associated
      "associating" -> pure Associating
      "disassociated" -> pure Disassociated
      "disassociating" -> pure Disassociating
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing DirectConnectGatewayAssociationState from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating, updating"

instance ToText DirectConnectGatewayAssociationState where
  toText = \case
    Associated -> "associated"
    Associating -> "associating"
    Disassociated -> "disassociated"
    Disassociating -> "disassociating"
    Updating -> "updating"

instance Hashable DirectConnectGatewayAssociationState

instance NFData DirectConnectGatewayAssociationState

instance ToByteString DirectConnectGatewayAssociationState

instance ToQuery DirectConnectGatewayAssociationState

instance ToHeader DirectConnectGatewayAssociationState

instance FromJSON DirectConnectGatewayAssociationState where
  parseJSON = parseJSONText "DirectConnectGatewayAssociationState"
