{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayState where

import Network.AWS.Prelude

data DirectConnectGatewayState
  = DCGSAvailable
  | DCGSDeleted
  | DCGSDeleting
  | DCGSPending
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

instance FromText DirectConnectGatewayState where
  parser =
    takeLowerText >>= \case
      "available" -> pure DCGSAvailable
      "deleted" -> pure DCGSDeleted
      "deleting" -> pure DCGSDeleting
      "pending" -> pure DCGSPending
      e ->
        fromTextError $
          "Failure parsing DirectConnectGatewayState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText DirectConnectGatewayState where
  toText = \case
    DCGSAvailable -> "available"
    DCGSDeleted -> "deleted"
    DCGSDeleting -> "deleting"
    DCGSPending -> "pending"

instance Hashable DirectConnectGatewayState

instance NFData DirectConnectGatewayState

instance ToByteString DirectConnectGatewayState

instance ToQuery DirectConnectGatewayState

instance ToHeader DirectConnectGatewayState

instance FromJSON DirectConnectGatewayState where
  parseJSON = parseJSONText "DirectConnectGatewayState"
