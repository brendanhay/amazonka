{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType where

import Network.AWS.Prelude

data DirectConnectGatewayAttachmentType
  = PrivateVirtualInterface
  | TransitVirtualInterface
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

instance FromText DirectConnectGatewayAttachmentType where
  parser =
    takeLowerText >>= \case
      "privatevirtualinterface" -> pure PrivateVirtualInterface
      "transitvirtualinterface" -> pure TransitVirtualInterface
      e ->
        fromTextError $
          "Failure parsing DirectConnectGatewayAttachmentType from value: '" <> e
            <> "'. Accepted values: privatevirtualinterface, transitvirtualinterface"

instance ToText DirectConnectGatewayAttachmentType where
  toText = \case
    PrivateVirtualInterface -> "PrivateVirtualInterface"
    TransitVirtualInterface -> "TransitVirtualInterface"

instance Hashable DirectConnectGatewayAttachmentType

instance NFData DirectConnectGatewayAttachmentType

instance ToByteString DirectConnectGatewayAttachmentType

instance ToQuery DirectConnectGatewayAttachmentType

instance ToHeader DirectConnectGatewayAttachmentType

instance FromJSON DirectConnectGatewayAttachmentType where
  parseJSON = parseJSONText "DirectConnectGatewayAttachmentType"
