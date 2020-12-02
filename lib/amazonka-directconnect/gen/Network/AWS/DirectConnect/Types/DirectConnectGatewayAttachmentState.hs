{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState where

import Network.AWS.Prelude

data DirectConnectGatewayAttachmentState
  = Attached
  | Attaching
  | Detached
  | Detaching
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

instance FromText DirectConnectGatewayAttachmentState where
  parser =
    takeLowerText >>= \case
      "attached" -> pure Attached
      "attaching" -> pure Attaching
      "detached" -> pure Detached
      "detaching" -> pure Detaching
      e ->
        fromTextError $
          "Failure parsing DirectConnectGatewayAttachmentState from value: '" <> e
            <> "'. Accepted values: attached, attaching, detached, detaching"

instance ToText DirectConnectGatewayAttachmentState where
  toText = \case
    Attached -> "attached"
    Attaching -> "attaching"
    Detached -> "detached"
    Detaching -> "detaching"

instance Hashable DirectConnectGatewayAttachmentState

instance NFData DirectConnectGatewayAttachmentState

instance ToByteString DirectConnectGatewayAttachmentState

instance ToQuery DirectConnectGatewayAttachmentState

instance ToHeader DirectConnectGatewayAttachmentState

instance FromJSON DirectConnectGatewayAttachmentState where
  parseJSON = parseJSONText "DirectConnectGatewayAttachmentState"
