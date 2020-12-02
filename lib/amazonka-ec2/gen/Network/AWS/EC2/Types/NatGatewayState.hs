{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGatewayState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NatGatewayState
  = NGSAvailable
  | NGSDeleted
  | NGSDeleting
  | NGSFailed
  | NGSPending
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

instance FromText NatGatewayState where
  parser =
    takeLowerText >>= \case
      "available" -> pure NGSAvailable
      "deleted" -> pure NGSDeleted
      "deleting" -> pure NGSDeleting
      "failed" -> pure NGSFailed
      "pending" -> pure NGSPending
      e ->
        fromTextError $
          "Failure parsing NatGatewayState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, failed, pending"

instance ToText NatGatewayState where
  toText = \case
    NGSAvailable -> "available"
    NGSDeleted -> "deleted"
    NGSDeleting -> "deleting"
    NGSFailed -> "failed"
    NGSPending -> "pending"

instance Hashable NatGatewayState

instance NFData NatGatewayState

instance ToByteString NatGatewayState

instance ToQuery NatGatewayState

instance ToHeader NatGatewayState

instance FromXML NatGatewayState where
  parseXML = parseXMLText "NatGatewayState"
