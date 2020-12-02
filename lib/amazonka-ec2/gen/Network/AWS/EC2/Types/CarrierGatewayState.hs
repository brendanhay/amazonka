{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CarrierGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGatewayState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CarrierGatewayState
  = Available
  | Deleted
  | Deleting
  | Pending
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

instance FromText CarrierGatewayState where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing CarrierGatewayState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText CarrierGatewayState where
  toText = \case
    Available -> "available"
    Deleted -> "deleted"
    Deleting -> "deleting"
    Pending -> "pending"

instance Hashable CarrierGatewayState

instance NFData CarrierGatewayState

instance ToByteString CarrierGatewayState

instance ToQuery CarrierGatewayState

instance ToHeader CarrierGatewayState

instance FromXML CarrierGatewayState where
  parseXML = parseXMLText "CarrierGatewayState"
