{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FlowLogsResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FlowLogsResourceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FlowLogsResourceType
  = FLRTNetworkInterface
  | FLRTSubnet
  | FLRTVPC
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

instance FromText FlowLogsResourceType where
  parser =
    takeLowerText >>= \case
      "networkinterface" -> pure FLRTNetworkInterface
      "subnet" -> pure FLRTSubnet
      "vpc" -> pure FLRTVPC
      e ->
        fromTextError $
          "Failure parsing FlowLogsResourceType from value: '" <> e
            <> "'. Accepted values: networkinterface, subnet, vpc"

instance ToText FlowLogsResourceType where
  toText = \case
    FLRTNetworkInterface -> "NetworkInterface"
    FLRTSubnet -> "Subnet"
    FLRTVPC -> "VPC"

instance Hashable FlowLogsResourceType

instance NFData FlowLogsResourceType

instance ToByteString FlowLogsResourceType

instance ToQuery FlowLogsResourceType

instance ToHeader FlowLogsResourceType
