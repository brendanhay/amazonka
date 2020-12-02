{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerTypeEnum where

import Network.AWS.Prelude

data LoadBalancerTypeEnum
  = Application
  | Gateway
  | Network
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

instance FromText LoadBalancerTypeEnum where
  parser =
    takeLowerText >>= \case
      "application" -> pure Application
      "gateway" -> pure Gateway
      "network" -> pure Network
      e ->
        fromTextError $
          "Failure parsing LoadBalancerTypeEnum from value: '" <> e
            <> "'. Accepted values: application, gateway, network"

instance ToText LoadBalancerTypeEnum where
  toText = \case
    Application -> "application"
    Gateway -> "gateway"
    Network -> "network"

instance Hashable LoadBalancerTypeEnum

instance NFData LoadBalancerTypeEnum

instance ToByteString LoadBalancerTypeEnum

instance ToQuery LoadBalancerTypeEnum

instance ToHeader LoadBalancerTypeEnum

instance FromXML LoadBalancerTypeEnum where
  parseXML = parseXMLText "LoadBalancerTypeEnum"
