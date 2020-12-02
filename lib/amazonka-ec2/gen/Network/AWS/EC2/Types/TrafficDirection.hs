{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficDirection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficDirection where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficDirection
  = Egress
  | Ingress
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

instance FromText TrafficDirection where
  parser =
    takeLowerText >>= \case
      "egress" -> pure Egress
      "ingress" -> pure Ingress
      e ->
        fromTextError $
          "Failure parsing TrafficDirection from value: '" <> e
            <> "'. Accepted values: egress, ingress"

instance ToText TrafficDirection where
  toText = \case
    Egress -> "egress"
    Ingress -> "ingress"

instance Hashable TrafficDirection

instance NFData TrafficDirection

instance ToByteString TrafficDirection

instance ToQuery TrafficDirection

instance ToHeader TrafficDirection

instance FromXML TrafficDirection where
  parseXML = parseXMLText "TrafficDirection"
