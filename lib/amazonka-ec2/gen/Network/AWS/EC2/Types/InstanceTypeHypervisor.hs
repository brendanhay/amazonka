{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTypeHypervisor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTypeHypervisor where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceTypeHypervisor
  = Nitro
  | Xen
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

instance FromText InstanceTypeHypervisor where
  parser =
    takeLowerText >>= \case
      "nitro" -> pure Nitro
      "xen" -> pure Xen
      e ->
        fromTextError $
          "Failure parsing InstanceTypeHypervisor from value: '" <> e
            <> "'. Accepted values: nitro, xen"

instance ToText InstanceTypeHypervisor where
  toText = \case
    Nitro -> "nitro"
    Xen -> "xen"

instance Hashable InstanceTypeHypervisor

instance NFData InstanceTypeHypervisor

instance ToByteString InstanceTypeHypervisor

instance ToQuery InstanceTypeHypervisor

instance ToHeader InstanceTypeHypervisor

instance FromXML InstanceTypeHypervisor where
  parseXML = parseXMLText "InstanceTypeHypervisor"
