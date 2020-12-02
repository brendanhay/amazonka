{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.HostEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.HostEnvironment where

import Network.AWS.Prelude

data HostEnvironment
  = EC2
  | HyperV
  | Kvm
  | Other
  | VMware
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

instance FromText HostEnvironment where
  parser =
    takeLowerText >>= \case
      "ec2" -> pure EC2
      "hyper-v" -> pure HyperV
      "kvm" -> pure Kvm
      "other" -> pure Other
      "vmware" -> pure VMware
      e ->
        fromTextError $
          "Failure parsing HostEnvironment from value: '" <> e
            <> "'. Accepted values: ec2, hyper-v, kvm, other, vmware"

instance ToText HostEnvironment where
  toText = \case
    EC2 -> "EC2"
    HyperV -> "HYPER-V"
    Kvm -> "KVM"
    Other -> "OTHER"
    VMware -> "VMWARE"

instance Hashable HostEnvironment

instance NFData HostEnvironment

instance ToByteString HostEnvironment

instance ToQuery HostEnvironment

instance ToHeader HostEnvironment

instance FromJSON HostEnvironment where
  parseJSON = parseJSONText "HostEnvironment"
