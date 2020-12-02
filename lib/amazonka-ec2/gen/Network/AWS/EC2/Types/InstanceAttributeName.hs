{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceAttributeName
  = IANBlockDeviceMapping
  | IANDisableAPITermination
  | IANEBSOptimized
  | IANEnaSupport
  | IANEnclaveOptions
  | IANGroupSet
  | IANInstanceInitiatedShutdownBehavior
  | IANInstanceType
  | IANKernel
  | IANProductCodes
  | IANRAMDisk
  | IANRootDeviceName
  | IANSRIOVNetSupport
  | IANSourceDestCheck
  | IANUserData
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

instance FromText InstanceAttributeName where
  parser =
    takeLowerText >>= \case
      "blockdevicemapping" -> pure IANBlockDeviceMapping
      "disableapitermination" -> pure IANDisableAPITermination
      "ebsoptimized" -> pure IANEBSOptimized
      "enasupport" -> pure IANEnaSupport
      "enclaveoptions" -> pure IANEnclaveOptions
      "groupset" -> pure IANGroupSet
      "instanceinitiatedshutdownbehavior" -> pure IANInstanceInitiatedShutdownBehavior
      "instancetype" -> pure IANInstanceType
      "kernel" -> pure IANKernel
      "productcodes" -> pure IANProductCodes
      "ramdisk" -> pure IANRAMDisk
      "rootdevicename" -> pure IANRootDeviceName
      "sriovnetsupport" -> pure IANSRIOVNetSupport
      "sourcedestcheck" -> pure IANSourceDestCheck
      "userdata" -> pure IANUserData
      e ->
        fromTextError $
          "Failure parsing InstanceAttributeName from value: '" <> e
            <> "'. Accepted values: blockdevicemapping, disableapitermination, ebsoptimized, enasupport, enclaveoptions, groupset, instanceinitiatedshutdownbehavior, instancetype, kernel, productcodes, ramdisk, rootdevicename, sriovnetsupport, sourcedestcheck, userdata"

instance ToText InstanceAttributeName where
  toText = \case
    IANBlockDeviceMapping -> "blockDeviceMapping"
    IANDisableAPITermination -> "disableApiTermination"
    IANEBSOptimized -> "ebsOptimized"
    IANEnaSupport -> "enaSupport"
    IANEnclaveOptions -> "enclaveOptions"
    IANGroupSet -> "groupSet"
    IANInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
    IANInstanceType -> "instanceType"
    IANKernel -> "kernel"
    IANProductCodes -> "productCodes"
    IANRAMDisk -> "ramdisk"
    IANRootDeviceName -> "rootDeviceName"
    IANSRIOVNetSupport -> "sriovNetSupport"
    IANSourceDestCheck -> "sourceDestCheck"
    IANUserData -> "userData"

instance Hashable InstanceAttributeName

instance NFData InstanceAttributeName

instance ToByteString InstanceAttributeName

instance ToQuery InstanceAttributeName

instance ToHeader InstanceAttributeName
