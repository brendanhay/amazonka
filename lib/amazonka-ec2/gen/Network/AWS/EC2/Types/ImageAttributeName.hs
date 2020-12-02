{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ImageAttributeName
  = BlockDeviceMapping
  | Description
  | Kernel
  | LaunchPermission
  | ProductCodes
  | RAMDisk
  | SRIOVNetSupport
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

instance FromText ImageAttributeName where
  parser =
    takeLowerText >>= \case
      "blockdevicemapping" -> pure BlockDeviceMapping
      "description" -> pure Description
      "kernel" -> pure Kernel
      "launchpermission" -> pure LaunchPermission
      "productcodes" -> pure ProductCodes
      "ramdisk" -> pure RAMDisk
      "sriovnetsupport" -> pure SRIOVNetSupport
      e ->
        fromTextError $
          "Failure parsing ImageAttributeName from value: '" <> e
            <> "'. Accepted values: blockdevicemapping, description, kernel, launchpermission, productcodes, ramdisk, sriovnetsupport"

instance ToText ImageAttributeName where
  toText = \case
    BlockDeviceMapping -> "blockDeviceMapping"
    Description -> "description"
    Kernel -> "kernel"
    LaunchPermission -> "launchPermission"
    ProductCodes -> "productCodes"
    RAMDisk -> "ramdisk"
    SRIOVNetSupport -> "sriovNetSupport"

instance Hashable ImageAttributeName

instance NFData ImageAttributeName

instance ToByteString ImageAttributeName

instance ToQuery ImageAttributeName

instance ToHeader ImageAttributeName
