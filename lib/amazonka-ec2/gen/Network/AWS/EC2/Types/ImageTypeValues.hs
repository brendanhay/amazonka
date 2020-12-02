{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageTypeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageTypeValues where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ImageTypeValues
  = ITVKernel
  | ITVMachine
  | ITVRAMDisk
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

instance FromText ImageTypeValues where
  parser =
    takeLowerText >>= \case
      "kernel" -> pure ITVKernel
      "machine" -> pure ITVMachine
      "ramdisk" -> pure ITVRAMDisk
      e ->
        fromTextError $
          "Failure parsing ImageTypeValues from value: '" <> e
            <> "'. Accepted values: kernel, machine, ramdisk"

instance ToText ImageTypeValues where
  toText = \case
    ITVKernel -> "kernel"
    ITVMachine -> "machine"
    ITVRAMDisk -> "ramdisk"

instance Hashable ImageTypeValues

instance NFData ImageTypeValues

instance ToByteString ImageTypeValues

instance ToQuery ImageTypeValues

instance ToHeader ImageTypeValues

instance FromXML ImageTypeValues where
  parseXML = parseXMLText "ImageTypeValues"
