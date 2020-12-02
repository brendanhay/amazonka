{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageFormat where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DiskImageFormat
  = Raw
  | VHD
  | VMDK
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

instance FromText DiskImageFormat where
  parser =
    takeLowerText >>= \case
      "raw" -> pure Raw
      "vhd" -> pure VHD
      "vmdk" -> pure VMDK
      e ->
        fromTextError $
          "Failure parsing DiskImageFormat from value: '" <> e
            <> "'. Accepted values: raw, vhd, vmdk"

instance ToText DiskImageFormat where
  toText = \case
    Raw -> "RAW"
    VHD -> "VHD"
    VMDK -> "VMDK"

instance Hashable DiskImageFormat

instance NFData DiskImageFormat

instance ToByteString DiskImageFormat

instance ToQuery DiskImageFormat

instance ToHeader DiskImageFormat

instance FromXML DiskImageFormat where
  parseXML = parseXMLText "DiskImageFormat"
