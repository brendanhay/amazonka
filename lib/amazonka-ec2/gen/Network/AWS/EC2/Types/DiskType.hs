{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DiskType
  = Hdd
  | Ssd
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

instance FromText DiskType where
  parser =
    takeLowerText >>= \case
      "hdd" -> pure Hdd
      "ssd" -> pure Ssd
      e ->
        fromTextError $
          "Failure parsing DiskType from value: '" <> e
            <> "'. Accepted values: hdd, ssd"

instance ToText DiskType where
  toText = \case
    Hdd -> "hdd"
    Ssd -> "ssd"

instance Hashable DiskType

instance NFData DiskType

instance ToByteString DiskType

instance ToQuery DiskType

instance ToHeader DiskType

instance FromXML DiskType where
  parseXML = parseXMLText "DiskType"
