{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeStatusName
  = IOEnabled
  | IOPerformance
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

instance FromText VolumeStatusName where
  parser =
    takeLowerText >>= \case
      "io-enabled" -> pure IOEnabled
      "io-performance" -> pure IOPerformance
      e ->
        fromTextError $
          "Failure parsing VolumeStatusName from value: '" <> e
            <> "'. Accepted values: io-enabled, io-performance"

instance ToText VolumeStatusName where
  toText = \case
    IOEnabled -> "io-enabled"
    IOPerformance -> "io-performance"

instance Hashable VolumeStatusName

instance NFData VolumeStatusName

instance ToByteString VolumeStatusName

instance ToQuery VolumeStatusName

instance ToHeader VolumeStatusName

instance FromXML VolumeStatusName where
  parseXML = parseXMLText "VolumeStatusName"
