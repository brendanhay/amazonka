{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeviceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DeviceType
  = DTEBS
  | DTInstanceStore
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

instance FromText DeviceType where
  parser =
    takeLowerText >>= \case
      "ebs" -> pure DTEBS
      "instance-store" -> pure DTInstanceStore
      e ->
        fromTextError $
          "Failure parsing DeviceType from value: '" <> e
            <> "'. Accepted values: ebs, instance-store"

instance ToText DeviceType where
  toText = \case
    DTEBS -> "ebs"
    DTInstanceStore -> "instance-store"

instance Hashable DeviceType

instance NFData DeviceType

instance ToByteString DeviceType

instance ToQuery DeviceType

instance ToHeader DeviceType

instance FromXML DeviceType where
  parseXML = parseXMLText "DeviceType"
