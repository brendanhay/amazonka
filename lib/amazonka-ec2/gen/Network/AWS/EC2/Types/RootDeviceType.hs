{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RootDeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RootDeviceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RootDeviceType
  = EBS
  | InstanceStore
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

instance FromText RootDeviceType where
  parser =
    takeLowerText >>= \case
      "ebs" -> pure EBS
      "instance-store" -> pure InstanceStore
      e ->
        fromTextError $
          "Failure parsing RootDeviceType from value: '" <> e
            <> "'. Accepted values: ebs, instance-store"

instance ToText RootDeviceType where
  toText = \case
    EBS -> "ebs"
    InstanceStore -> "instance-store"

instance Hashable RootDeviceType

instance NFData RootDeviceType

instance ToByteString RootDeviceType

instance ToQuery RootDeviceType

instance ToHeader RootDeviceType

instance FromXML RootDeviceType where
  parseXML = parseXMLText "RootDeviceType"
