{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EventCode
  = InstanceReboot
  | InstanceRetirement
  | InstanceStop
  | SystemMaintenance
  | SystemReboot
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

instance FromText EventCode where
  parser =
    takeLowerText >>= \case
      "instance-reboot" -> pure InstanceReboot
      "instance-retirement" -> pure InstanceRetirement
      "instance-stop" -> pure InstanceStop
      "system-maintenance" -> pure SystemMaintenance
      "system-reboot" -> pure SystemReboot
      e ->
        fromTextError $
          "Failure parsing EventCode from value: '" <> e
            <> "'. Accepted values: instance-reboot, instance-retirement, instance-stop, system-maintenance, system-reboot"

instance ToText EventCode where
  toText = \case
    InstanceReboot -> "instance-reboot"
    InstanceRetirement -> "instance-retirement"
    InstanceStop -> "instance-stop"
    SystemMaintenance -> "system-maintenance"
    SystemReboot -> "system-reboot"

instance Hashable EventCode

instance NFData EventCode

instance ToByteString EventCode

instance ToQuery EventCode

instance ToHeader EventCode

instance FromXML EventCode where
  parseXML = parseXMLText "EventCode"
