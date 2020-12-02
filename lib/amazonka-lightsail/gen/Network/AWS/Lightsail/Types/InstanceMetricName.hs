{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceMetricName where

import Network.AWS.Prelude

data InstanceMetricName
  = IMNBurstCapacityPercentage
  | IMNBurstCapacityTime
  | IMNCPUUtilization
  | IMNNetworkIn
  | IMNNetworkOut
  | IMNStatusCheckFailed
  | IMNStatusCheckFailedInstance
  | IMNStatusCheckFailedSystem
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

instance FromText InstanceMetricName where
  parser =
    takeLowerText >>= \case
      "burstcapacitypercentage" -> pure IMNBurstCapacityPercentage
      "burstcapacitytime" -> pure IMNBurstCapacityTime
      "cpuutilization" -> pure IMNCPUUtilization
      "networkin" -> pure IMNNetworkIn
      "networkout" -> pure IMNNetworkOut
      "statuscheckfailed" -> pure IMNStatusCheckFailed
      "statuscheckfailed_instance" -> pure IMNStatusCheckFailedInstance
      "statuscheckfailed_system" -> pure IMNStatusCheckFailedSystem
      e ->
        fromTextError $
          "Failure parsing InstanceMetricName from value: '" <> e
            <> "'. Accepted values: burstcapacitypercentage, burstcapacitytime, cpuutilization, networkin, networkout, statuscheckfailed, statuscheckfailed_instance, statuscheckfailed_system"

instance ToText InstanceMetricName where
  toText = \case
    IMNBurstCapacityPercentage -> "BurstCapacityPercentage"
    IMNBurstCapacityTime -> "BurstCapacityTime"
    IMNCPUUtilization -> "CPUUtilization"
    IMNNetworkIn -> "NetworkIn"
    IMNNetworkOut -> "NetworkOut"
    IMNStatusCheckFailed -> "StatusCheckFailed"
    IMNStatusCheckFailedInstance -> "StatusCheckFailed_Instance"
    IMNStatusCheckFailedSystem -> "StatusCheckFailed_System"

instance Hashable InstanceMetricName

instance NFData InstanceMetricName

instance ToByteString InstanceMetricName

instance ToQuery InstanceMetricName

instance ToHeader InstanceMetricName

instance ToJSON InstanceMetricName where
  toJSON = toJSONText

instance FromJSON InstanceMetricName where
  parseJSON = parseJSONText "InstanceMetricName"
