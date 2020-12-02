{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceMetricName where

import Network.AWS.Prelude

data ContainerServiceMetricName
  = CSMNCPUUtilization
  | CSMNMemoryUtilization
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

instance FromText ContainerServiceMetricName where
  parser =
    takeLowerText >>= \case
      "cpuutilization" -> pure CSMNCPUUtilization
      "memoryutilization" -> pure CSMNMemoryUtilization
      e ->
        fromTextError $
          "Failure parsing ContainerServiceMetricName from value: '" <> e
            <> "'. Accepted values: cpuutilization, memoryutilization"

instance ToText ContainerServiceMetricName where
  toText = \case
    CSMNCPUUtilization -> "CPUUtilization"
    CSMNMemoryUtilization -> "MemoryUtilization"

instance Hashable ContainerServiceMetricName

instance NFData ContainerServiceMetricName

instance ToByteString ContainerServiceMetricName

instance ToQuery ContainerServiceMetricName

instance ToHeader ContainerServiceMetricName

instance ToJSON ContainerServiceMetricName where
  toJSON = toJSONText

instance FromJSON ContainerServiceMetricName where
  parseJSON = parseJSONText "ContainerServiceMetricName"
