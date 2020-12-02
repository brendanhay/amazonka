{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AlarmType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AlarmType where

import Network.AWS.Prelude

data AlarmType
  = CompositeAlarm
  | MetricAlarm
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

instance FromText AlarmType where
  parser =
    takeLowerText >>= \case
      "compositealarm" -> pure CompositeAlarm
      "metricalarm" -> pure MetricAlarm
      e ->
        fromTextError $
          "Failure parsing AlarmType from value: '" <> e
            <> "'. Accepted values: compositealarm, metricalarm"

instance ToText AlarmType where
  toText = \case
    CompositeAlarm -> "CompositeAlarm"
    MetricAlarm -> "MetricAlarm"

instance Hashable AlarmType

instance NFData AlarmType

instance ToByteString AlarmType

instance ToQuery AlarmType

instance ToHeader AlarmType

instance FromXML AlarmType where
  parseXML = parseXMLText "AlarmType"
