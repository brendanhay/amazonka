{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LogDriver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LogDriver where

import Network.AWS.Prelude

data LogDriver
  = LDAWSfirelens
  | LDAWSlogs
  | LDFluentd
  | LDGelf
  | LDJSONFile
  | LDJournald
  | LDSplunk
  | LDSyslog
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

instance FromText LogDriver where
  parser =
    takeLowerText >>= \case
      "awsfirelens" -> pure LDAWSfirelens
      "awslogs" -> pure LDAWSlogs
      "fluentd" -> pure LDFluentd
      "gelf" -> pure LDGelf
      "json-file" -> pure LDJSONFile
      "journald" -> pure LDJournald
      "splunk" -> pure LDSplunk
      "syslog" -> pure LDSyslog
      e ->
        fromTextError $
          "Failure parsing LogDriver from value: '" <> e
            <> "'. Accepted values: awsfirelens, awslogs, fluentd, gelf, json-file, journald, splunk, syslog"

instance ToText LogDriver where
  toText = \case
    LDAWSfirelens -> "awsfirelens"
    LDAWSlogs -> "awslogs"
    LDFluentd -> "fluentd"
    LDGelf -> "gelf"
    LDJSONFile -> "json-file"
    LDJournald -> "journald"
    LDSplunk -> "splunk"
    LDSyslog -> "syslog"

instance Hashable LogDriver

instance NFData LogDriver

instance ToByteString LogDriver

instance ToQuery LogDriver

instance ToHeader LogDriver

instance ToJSON LogDriver where
  toJSON = toJSONText

instance FromJSON LogDriver where
  parseJSON = parseJSONText "LogDriver"
