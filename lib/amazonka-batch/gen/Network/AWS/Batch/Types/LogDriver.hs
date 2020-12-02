{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LogDriver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LogDriver where

import Network.AWS.Prelude

data LogDriver
  = AWSlogs
  | Fluentd
  | Gelf
  | JSONFile
  | Journald
  | Splunk
  | Syslog
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
      "awslogs" -> pure AWSlogs
      "fluentd" -> pure Fluentd
      "gelf" -> pure Gelf
      "json-file" -> pure JSONFile
      "journald" -> pure Journald
      "splunk" -> pure Splunk
      "syslog" -> pure Syslog
      e ->
        fromTextError $
          "Failure parsing LogDriver from value: '" <> e
            <> "'. Accepted values: awslogs, fluentd, gelf, json-file, journald, splunk, syslog"

instance ToText LogDriver where
  toText = \case
    AWSlogs -> "awslogs"
    Fluentd -> "fluentd"
    Gelf -> "gelf"
    JSONFile -> "json-file"
    Journald -> "journald"
    Splunk -> "splunk"
    Syslog -> "syslog"

instance Hashable LogDriver

instance NFData LogDriver

instance ToByteString LogDriver

instance ToQuery LogDriver

instance ToHeader LogDriver

instance ToJSON LogDriver where
  toJSON = toJSONText

instance FromJSON LogDriver where
  parseJSON = parseJSONText "LogDriver"
