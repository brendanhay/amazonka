{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ActionStatus where

import Network.AWS.Prelude

data ActionStatus
  = ASPending
  | ASRunning
  | ASScheduled
  | ASUnknown
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

instance FromText ActionStatus where
  parser =
    takeLowerText >>= \case
      "pending" -> pure ASPending
      "running" -> pure ASRunning
      "scheduled" -> pure ASScheduled
      "unknown" -> pure ASUnknown
      e ->
        fromTextError $
          "Failure parsing ActionStatus from value: '" <> e
            <> "'. Accepted values: pending, running, scheduled, unknown"

instance ToText ActionStatus where
  toText = \case
    ASPending -> "Pending"
    ASRunning -> "Running"
    ASScheduled -> "Scheduled"
    ASUnknown -> "Unknown"

instance Hashable ActionStatus

instance NFData ActionStatus

instance ToByteString ActionStatus

instance ToQuery ActionStatus

instance ToHeader ActionStatus

instance FromXML ActionStatus where
  parseXML = parseXMLText "ActionStatus"
