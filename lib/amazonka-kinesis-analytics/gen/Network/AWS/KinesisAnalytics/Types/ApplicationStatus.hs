{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationStatus where

import Network.AWS.Prelude

data ApplicationStatus
  = Deleting
  | Ready
  | Running
  | Starting
  | Stopping
  | Updating
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

instance FromText ApplicationStatus where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure Deleting
      "ready" -> pure Ready
      "running" -> pure Running
      "starting" -> pure Starting
      "stopping" -> pure Stopping
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing ApplicationStatus from value: '" <> e
            <> "'. Accepted values: deleting, ready, running, starting, stopping, updating"

instance ToText ApplicationStatus where
  toText = \case
    Deleting -> "DELETING"
    Ready -> "READY"
    Running -> "RUNNING"
    Starting -> "STARTING"
    Stopping -> "STOPPING"
    Updating -> "UPDATING"

instance Hashable ApplicationStatus

instance NFData ApplicationStatus

instance ToByteString ApplicationStatus

instance ToQuery ApplicationStatus

instance ToHeader ApplicationStatus

instance FromJSON ApplicationStatus where
  parseJSON = parseJSONText "ApplicationStatus"
