{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ActivityStreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ActivityStreamStatus where

import Network.AWS.Prelude

data ActivityStreamStatus
  = Started
  | Starting
  | Stopped
  | Stopping
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

instance FromText ActivityStreamStatus where
  parser =
    takeLowerText >>= \case
      "started" -> pure Started
      "starting" -> pure Starting
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      e ->
        fromTextError $
          "Failure parsing ActivityStreamStatus from value: '" <> e
            <> "'. Accepted values: started, starting, stopped, stopping"

instance ToText ActivityStreamStatus where
  toText = \case
    Started -> "started"
    Starting -> "starting"
    Stopped -> "stopped"
    Stopping -> "stopping"

instance Hashable ActivityStreamStatus

instance NFData ActivityStreamStatus

instance ToByteString ActivityStreamStatus

instance ToQuery ActivityStreamStatus

instance ToHeader ActivityStreamStatus

instance FromXML ActivityStreamStatus where
  parseXML = parseXMLText "ActivityStreamStatus"
