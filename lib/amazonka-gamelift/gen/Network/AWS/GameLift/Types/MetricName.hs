{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MetricName where

import Network.AWS.Prelude

data MetricName
  = ActivatingGameSessions
  | ActiveGameSessions
  | ActiveInstances
  | AvailableGameSessions
  | AvailablePlayerSessions
  | CurrentPlayerSessions
  | IdleInstances
  | PercentAvailableGameSessions
  | PercentIdleInstances
  | QueueDepth
  | WaitTime
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

instance FromText MetricName where
  parser =
    takeLowerText >>= \case
      "activatinggamesessions" -> pure ActivatingGameSessions
      "activegamesessions" -> pure ActiveGameSessions
      "activeinstances" -> pure ActiveInstances
      "availablegamesessions" -> pure AvailableGameSessions
      "availableplayersessions" -> pure AvailablePlayerSessions
      "currentplayersessions" -> pure CurrentPlayerSessions
      "idleinstances" -> pure IdleInstances
      "percentavailablegamesessions" -> pure PercentAvailableGameSessions
      "percentidleinstances" -> pure PercentIdleInstances
      "queuedepth" -> pure QueueDepth
      "waittime" -> pure WaitTime
      e ->
        fromTextError $
          "Failure parsing MetricName from value: '" <> e
            <> "'. Accepted values: activatinggamesessions, activegamesessions, activeinstances, availablegamesessions, availableplayersessions, currentplayersessions, idleinstances, percentavailablegamesessions, percentidleinstances, queuedepth, waittime"

instance ToText MetricName where
  toText = \case
    ActivatingGameSessions -> "ActivatingGameSessions"
    ActiveGameSessions -> "ActiveGameSessions"
    ActiveInstances -> "ActiveInstances"
    AvailableGameSessions -> "AvailableGameSessions"
    AvailablePlayerSessions -> "AvailablePlayerSessions"
    CurrentPlayerSessions -> "CurrentPlayerSessions"
    IdleInstances -> "IdleInstances"
    PercentAvailableGameSessions -> "PercentAvailableGameSessions"
    PercentIdleInstances -> "PercentIdleInstances"
    QueueDepth -> "QueueDepth"
    WaitTime -> "WaitTime"

instance Hashable MetricName

instance NFData MetricName

instance ToByteString MetricName

instance ToQuery MetricName

instance ToHeader MetricName

instance ToJSON MetricName where
  toJSON = toJSONText

instance FromJSON MetricName where
  parseJSON = parseJSONText "MetricName"
