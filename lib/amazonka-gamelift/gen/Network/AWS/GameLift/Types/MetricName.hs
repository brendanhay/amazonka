{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.MetricName
  ( MetricName
    ( MetricName'
    , MetricNameActivatingGameSessions
    , MetricNameActiveGameSessions
    , MetricNameActiveInstances
    , MetricNameAvailableGameSessions
    , MetricNameAvailablePlayerSessions
    , MetricNameCurrentPlayerSessions
    , MetricNameIdleInstances
    , MetricNamePercentAvailableGameSessions
    , MetricNamePercentIdleInstances
    , MetricNameQueueDepth
    , MetricNameWaitTime
    , fromMetricName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MetricName = MetricName'{fromMetricName :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern MetricNameActivatingGameSessions :: MetricName
pattern MetricNameActivatingGameSessions = MetricName' "ActivatingGameSessions"

pattern MetricNameActiveGameSessions :: MetricName
pattern MetricNameActiveGameSessions = MetricName' "ActiveGameSessions"

pattern MetricNameActiveInstances :: MetricName
pattern MetricNameActiveInstances = MetricName' "ActiveInstances"

pattern MetricNameAvailableGameSessions :: MetricName
pattern MetricNameAvailableGameSessions = MetricName' "AvailableGameSessions"

pattern MetricNameAvailablePlayerSessions :: MetricName
pattern MetricNameAvailablePlayerSessions = MetricName' "AvailablePlayerSessions"

pattern MetricNameCurrentPlayerSessions :: MetricName
pattern MetricNameCurrentPlayerSessions = MetricName' "CurrentPlayerSessions"

pattern MetricNameIdleInstances :: MetricName
pattern MetricNameIdleInstances = MetricName' "IdleInstances"

pattern MetricNamePercentAvailableGameSessions :: MetricName
pattern MetricNamePercentAvailableGameSessions = MetricName' "PercentAvailableGameSessions"

pattern MetricNamePercentIdleInstances :: MetricName
pattern MetricNamePercentIdleInstances = MetricName' "PercentIdleInstances"

pattern MetricNameQueueDepth :: MetricName
pattern MetricNameQueueDepth = MetricName' "QueueDepth"

pattern MetricNameWaitTime :: MetricName
pattern MetricNameWaitTime = MetricName' "WaitTime"

{-# COMPLETE 
  MetricNameActivatingGameSessions,

  MetricNameActiveGameSessions,

  MetricNameActiveInstances,

  MetricNameAvailableGameSessions,

  MetricNameAvailablePlayerSessions,

  MetricNameCurrentPlayerSessions,

  MetricNameIdleInstances,

  MetricNamePercentAvailableGameSessions,

  MetricNamePercentIdleInstances,

  MetricNameQueueDepth,

  MetricNameWaitTime,
  MetricName'
  #-}
