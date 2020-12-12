{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MetricName
  ( MetricName
      ( MetricName',
        ActivatingGameSessions,
        ActiveGameSessions,
        ActiveInstances,
        AvailableGameSessions,
        AvailablePlayerSessions,
        CurrentPlayerSessions,
        IdleInstances,
        PercentAvailableGameSessions,
        PercentIdleInstances,
        QueueDepth,
        WaitTime
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MetricName = MetricName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ActivatingGameSessions :: MetricName
pattern ActivatingGameSessions = MetricName' "ActivatingGameSessions"

pattern ActiveGameSessions :: MetricName
pattern ActiveGameSessions = MetricName' "ActiveGameSessions"

pattern ActiveInstances :: MetricName
pattern ActiveInstances = MetricName' "ActiveInstances"

pattern AvailableGameSessions :: MetricName
pattern AvailableGameSessions = MetricName' "AvailableGameSessions"

pattern AvailablePlayerSessions :: MetricName
pattern AvailablePlayerSessions = MetricName' "AvailablePlayerSessions"

pattern CurrentPlayerSessions :: MetricName
pattern CurrentPlayerSessions = MetricName' "CurrentPlayerSessions"

pattern IdleInstances :: MetricName
pattern IdleInstances = MetricName' "IdleInstances"

pattern PercentAvailableGameSessions :: MetricName
pattern PercentAvailableGameSessions = MetricName' "PercentAvailableGameSessions"

pattern PercentIdleInstances :: MetricName
pattern PercentIdleInstances = MetricName' "PercentIdleInstances"

pattern QueueDepth :: MetricName
pattern QueueDepth = MetricName' "QueueDepth"

pattern WaitTime :: MetricName
pattern WaitTime = MetricName' "WaitTime"

{-# COMPLETE
  ActivatingGameSessions,
  ActiveGameSessions,
  ActiveInstances,
  AvailableGameSessions,
  AvailablePlayerSessions,
  CurrentPlayerSessions,
  IdleInstances,
  PercentAvailableGameSessions,
  PercentIdleInstances,
  QueueDepth,
  WaitTime,
  MetricName'
  #-}
