{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MetricName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MetricName
  ( MetricName
      ( ..,
        MetricName_ActivatingGameSessions,
        MetricName_ActiveGameSessions,
        MetricName_ActiveInstances,
        MetricName_AvailableGameSessions,
        MetricName_AvailablePlayerSessions,
        MetricName_CurrentPlayerSessions,
        MetricName_IdleInstances,
        MetricName_PercentAvailableGameSessions,
        MetricName_PercentIdleInstances,
        MetricName_QueueDepth,
        MetricName_WaitTime
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MetricName = MetricName'
  { fromMetricName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern MetricName_ActivatingGameSessions :: MetricName
pattern MetricName_ActivatingGameSessions = MetricName' "ActivatingGameSessions"

pattern MetricName_ActiveGameSessions :: MetricName
pattern MetricName_ActiveGameSessions = MetricName' "ActiveGameSessions"

pattern MetricName_ActiveInstances :: MetricName
pattern MetricName_ActiveInstances = MetricName' "ActiveInstances"

pattern MetricName_AvailableGameSessions :: MetricName
pattern MetricName_AvailableGameSessions = MetricName' "AvailableGameSessions"

pattern MetricName_AvailablePlayerSessions :: MetricName
pattern MetricName_AvailablePlayerSessions = MetricName' "AvailablePlayerSessions"

pattern MetricName_CurrentPlayerSessions :: MetricName
pattern MetricName_CurrentPlayerSessions = MetricName' "CurrentPlayerSessions"

pattern MetricName_IdleInstances :: MetricName
pattern MetricName_IdleInstances = MetricName' "IdleInstances"

pattern MetricName_PercentAvailableGameSessions :: MetricName
pattern MetricName_PercentAvailableGameSessions = MetricName' "PercentAvailableGameSessions"

pattern MetricName_PercentIdleInstances :: MetricName
pattern MetricName_PercentIdleInstances = MetricName' "PercentIdleInstances"

pattern MetricName_QueueDepth :: MetricName
pattern MetricName_QueueDepth = MetricName' "QueueDepth"

pattern MetricName_WaitTime :: MetricName
pattern MetricName_WaitTime = MetricName' "WaitTime"

{-# COMPLETE
  MetricName_ActivatingGameSessions,
  MetricName_ActiveGameSessions,
  MetricName_ActiveInstances,
  MetricName_AvailableGameSessions,
  MetricName_AvailablePlayerSessions,
  MetricName_CurrentPlayerSessions,
  MetricName_IdleInstances,
  MetricName_PercentAvailableGameSessions,
  MetricName_PercentIdleInstances,
  MetricName_QueueDepth,
  MetricName_WaitTime,
  MetricName'
  #-}
