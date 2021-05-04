{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.DecisionTaskTimeoutType

-- | Provides the details of the @DecisionTaskTimedOut@ event.
--
-- /See:/ 'newDecisionTaskTimedOutEventAttributes' smart constructor.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes'
  { -- | The type of timeout that expired before the decision task could be
    -- completed.
    timeoutType :: DecisionTaskTimeoutType,
    -- | The ID of the @DecisionTaskScheduled@ event that was recorded when this
    -- decision task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Prelude.Integer,
    -- | The ID of the @DecisionTaskStarted@ event recorded when this decision
    -- task was started. This information can be useful for diagnosing problems
    -- by tracing back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DecisionTaskTimedOutEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutType', 'decisionTaskTimedOutEventAttributes_timeoutType' - The type of timeout that expired before the decision task could be
-- completed.
--
-- 'scheduledEventId', 'decisionTaskTimedOutEventAttributes_scheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'decisionTaskTimedOutEventAttributes_startedEventId' - The ID of the @DecisionTaskStarted@ event recorded when this decision
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newDecisionTaskTimedOutEventAttributes ::
  -- | 'timeoutType'
  DecisionTaskTimeoutType ->
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  DecisionTaskTimedOutEventAttributes
newDecisionTaskTimedOutEventAttributes
  pTimeoutType_
  pScheduledEventId_
  pStartedEventId_ =
    DecisionTaskTimedOutEventAttributes'
      { timeoutType =
          pTimeoutType_,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The type of timeout that expired before the decision task could be
-- completed.
decisionTaskTimedOutEventAttributes_timeoutType :: Lens.Lens' DecisionTaskTimedOutEventAttributes DecisionTaskTimeoutType
decisionTaskTimedOutEventAttributes_timeoutType = Lens.lens (\DecisionTaskTimedOutEventAttributes' {timeoutType} -> timeoutType) (\s@DecisionTaskTimedOutEventAttributes' {} a -> s {timeoutType = a} :: DecisionTaskTimedOutEventAttributes)

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
decisionTaskTimedOutEventAttributes_scheduledEventId :: Lens.Lens' DecisionTaskTimedOutEventAttributes Prelude.Integer
decisionTaskTimedOutEventAttributes_scheduledEventId = Lens.lens (\DecisionTaskTimedOutEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@DecisionTaskTimedOutEventAttributes' {} a -> s {scheduledEventId = a} :: DecisionTaskTimedOutEventAttributes)

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
decisionTaskTimedOutEventAttributes_startedEventId :: Lens.Lens' DecisionTaskTimedOutEventAttributes Prelude.Integer
decisionTaskTimedOutEventAttributes_startedEventId = Lens.lens (\DecisionTaskTimedOutEventAttributes' {startedEventId} -> startedEventId) (\s@DecisionTaskTimedOutEventAttributes' {} a -> s {startedEventId = a} :: DecisionTaskTimedOutEventAttributes)

instance
  Prelude.FromJSON
    DecisionTaskTimedOutEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "DecisionTaskTimedOutEventAttributes"
      ( \x ->
          DecisionTaskTimedOutEventAttributes'
            Prelude.<$> (x Prelude..: "timeoutType")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    DecisionTaskTimedOutEventAttributes

instance
  Prelude.NFData
    DecisionTaskTimedOutEventAttributes
