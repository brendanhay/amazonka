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
-- Module      : Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @DecisionTaskCompleted@ event.
--
-- /See:/ 'newDecisionTaskCompletedEventAttributes' smart constructor.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes'
  { -- | User defined context for the workflow execution.
    executionContext :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'DecisionTaskCompletedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionContext', 'decisionTaskCompletedEventAttributes_executionContext' - User defined context for the workflow execution.
--
-- 'scheduledEventId', 'decisionTaskCompletedEventAttributes_scheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'decisionTaskCompletedEventAttributes_startedEventId' - The ID of the @DecisionTaskStarted@ event recorded when this decision
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newDecisionTaskCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  DecisionTaskCompletedEventAttributes
newDecisionTaskCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    DecisionTaskCompletedEventAttributes'
      { executionContext =
          Prelude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | User defined context for the workflow execution.
decisionTaskCompletedEventAttributes_executionContext :: Lens.Lens' DecisionTaskCompletedEventAttributes (Prelude.Maybe Prelude.Text)
decisionTaskCompletedEventAttributes_executionContext = Lens.lens (\DecisionTaskCompletedEventAttributes' {executionContext} -> executionContext) (\s@DecisionTaskCompletedEventAttributes' {} a -> s {executionContext = a} :: DecisionTaskCompletedEventAttributes)

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
decisionTaskCompletedEventAttributes_scheduledEventId :: Lens.Lens' DecisionTaskCompletedEventAttributes Prelude.Integer
decisionTaskCompletedEventAttributes_scheduledEventId = Lens.lens (\DecisionTaskCompletedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@DecisionTaskCompletedEventAttributes' {} a -> s {scheduledEventId = a} :: DecisionTaskCompletedEventAttributes)

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
decisionTaskCompletedEventAttributes_startedEventId :: Lens.Lens' DecisionTaskCompletedEventAttributes Prelude.Integer
decisionTaskCompletedEventAttributes_startedEventId = Lens.lens (\DecisionTaskCompletedEventAttributes' {startedEventId} -> startedEventId) (\s@DecisionTaskCompletedEventAttributes' {} a -> s {startedEventId = a} :: DecisionTaskCompletedEventAttributes)

instance
  Prelude.FromJSON
    DecisionTaskCompletedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "DecisionTaskCompletedEventAttributes"
      ( \x ->
          DecisionTaskCompletedEventAttributes'
            Prelude.<$> (x Prelude..:? "executionContext")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    DecisionTaskCompletedEventAttributes

instance
  Prelude.NFData
    DecisionTaskCompletedEventAttributes
