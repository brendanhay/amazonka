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
-- Module      : Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @DecisionTaskStarted@ event.
--
-- /See:/ 'newDecisionTaskStartedEventAttributes' smart constructor.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes'
  { -- | Identity of the decider making the request. This enables diagnostic
    -- tracing when problems arise. The form of this identity is user defined.
    identity :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DecisionTaskScheduled@ event that was recorded when this
    -- decision task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DecisionTaskStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'decisionTaskStartedEventAttributes_identity' - Identity of the decider making the request. This enables diagnostic
-- tracing when problems arise. The form of this identity is user defined.
--
-- 'scheduledEventId', 'decisionTaskStartedEventAttributes_scheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newDecisionTaskStartedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  DecisionTaskStartedEventAttributes
newDecisionTaskStartedEventAttributes
  pScheduledEventId_ =
    DecisionTaskStartedEventAttributes'
      { identity =
          Prelude.Nothing,
        scheduledEventId = pScheduledEventId_
      }

-- | Identity of the decider making the request. This enables diagnostic
-- tracing when problems arise. The form of this identity is user defined.
decisionTaskStartedEventAttributes_identity :: Lens.Lens' DecisionTaskStartedEventAttributes (Prelude.Maybe Prelude.Text)
decisionTaskStartedEventAttributes_identity = Lens.lens (\DecisionTaskStartedEventAttributes' {identity} -> identity) (\s@DecisionTaskStartedEventAttributes' {} a -> s {identity = a} :: DecisionTaskStartedEventAttributes)

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
decisionTaskStartedEventAttributes_scheduledEventId :: Lens.Lens' DecisionTaskStartedEventAttributes Prelude.Integer
decisionTaskStartedEventAttributes_scheduledEventId = Lens.lens (\DecisionTaskStartedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@DecisionTaskStartedEventAttributes' {} a -> s {scheduledEventId = a} :: DecisionTaskStartedEventAttributes)

instance
  Prelude.FromJSON
    DecisionTaskStartedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "DecisionTaskStartedEventAttributes"
      ( \x ->
          DecisionTaskStartedEventAttributes'
            Prelude.<$> (x Prelude..:? "identity")
            Prelude.<*> (x Prelude..: "scheduledEventId")
      )

instance
  Prelude.Hashable
    DecisionTaskStartedEventAttributes

instance
  Prelude.NFData
    DecisionTaskStartedEventAttributes
