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
-- Module      : Amazonka.SWF.Types.DecisionTaskStartedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.DecisionTaskStartedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromJSON
    DecisionTaskStartedEventAttributes
  where
  parseJSON =
    Data.withObject
      "DecisionTaskStartedEventAttributes"
      ( \x ->
          DecisionTaskStartedEventAttributes'
            Prelude.<$> (x Data..:? "identity")
            Prelude.<*> (x Data..: "scheduledEventId")
      )

instance
  Prelude.Hashable
    DecisionTaskStartedEventAttributes
  where
  hashWithSalt
    _salt
    DecisionTaskStartedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` identity
        `Prelude.hashWithSalt` scheduledEventId

instance
  Prelude.NFData
    DecisionTaskStartedEventAttributes
  where
  rnf DecisionTaskStartedEventAttributes' {..} =
    Prelude.rnf identity `Prelude.seq`
      Prelude.rnf scheduledEventId
