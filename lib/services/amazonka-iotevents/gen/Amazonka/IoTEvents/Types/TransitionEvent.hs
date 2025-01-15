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
-- Module      : Amazonka.IoTEvents.Types.TransitionEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.TransitionEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies the actions performed and the next state entered when a
-- @condition@ evaluates to TRUE.
--
-- /See:/ 'newTransitionEvent' smart constructor.
data TransitionEvent = TransitionEvent'
  { -- | The actions to be performed.
    actions :: Prelude.Maybe [Action],
    -- | The name of the transition event.
    eventName :: Prelude.Text,
    -- | Required. A Boolean expression that when TRUE causes the actions to be
    -- performed and the @nextState@ to be entered.
    condition :: Prelude.Text,
    -- | The next state to enter.
    nextState :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitionEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'transitionEvent_actions' - The actions to be performed.
--
-- 'eventName', 'transitionEvent_eventName' - The name of the transition event.
--
-- 'condition', 'transitionEvent_condition' - Required. A Boolean expression that when TRUE causes the actions to be
-- performed and the @nextState@ to be entered.
--
-- 'nextState', 'transitionEvent_nextState' - The next state to enter.
newTransitionEvent ::
  -- | 'eventName'
  Prelude.Text ->
  -- | 'condition'
  Prelude.Text ->
  -- | 'nextState'
  Prelude.Text ->
  TransitionEvent
newTransitionEvent
  pEventName_
  pCondition_
  pNextState_ =
    TransitionEvent'
      { actions = Prelude.Nothing,
        eventName = pEventName_,
        condition = pCondition_,
        nextState = pNextState_
      }

-- | The actions to be performed.
transitionEvent_actions :: Lens.Lens' TransitionEvent (Prelude.Maybe [Action])
transitionEvent_actions = Lens.lens (\TransitionEvent' {actions} -> actions) (\s@TransitionEvent' {} a -> s {actions = a} :: TransitionEvent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the transition event.
transitionEvent_eventName :: Lens.Lens' TransitionEvent Prelude.Text
transitionEvent_eventName = Lens.lens (\TransitionEvent' {eventName} -> eventName) (\s@TransitionEvent' {} a -> s {eventName = a} :: TransitionEvent)

-- | Required. A Boolean expression that when TRUE causes the actions to be
-- performed and the @nextState@ to be entered.
transitionEvent_condition :: Lens.Lens' TransitionEvent Prelude.Text
transitionEvent_condition = Lens.lens (\TransitionEvent' {condition} -> condition) (\s@TransitionEvent' {} a -> s {condition = a} :: TransitionEvent)

-- | The next state to enter.
transitionEvent_nextState :: Lens.Lens' TransitionEvent Prelude.Text
transitionEvent_nextState = Lens.lens (\TransitionEvent' {nextState} -> nextState) (\s@TransitionEvent' {} a -> s {nextState = a} :: TransitionEvent)

instance Data.FromJSON TransitionEvent where
  parseJSON =
    Data.withObject
      "TransitionEvent"
      ( \x ->
          TransitionEvent'
            Prelude.<$> (x Data..:? "actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "eventName")
            Prelude.<*> (x Data..: "condition")
            Prelude.<*> (x Data..: "nextState")
      )

instance Prelude.Hashable TransitionEvent where
  hashWithSalt _salt TransitionEvent' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` nextState

instance Prelude.NFData TransitionEvent where
  rnf TransitionEvent' {..} =
    Prelude.rnf actions `Prelude.seq`
      Prelude.rnf eventName `Prelude.seq`
        Prelude.rnf condition `Prelude.seq`
          Prelude.rnf nextState

instance Data.ToJSON TransitionEvent where
  toJSON TransitionEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actions" Data..=) Prelude.<$> actions,
            Prelude.Just ("eventName" Data..= eventName),
            Prelude.Just ("condition" Data..= condition),
            Prelude.Just ("nextState" Data..= nextState)
          ]
      )
