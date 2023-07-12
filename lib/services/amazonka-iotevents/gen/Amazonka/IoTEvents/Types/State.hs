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
-- Module      : Amazonka.IoTEvents.Types.State
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.State where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.OnEnterLifecycle
import Amazonka.IoTEvents.Types.OnExitLifecycle
import Amazonka.IoTEvents.Types.OnInputLifecycle
import qualified Amazonka.Prelude as Prelude

-- | Information that defines a state of a detector.
--
-- /See:/ 'newState' smart constructor.
data State = State'
  { -- | When entering this state, perform these @actions@ if the @condition@ is
    -- TRUE.
    onEnter :: Prelude.Maybe OnEnterLifecycle,
    -- | When exiting this state, perform these @actions@ if the specified
    -- @condition@ is @TRUE@.
    onExit :: Prelude.Maybe OnExitLifecycle,
    -- | When an input is received and the @condition@ is TRUE, perform the
    -- specified @actions@.
    onInput :: Prelude.Maybe OnInputLifecycle,
    -- | The name of the state.
    stateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'State' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onEnter', 'state_onEnter' - When entering this state, perform these @actions@ if the @condition@ is
-- TRUE.
--
-- 'onExit', 'state_onExit' - When exiting this state, perform these @actions@ if the specified
-- @condition@ is @TRUE@.
--
-- 'onInput', 'state_onInput' - When an input is received and the @condition@ is TRUE, perform the
-- specified @actions@.
--
-- 'stateName', 'state_stateName' - The name of the state.
newState ::
  -- | 'stateName'
  Prelude.Text ->
  State
newState pStateName_ =
  State'
    { onEnter = Prelude.Nothing,
      onExit = Prelude.Nothing,
      onInput = Prelude.Nothing,
      stateName = pStateName_
    }

-- | When entering this state, perform these @actions@ if the @condition@ is
-- TRUE.
state_onEnter :: Lens.Lens' State (Prelude.Maybe OnEnterLifecycle)
state_onEnter = Lens.lens (\State' {onEnter} -> onEnter) (\s@State' {} a -> s {onEnter = a} :: State)

-- | When exiting this state, perform these @actions@ if the specified
-- @condition@ is @TRUE@.
state_onExit :: Lens.Lens' State (Prelude.Maybe OnExitLifecycle)
state_onExit = Lens.lens (\State' {onExit} -> onExit) (\s@State' {} a -> s {onExit = a} :: State)

-- | When an input is received and the @condition@ is TRUE, perform the
-- specified @actions@.
state_onInput :: Lens.Lens' State (Prelude.Maybe OnInputLifecycle)
state_onInput = Lens.lens (\State' {onInput} -> onInput) (\s@State' {} a -> s {onInput = a} :: State)

-- | The name of the state.
state_stateName :: Lens.Lens' State Prelude.Text
state_stateName = Lens.lens (\State' {stateName} -> stateName) (\s@State' {} a -> s {stateName = a} :: State)

instance Data.FromJSON State where
  parseJSON =
    Data.withObject
      "State"
      ( \x ->
          State'
            Prelude.<$> (x Data..:? "onEnter")
            Prelude.<*> (x Data..:? "onExit")
            Prelude.<*> (x Data..:? "onInput")
            Prelude.<*> (x Data..: "stateName")
      )

instance Prelude.Hashable State where
  hashWithSalt _salt State' {..} =
    _salt
      `Prelude.hashWithSalt` onEnter
      `Prelude.hashWithSalt` onExit
      `Prelude.hashWithSalt` onInput
      `Prelude.hashWithSalt` stateName

instance Prelude.NFData State where
  rnf State' {..} =
    Prelude.rnf onEnter
      `Prelude.seq` Prelude.rnf onExit
      `Prelude.seq` Prelude.rnf onInput
      `Prelude.seq` Prelude.rnf stateName

instance Data.ToJSON State where
  toJSON State' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("onEnter" Data..=) Prelude.<$> onEnter,
            ("onExit" Data..=) Prelude.<$> onExit,
            ("onInput" Data..=) Prelude.<$> onInput,
            Prelude.Just ("stateName" Data..= stateName)
          ]
      )
