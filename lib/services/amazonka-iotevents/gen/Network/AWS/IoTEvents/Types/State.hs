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
-- Module      : Network.AWS.IoTEvents.Types.State
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.State where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types.OnEnterLifecycle
import Network.AWS.IoTEvents.Types.OnExitLifecycle
import Network.AWS.IoTEvents.Types.OnInputLifecycle
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information that defines a state of a detector.
--
-- /See:/ 'newState' smart constructor.
data State = State'
  { -- | When entering this state, perform these @actions@ if the @condition@ is
    -- TRUE.
    onEnter :: Prelude.Maybe OnEnterLifecycle,
    -- | When an input is received and the @condition@ is TRUE, perform the
    -- specified @actions@.
    onInput :: Prelude.Maybe OnInputLifecycle,
    -- | When exiting this state, perform these @actions@ if the specified
    -- @condition@ is @TRUE@.
    onExit :: Prelude.Maybe OnExitLifecycle,
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
-- 'onInput', 'state_onInput' - When an input is received and the @condition@ is TRUE, perform the
-- specified @actions@.
--
-- 'onExit', 'state_onExit' - When exiting this state, perform these @actions@ if the specified
-- @condition@ is @TRUE@.
--
-- 'stateName', 'state_stateName' - The name of the state.
newState ::
  -- | 'stateName'
  Prelude.Text ->
  State
newState pStateName_ =
  State'
    { onEnter = Prelude.Nothing,
      onInput = Prelude.Nothing,
      onExit = Prelude.Nothing,
      stateName = pStateName_
    }

-- | When entering this state, perform these @actions@ if the @condition@ is
-- TRUE.
state_onEnter :: Lens.Lens' State (Prelude.Maybe OnEnterLifecycle)
state_onEnter = Lens.lens (\State' {onEnter} -> onEnter) (\s@State' {} a -> s {onEnter = a} :: State)

-- | When an input is received and the @condition@ is TRUE, perform the
-- specified @actions@.
state_onInput :: Lens.Lens' State (Prelude.Maybe OnInputLifecycle)
state_onInput = Lens.lens (\State' {onInput} -> onInput) (\s@State' {} a -> s {onInput = a} :: State)

-- | When exiting this state, perform these @actions@ if the specified
-- @condition@ is @TRUE@.
state_onExit :: Lens.Lens' State (Prelude.Maybe OnExitLifecycle)
state_onExit = Lens.lens (\State' {onExit} -> onExit) (\s@State' {} a -> s {onExit = a} :: State)

-- | The name of the state.
state_stateName :: Lens.Lens' State Prelude.Text
state_stateName = Lens.lens (\State' {stateName} -> stateName) (\s@State' {} a -> s {stateName = a} :: State)

instance Core.FromJSON State where
  parseJSON =
    Core.withObject
      "State"
      ( \x ->
          State'
            Prelude.<$> (x Core..:? "onEnter")
            Prelude.<*> (x Core..:? "onInput")
            Prelude.<*> (x Core..:? "onExit")
            Prelude.<*> (x Core..: "stateName")
      )

instance Prelude.Hashable State

instance Prelude.NFData State

instance Core.ToJSON State where
  toJSON State' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("onEnter" Core..=) Prelude.<$> onEnter,
            ("onInput" Core..=) Prelude.<$> onInput,
            ("onExit" Core..=) Prelude.<$> onExit,
            Prelude.Just ("stateName" Core..= stateName)
          ]
      )
