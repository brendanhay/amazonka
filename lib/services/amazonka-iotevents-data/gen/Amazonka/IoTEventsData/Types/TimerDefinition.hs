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
-- Module      : Amazonka.IoTEventsData.Types.TimerDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.TimerDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The new setting of a timer.
--
-- /See:/ 'newTimerDefinition' smart constructor.
data TimerDefinition = TimerDefinition'
  { -- | The name of the timer.
    name :: Prelude.Text,
    -- | The new setting of the timer (the number of seconds before the timer
    -- elapses).
    seconds :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'timerDefinition_name' - The name of the timer.
--
-- 'seconds', 'timerDefinition_seconds' - The new setting of the timer (the number of seconds before the timer
-- elapses).
newTimerDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'seconds'
  Prelude.Int ->
  TimerDefinition
newTimerDefinition pName_ pSeconds_ =
  TimerDefinition'
    { name = pName_,
      seconds = pSeconds_
    }

-- | The name of the timer.
timerDefinition_name :: Lens.Lens' TimerDefinition Prelude.Text
timerDefinition_name = Lens.lens (\TimerDefinition' {name} -> name) (\s@TimerDefinition' {} a -> s {name = a} :: TimerDefinition)

-- | The new setting of the timer (the number of seconds before the timer
-- elapses).
timerDefinition_seconds :: Lens.Lens' TimerDefinition Prelude.Int
timerDefinition_seconds = Lens.lens (\TimerDefinition' {seconds} -> seconds) (\s@TimerDefinition' {} a -> s {seconds = a} :: TimerDefinition)

instance Prelude.Hashable TimerDefinition where
  hashWithSalt _salt TimerDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` seconds

instance Prelude.NFData TimerDefinition where
  rnf TimerDefinition' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf seconds

instance Data.ToJSON TimerDefinition where
  toJSON TimerDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("seconds" Data..= seconds)
          ]
      )
