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
-- Module      : Amazonka.IoTEvents.Types.ResetTimerAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.ResetTimerAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information required to reset the timer. The timer is reset to the
-- previously evaluated result of the duration. The duration expression
-- isn\'t reevaluated when you reset the timer.
--
-- /See:/ 'newResetTimerAction' smart constructor.
data ResetTimerAction = ResetTimerAction'
  { -- | The name of the timer to reset.
    timerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetTimerAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timerName', 'resetTimerAction_timerName' - The name of the timer to reset.
newResetTimerAction ::
  -- | 'timerName'
  Prelude.Text ->
  ResetTimerAction
newResetTimerAction pTimerName_ =
  ResetTimerAction' {timerName = pTimerName_}

-- | The name of the timer to reset.
resetTimerAction_timerName :: Lens.Lens' ResetTimerAction Prelude.Text
resetTimerAction_timerName = Lens.lens (\ResetTimerAction' {timerName} -> timerName) (\s@ResetTimerAction' {} a -> s {timerName = a} :: ResetTimerAction)

instance Data.FromJSON ResetTimerAction where
  parseJSON =
    Data.withObject
      "ResetTimerAction"
      ( \x ->
          ResetTimerAction'
            Prelude.<$> (x Data..: "timerName")
      )

instance Prelude.Hashable ResetTimerAction where
  hashWithSalt _salt ResetTimerAction' {..} =
    _salt `Prelude.hashWithSalt` timerName

instance Prelude.NFData ResetTimerAction where
  rnf ResetTimerAction' {..} = Prelude.rnf timerName

instance Data.ToJSON ResetTimerAction where
  toJSON ResetTimerAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("timerName" Data..= timerName)]
      )
