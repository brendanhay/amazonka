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
-- Module      : Amazonka.IoTEvents.Types.ClearTimerAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.ClearTimerAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information needed to clear the timer.
--
-- /See:/ 'newClearTimerAction' smart constructor.
data ClearTimerAction = ClearTimerAction'
  { -- | The name of the timer to clear.
    timerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClearTimerAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timerName', 'clearTimerAction_timerName' - The name of the timer to clear.
newClearTimerAction ::
  -- | 'timerName'
  Prelude.Text ->
  ClearTimerAction
newClearTimerAction pTimerName_ =
  ClearTimerAction' {timerName = pTimerName_}

-- | The name of the timer to clear.
clearTimerAction_timerName :: Lens.Lens' ClearTimerAction Prelude.Text
clearTimerAction_timerName = Lens.lens (\ClearTimerAction' {timerName} -> timerName) (\s@ClearTimerAction' {} a -> s {timerName = a} :: ClearTimerAction)

instance Data.FromJSON ClearTimerAction where
  parseJSON =
    Data.withObject
      "ClearTimerAction"
      ( \x ->
          ClearTimerAction'
            Prelude.<$> (x Data..: "timerName")
      )

instance Prelude.Hashable ClearTimerAction where
  hashWithSalt _salt ClearTimerAction' {..} =
    _salt `Prelude.hashWithSalt` timerName

instance Prelude.NFData ClearTimerAction where
  rnf ClearTimerAction' {..} = Prelude.rnf timerName

instance Data.ToJSON ClearTimerAction where
  toJSON ClearTimerAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("timerName" Data..= timerName)]
      )
