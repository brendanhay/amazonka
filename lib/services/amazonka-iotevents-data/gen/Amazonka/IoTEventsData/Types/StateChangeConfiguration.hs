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
-- Module      : Amazonka.IoTEventsData.Types.StateChangeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.StateChangeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types.TriggerType
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of alarm state changes.
--
-- /See:/ 'newStateChangeConfiguration' smart constructor.
data StateChangeConfiguration = StateChangeConfiguration'
  { -- | The trigger type. If the value is @SNOOZE_TIMEOUT@, the snooze duration
    -- ends and the alarm automatically changes to the @NORMAL@ state.
    triggerType :: Prelude.Maybe TriggerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StateChangeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerType', 'stateChangeConfiguration_triggerType' - The trigger type. If the value is @SNOOZE_TIMEOUT@, the snooze duration
-- ends and the alarm automatically changes to the @NORMAL@ state.
newStateChangeConfiguration ::
  StateChangeConfiguration
newStateChangeConfiguration =
  StateChangeConfiguration'
    { triggerType =
        Prelude.Nothing
    }

-- | The trigger type. If the value is @SNOOZE_TIMEOUT@, the snooze duration
-- ends and the alarm automatically changes to the @NORMAL@ state.
stateChangeConfiguration_triggerType :: Lens.Lens' StateChangeConfiguration (Prelude.Maybe TriggerType)
stateChangeConfiguration_triggerType = Lens.lens (\StateChangeConfiguration' {triggerType} -> triggerType) (\s@StateChangeConfiguration' {} a -> s {triggerType = a} :: StateChangeConfiguration)

instance Core.FromJSON StateChangeConfiguration where
  parseJSON =
    Core.withObject
      "StateChangeConfiguration"
      ( \x ->
          StateChangeConfiguration'
            Prelude.<$> (x Core..:? "triggerType")
      )

instance Prelude.Hashable StateChangeConfiguration where
  hashWithSalt _salt StateChangeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` triggerType

instance Prelude.NFData StateChangeConfiguration where
  rnf StateChangeConfiguration' {..} =
    Prelude.rnf triggerType
