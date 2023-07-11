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
-- Module      : Amazonka.IoTEvents.Types.AlarmCapabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmCapabilities where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.AcknowledgeFlow
import Amazonka.IoTEvents.Types.InitializationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of alarm state changes.
--
-- /See:/ 'newAlarmCapabilities' smart constructor.
data AlarmCapabilities = AlarmCapabilities'
  { -- | Specifies whether to get notified for alarm state changes.
    acknowledgeFlow :: Prelude.Maybe AcknowledgeFlow,
    -- | Specifies the default alarm state. The configuration applies to all
    -- alarms that were created based on this alarm model.
    initializationConfiguration :: Prelude.Maybe InitializationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmCapabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acknowledgeFlow', 'alarmCapabilities_acknowledgeFlow' - Specifies whether to get notified for alarm state changes.
--
-- 'initializationConfiguration', 'alarmCapabilities_initializationConfiguration' - Specifies the default alarm state. The configuration applies to all
-- alarms that were created based on this alarm model.
newAlarmCapabilities ::
  AlarmCapabilities
newAlarmCapabilities =
  AlarmCapabilities'
    { acknowledgeFlow =
        Prelude.Nothing,
      initializationConfiguration = Prelude.Nothing
    }

-- | Specifies whether to get notified for alarm state changes.
alarmCapabilities_acknowledgeFlow :: Lens.Lens' AlarmCapabilities (Prelude.Maybe AcknowledgeFlow)
alarmCapabilities_acknowledgeFlow = Lens.lens (\AlarmCapabilities' {acknowledgeFlow} -> acknowledgeFlow) (\s@AlarmCapabilities' {} a -> s {acknowledgeFlow = a} :: AlarmCapabilities)

-- | Specifies the default alarm state. The configuration applies to all
-- alarms that were created based on this alarm model.
alarmCapabilities_initializationConfiguration :: Lens.Lens' AlarmCapabilities (Prelude.Maybe InitializationConfiguration)
alarmCapabilities_initializationConfiguration = Lens.lens (\AlarmCapabilities' {initializationConfiguration} -> initializationConfiguration) (\s@AlarmCapabilities' {} a -> s {initializationConfiguration = a} :: AlarmCapabilities)

instance Data.FromJSON AlarmCapabilities where
  parseJSON =
    Data.withObject
      "AlarmCapabilities"
      ( \x ->
          AlarmCapabilities'
            Prelude.<$> (x Data..:? "acknowledgeFlow")
            Prelude.<*> (x Data..:? "initializationConfiguration")
      )

instance Prelude.Hashable AlarmCapabilities where
  hashWithSalt _salt AlarmCapabilities' {..} =
    _salt
      `Prelude.hashWithSalt` acknowledgeFlow
      `Prelude.hashWithSalt` initializationConfiguration

instance Prelude.NFData AlarmCapabilities where
  rnf AlarmCapabilities' {..} =
    Prelude.rnf acknowledgeFlow
      `Prelude.seq` Prelude.rnf initializationConfiguration

instance Data.ToJSON AlarmCapabilities where
  toJSON AlarmCapabilities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("acknowledgeFlow" Data..=)
              Prelude.<$> acknowledgeFlow,
            ("initializationConfiguration" Data..=)
              Prelude.<$> initializationConfiguration
          ]
      )
