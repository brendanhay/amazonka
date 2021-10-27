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
-- Module      : Network.AWS.IoTEvents.Types.AlarmCapabilities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.AlarmCapabilities where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types.AcknowledgeFlow
import Network.AWS.IoTEvents.Types.InitializationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON AlarmCapabilities where
  parseJSON =
    Core.withObject
      "AlarmCapabilities"
      ( \x ->
          AlarmCapabilities'
            Prelude.<$> (x Core..:? "acknowledgeFlow")
            Prelude.<*> (x Core..:? "initializationConfiguration")
      )

instance Prelude.Hashable AlarmCapabilities

instance Prelude.NFData AlarmCapabilities

instance Core.ToJSON AlarmCapabilities where
  toJSON AlarmCapabilities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("acknowledgeFlow" Core..=)
              Prelude.<$> acknowledgeFlow,
            ("initializationConfiguration" Core..=)
              Prelude.<$> initializationConfiguration
          ]
      )
