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
-- Module      : Amazonka.IoTEventsData.Types.SystemEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.SystemEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.EventType
import Amazonka.IoTEventsData.Types.StateChangeConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains information about alarm state changes.
--
-- /See:/ 'newSystemEvent' smart constructor.
data SystemEvent = SystemEvent'
  { -- | The event type. If the value is @STATE_CHANGE@, the event contains
    -- information about alarm state changes.
    eventType :: Prelude.Maybe EventType,
    -- | Contains the configuration information of alarm state changes.
    stateChangeConfiguration :: Prelude.Maybe StateChangeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'systemEvent_eventType' - The event type. If the value is @STATE_CHANGE@, the event contains
-- information about alarm state changes.
--
-- 'stateChangeConfiguration', 'systemEvent_stateChangeConfiguration' - Contains the configuration information of alarm state changes.
newSystemEvent ::
  SystemEvent
newSystemEvent =
  SystemEvent'
    { eventType = Prelude.Nothing,
      stateChangeConfiguration = Prelude.Nothing
    }

-- | The event type. If the value is @STATE_CHANGE@, the event contains
-- information about alarm state changes.
systemEvent_eventType :: Lens.Lens' SystemEvent (Prelude.Maybe EventType)
systemEvent_eventType = Lens.lens (\SystemEvent' {eventType} -> eventType) (\s@SystemEvent' {} a -> s {eventType = a} :: SystemEvent)

-- | Contains the configuration information of alarm state changes.
systemEvent_stateChangeConfiguration :: Lens.Lens' SystemEvent (Prelude.Maybe StateChangeConfiguration)
systemEvent_stateChangeConfiguration = Lens.lens (\SystemEvent' {stateChangeConfiguration} -> stateChangeConfiguration) (\s@SystemEvent' {} a -> s {stateChangeConfiguration = a} :: SystemEvent)

instance Data.FromJSON SystemEvent where
  parseJSON =
    Data.withObject
      "SystemEvent"
      ( \x ->
          SystemEvent'
            Prelude.<$> (x Data..:? "eventType")
            Prelude.<*> (x Data..:? "stateChangeConfiguration")
      )

instance Prelude.Hashable SystemEvent where
  hashWithSalt _salt SystemEvent' {..} =
    _salt
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` stateChangeConfiguration

instance Prelude.NFData SystemEvent where
  rnf SystemEvent' {..} =
    Prelude.rnf eventType `Prelude.seq`
      Prelude.rnf stateChangeConfiguration
