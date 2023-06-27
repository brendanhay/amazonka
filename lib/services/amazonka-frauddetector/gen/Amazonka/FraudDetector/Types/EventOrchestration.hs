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
-- Module      : Amazonka.FraudDetector.Types.EventOrchestration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EventOrchestration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The event orchestration status.
--
-- /See:/ 'newEventOrchestration' smart constructor.
data EventOrchestration = EventOrchestration'
  { -- | Specifies if event orchestration is enabled through Amazon EventBridge.
    eventBridgeEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventOrchestration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBridgeEnabled', 'eventOrchestration_eventBridgeEnabled' - Specifies if event orchestration is enabled through Amazon EventBridge.
newEventOrchestration ::
  -- | 'eventBridgeEnabled'
  Prelude.Bool ->
  EventOrchestration
newEventOrchestration pEventBridgeEnabled_ =
  EventOrchestration'
    { eventBridgeEnabled =
        pEventBridgeEnabled_
    }

-- | Specifies if event orchestration is enabled through Amazon EventBridge.
eventOrchestration_eventBridgeEnabled :: Lens.Lens' EventOrchestration Prelude.Bool
eventOrchestration_eventBridgeEnabled = Lens.lens (\EventOrchestration' {eventBridgeEnabled} -> eventBridgeEnabled) (\s@EventOrchestration' {} a -> s {eventBridgeEnabled = a} :: EventOrchestration)

instance Data.FromJSON EventOrchestration where
  parseJSON =
    Data.withObject
      "EventOrchestration"
      ( \x ->
          EventOrchestration'
            Prelude.<$> (x Data..: "eventBridgeEnabled")
      )

instance Prelude.Hashable EventOrchestration where
  hashWithSalt _salt EventOrchestration' {..} =
    _salt `Prelude.hashWithSalt` eventBridgeEnabled

instance Prelude.NFData EventOrchestration where
  rnf EventOrchestration' {..} =
    Prelude.rnf eventBridgeEnabled

instance Data.ToJSON EventOrchestration where
  toJSON EventOrchestration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eventBridgeEnabled" Data..= eventBridgeEnabled)
          ]
      )
