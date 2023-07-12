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
-- Module      : Amazonka.IoTEvents.Types.OnInputLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.OnInputLifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Event
import Amazonka.IoTEvents.Types.TransitionEvent
import qualified Amazonka.Prelude as Prelude

-- | Specifies the actions performed when the @condition@ evaluates to TRUE.
--
-- /See:/ 'newOnInputLifecycle' smart constructor.
data OnInputLifecycle = OnInputLifecycle'
  { -- | Specifies the actions performed when the @condition@ evaluates to TRUE.
    events :: Prelude.Maybe [Event],
    -- | Specifies the actions performed, and the next state entered, when a
    -- @condition@ evaluates to TRUE.
    transitionEvents :: Prelude.Maybe [TransitionEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnInputLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'onInputLifecycle_events' - Specifies the actions performed when the @condition@ evaluates to TRUE.
--
-- 'transitionEvents', 'onInputLifecycle_transitionEvents' - Specifies the actions performed, and the next state entered, when a
-- @condition@ evaluates to TRUE.
newOnInputLifecycle ::
  OnInputLifecycle
newOnInputLifecycle =
  OnInputLifecycle'
    { events = Prelude.Nothing,
      transitionEvents = Prelude.Nothing
    }

-- | Specifies the actions performed when the @condition@ evaluates to TRUE.
onInputLifecycle_events :: Lens.Lens' OnInputLifecycle (Prelude.Maybe [Event])
onInputLifecycle_events = Lens.lens (\OnInputLifecycle' {events} -> events) (\s@OnInputLifecycle' {} a -> s {events = a} :: OnInputLifecycle) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the actions performed, and the next state entered, when a
-- @condition@ evaluates to TRUE.
onInputLifecycle_transitionEvents :: Lens.Lens' OnInputLifecycle (Prelude.Maybe [TransitionEvent])
onInputLifecycle_transitionEvents = Lens.lens (\OnInputLifecycle' {transitionEvents} -> transitionEvents) (\s@OnInputLifecycle' {} a -> s {transitionEvents = a} :: OnInputLifecycle) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OnInputLifecycle where
  parseJSON =
    Data.withObject
      "OnInputLifecycle"
      ( \x ->
          OnInputLifecycle'
            Prelude.<$> (x Data..:? "events" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "transitionEvents"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OnInputLifecycle where
  hashWithSalt _salt OnInputLifecycle' {..} =
    _salt
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` transitionEvents

instance Prelude.NFData OnInputLifecycle where
  rnf OnInputLifecycle' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf transitionEvents

instance Data.ToJSON OnInputLifecycle where
  toJSON OnInputLifecycle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("events" Data..=) Prelude.<$> events,
            ("transitionEvents" Data..=)
              Prelude.<$> transitionEvents
          ]
      )
