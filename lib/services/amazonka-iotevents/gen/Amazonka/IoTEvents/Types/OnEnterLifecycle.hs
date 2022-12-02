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
-- Module      : Amazonka.IoTEvents.Types.OnEnterLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.OnEnterLifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Event
import qualified Amazonka.Prelude as Prelude

-- | When entering this state, perform these @actions@ if the @condition@ is
-- TRUE.
--
-- /See:/ 'newOnEnterLifecycle' smart constructor.
data OnEnterLifecycle = OnEnterLifecycle'
  { -- | Specifies the actions that are performed when the state is entered and
    -- the @condition@ is @TRUE@.
    events :: Prelude.Maybe [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnEnterLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'onEnterLifecycle_events' - Specifies the actions that are performed when the state is entered and
-- the @condition@ is @TRUE@.
newOnEnterLifecycle ::
  OnEnterLifecycle
newOnEnterLifecycle =
  OnEnterLifecycle' {events = Prelude.Nothing}

-- | Specifies the actions that are performed when the state is entered and
-- the @condition@ is @TRUE@.
onEnterLifecycle_events :: Lens.Lens' OnEnterLifecycle (Prelude.Maybe [Event])
onEnterLifecycle_events = Lens.lens (\OnEnterLifecycle' {events} -> events) (\s@OnEnterLifecycle' {} a -> s {events = a} :: OnEnterLifecycle) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OnEnterLifecycle where
  parseJSON =
    Data.withObject
      "OnEnterLifecycle"
      ( \x ->
          OnEnterLifecycle'
            Prelude.<$> (x Data..:? "events" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OnEnterLifecycle where
  hashWithSalt _salt OnEnterLifecycle' {..} =
    _salt `Prelude.hashWithSalt` events

instance Prelude.NFData OnEnterLifecycle where
  rnf OnEnterLifecycle' {..} = Prelude.rnf events

instance Data.ToJSON OnEnterLifecycle where
  toJSON OnEnterLifecycle' {..} =
    Data.object
      ( Prelude.catMaybes
          [("events" Data..=) Prelude.<$> events]
      )
