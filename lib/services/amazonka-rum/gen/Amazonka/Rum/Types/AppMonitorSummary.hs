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
-- Module      : Amazonka.Rum.Types.AppMonitorSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.AppMonitorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.StateEnum

-- | A structure that includes some data about app monitors and their
-- settings.
--
-- /See:/ 'newAppMonitorSummary' smart constructor.
data AppMonitorSummary = AppMonitorSummary'
  { -- | The date and time that the app monitor was created.
    created :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of this app monitor.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the most recent changes to this app monitor\'s
    -- configuration.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The name of this app monitor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current state of this app monitor.
    state :: Prelude.Maybe StateEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppMonitorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'appMonitorSummary_created' - The date and time that the app monitor was created.
--
-- 'id', 'appMonitorSummary_id' - The unique ID of this app monitor.
--
-- 'lastModified', 'appMonitorSummary_lastModified' - The date and time of the most recent changes to this app monitor\'s
-- configuration.
--
-- 'name', 'appMonitorSummary_name' - The name of this app monitor.
--
-- 'state', 'appMonitorSummary_state' - The current state of this app monitor.
newAppMonitorSummary ::
  AppMonitorSummary
newAppMonitorSummary =
  AppMonitorSummary'
    { created = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The date and time that the app monitor was created.
appMonitorSummary_created :: Lens.Lens' AppMonitorSummary (Prelude.Maybe Prelude.Text)
appMonitorSummary_created = Lens.lens (\AppMonitorSummary' {created} -> created) (\s@AppMonitorSummary' {} a -> s {created = a} :: AppMonitorSummary)

-- | The unique ID of this app monitor.
appMonitorSummary_id :: Lens.Lens' AppMonitorSummary (Prelude.Maybe Prelude.Text)
appMonitorSummary_id = Lens.lens (\AppMonitorSummary' {id} -> id) (\s@AppMonitorSummary' {} a -> s {id = a} :: AppMonitorSummary)

-- | The date and time of the most recent changes to this app monitor\'s
-- configuration.
appMonitorSummary_lastModified :: Lens.Lens' AppMonitorSummary (Prelude.Maybe Prelude.Text)
appMonitorSummary_lastModified = Lens.lens (\AppMonitorSummary' {lastModified} -> lastModified) (\s@AppMonitorSummary' {} a -> s {lastModified = a} :: AppMonitorSummary)

-- | The name of this app monitor.
appMonitorSummary_name :: Lens.Lens' AppMonitorSummary (Prelude.Maybe Prelude.Text)
appMonitorSummary_name = Lens.lens (\AppMonitorSummary' {name} -> name) (\s@AppMonitorSummary' {} a -> s {name = a} :: AppMonitorSummary)

-- | The current state of this app monitor.
appMonitorSummary_state :: Lens.Lens' AppMonitorSummary (Prelude.Maybe StateEnum)
appMonitorSummary_state = Lens.lens (\AppMonitorSummary' {state} -> state) (\s@AppMonitorSummary' {} a -> s {state = a} :: AppMonitorSummary)

instance Data.FromJSON AppMonitorSummary where
  parseJSON =
    Data.withObject
      "AppMonitorSummary"
      ( \x ->
          AppMonitorSummary'
            Prelude.<$> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable AppMonitorSummary where
  hashWithSalt _salt AppMonitorSummary' {..} =
    _salt `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData AppMonitorSummary where
  rnf AppMonitorSummary' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
