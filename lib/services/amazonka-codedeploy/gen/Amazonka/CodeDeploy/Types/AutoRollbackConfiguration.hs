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
-- Module      : Amazonka.CodeDeploy.Types.AutoRollbackConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.AutoRollbackConfiguration where

import Amazonka.CodeDeploy.Types.AutoRollbackEvent
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a configuration for automatically rolling back to a
-- previous version of an application revision when a deployment is not
-- completed successfully.
--
-- /See:/ 'newAutoRollbackConfiguration' smart constructor.
data AutoRollbackConfiguration = AutoRollbackConfiguration'
  { -- | Indicates whether a defined automatic rollback configuration is
    -- currently enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The event type or types that trigger a rollback.
    events :: Prelude.Maybe [AutoRollbackEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoRollbackConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'autoRollbackConfiguration_enabled' - Indicates whether a defined automatic rollback configuration is
-- currently enabled.
--
-- 'events', 'autoRollbackConfiguration_events' - The event type or types that trigger a rollback.
newAutoRollbackConfiguration ::
  AutoRollbackConfiguration
newAutoRollbackConfiguration =
  AutoRollbackConfiguration'
    { enabled =
        Prelude.Nothing,
      events = Prelude.Nothing
    }

-- | Indicates whether a defined automatic rollback configuration is
-- currently enabled.
autoRollbackConfiguration_enabled :: Lens.Lens' AutoRollbackConfiguration (Prelude.Maybe Prelude.Bool)
autoRollbackConfiguration_enabled = Lens.lens (\AutoRollbackConfiguration' {enabled} -> enabled) (\s@AutoRollbackConfiguration' {} a -> s {enabled = a} :: AutoRollbackConfiguration)

-- | The event type or types that trigger a rollback.
autoRollbackConfiguration_events :: Lens.Lens' AutoRollbackConfiguration (Prelude.Maybe [AutoRollbackEvent])
autoRollbackConfiguration_events = Lens.lens (\AutoRollbackConfiguration' {events} -> events) (\s@AutoRollbackConfiguration' {} a -> s {events = a} :: AutoRollbackConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutoRollbackConfiguration where
  parseJSON =
    Data.withObject
      "AutoRollbackConfiguration"
      ( \x ->
          AutoRollbackConfiguration'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "events" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AutoRollbackConfiguration where
  hashWithSalt _salt AutoRollbackConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` events

instance Prelude.NFData AutoRollbackConfiguration where
  rnf AutoRollbackConfiguration' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf events

instance Data.ToJSON AutoRollbackConfiguration where
  toJSON AutoRollbackConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("events" Data..=) Prelude.<$> events
          ]
      )
