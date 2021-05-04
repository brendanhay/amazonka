{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration where

import Network.AWS.CodeDeploy.Types.AutoRollbackEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
autoRollbackConfiguration_events = Lens.lens (\AutoRollbackConfiguration' {events} -> events) (\s@AutoRollbackConfiguration' {} a -> s {events = a} :: AutoRollbackConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AutoRollbackConfiguration where
  parseJSON =
    Prelude.withObject
      "AutoRollbackConfiguration"
      ( \x ->
          AutoRollbackConfiguration'
            Prelude.<$> (x Prelude..:? "enabled")
            Prelude.<*> (x Prelude..:? "events" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable AutoRollbackConfiguration

instance Prelude.NFData AutoRollbackConfiguration

instance Prelude.ToJSON AutoRollbackConfiguration where
  toJSON AutoRollbackConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("enabled" Prelude..=) Prelude.<$> enabled,
            ("events" Prelude..=) Prelude.<$> events
          ]
      )
