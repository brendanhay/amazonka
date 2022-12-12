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
-- Module      : Amazonka.EMRServerless.Types.AutoStopConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.AutoStopConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an application to automatically stop after a
-- certain amount of time being idle.
--
-- /See:/ 'newAutoStopConfig' smart constructor.
data AutoStopConfig = AutoStopConfig'
  { -- | Enables the application to automatically stop after a certain amount of
    -- time being idle. Defaults to true.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The amount of idle time in minutes after which your application will
    -- automatically stop. Defaults to 15 minutes.
    idleTimeoutMinutes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoStopConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'autoStopConfig_enabled' - Enables the application to automatically stop after a certain amount of
-- time being idle. Defaults to true.
--
-- 'idleTimeoutMinutes', 'autoStopConfig_idleTimeoutMinutes' - The amount of idle time in minutes after which your application will
-- automatically stop. Defaults to 15 minutes.
newAutoStopConfig ::
  AutoStopConfig
newAutoStopConfig =
  AutoStopConfig'
    { enabled = Prelude.Nothing,
      idleTimeoutMinutes = Prelude.Nothing
    }

-- | Enables the application to automatically stop after a certain amount of
-- time being idle. Defaults to true.
autoStopConfig_enabled :: Lens.Lens' AutoStopConfig (Prelude.Maybe Prelude.Bool)
autoStopConfig_enabled = Lens.lens (\AutoStopConfig' {enabled} -> enabled) (\s@AutoStopConfig' {} a -> s {enabled = a} :: AutoStopConfig)

-- | The amount of idle time in minutes after which your application will
-- automatically stop. Defaults to 15 minutes.
autoStopConfig_idleTimeoutMinutes :: Lens.Lens' AutoStopConfig (Prelude.Maybe Prelude.Natural)
autoStopConfig_idleTimeoutMinutes = Lens.lens (\AutoStopConfig' {idleTimeoutMinutes} -> idleTimeoutMinutes) (\s@AutoStopConfig' {} a -> s {idleTimeoutMinutes = a} :: AutoStopConfig)

instance Data.FromJSON AutoStopConfig where
  parseJSON =
    Data.withObject
      "AutoStopConfig"
      ( \x ->
          AutoStopConfig'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "idleTimeoutMinutes")
      )

instance Prelude.Hashable AutoStopConfig where
  hashWithSalt _salt AutoStopConfig' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` idleTimeoutMinutes

instance Prelude.NFData AutoStopConfig where
  rnf AutoStopConfig' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf idleTimeoutMinutes

instance Data.ToJSON AutoStopConfig where
  toJSON AutoStopConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("idleTimeoutMinutes" Data..=)
              Prelude.<$> idleTimeoutMinutes
          ]
      )
