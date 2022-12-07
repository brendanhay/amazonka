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
-- Module      : Amazonka.IoTSecureTunneling.Types.TimeoutConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Types.TimeoutConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Tunnel timeout configuration.
--
-- /See:/ 'newTimeoutConfig' smart constructor.
data TimeoutConfig = TimeoutConfig'
  { -- | The maximum amount of time (in minutes) a tunnel can remain open. If not
    -- specified, maxLifetimeTimeoutMinutes defaults to 720 minutes. Valid
    -- values are from 1 minute to 12 hours (720 minutes)
    maxLifetimeTimeoutMinutes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeoutConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLifetimeTimeoutMinutes', 'timeoutConfig_maxLifetimeTimeoutMinutes' - The maximum amount of time (in minutes) a tunnel can remain open. If not
-- specified, maxLifetimeTimeoutMinutes defaults to 720 minutes. Valid
-- values are from 1 minute to 12 hours (720 minutes)
newTimeoutConfig ::
  TimeoutConfig
newTimeoutConfig =
  TimeoutConfig'
    { maxLifetimeTimeoutMinutes =
        Prelude.Nothing
    }

-- | The maximum amount of time (in minutes) a tunnel can remain open. If not
-- specified, maxLifetimeTimeoutMinutes defaults to 720 minutes. Valid
-- values are from 1 minute to 12 hours (720 minutes)
timeoutConfig_maxLifetimeTimeoutMinutes :: Lens.Lens' TimeoutConfig (Prelude.Maybe Prelude.Natural)
timeoutConfig_maxLifetimeTimeoutMinutes = Lens.lens (\TimeoutConfig' {maxLifetimeTimeoutMinutes} -> maxLifetimeTimeoutMinutes) (\s@TimeoutConfig' {} a -> s {maxLifetimeTimeoutMinutes = a} :: TimeoutConfig)

instance Data.FromJSON TimeoutConfig where
  parseJSON =
    Data.withObject
      "TimeoutConfig"
      ( \x ->
          TimeoutConfig'
            Prelude.<$> (x Data..:? "maxLifetimeTimeoutMinutes")
      )

instance Prelude.Hashable TimeoutConfig where
  hashWithSalt _salt TimeoutConfig' {..} =
    _salt
      `Prelude.hashWithSalt` maxLifetimeTimeoutMinutes

instance Prelude.NFData TimeoutConfig where
  rnf TimeoutConfig' {..} =
    Prelude.rnf maxLifetimeTimeoutMinutes

instance Data.ToJSON TimeoutConfig where
  toJSON TimeoutConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxLifetimeTimeoutMinutes" Data..=)
              Prelude.<$> maxLifetimeTimeoutMinutes
          ]
      )
