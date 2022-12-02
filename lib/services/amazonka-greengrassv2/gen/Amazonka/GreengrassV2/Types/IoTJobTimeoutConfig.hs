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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobTimeoutConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobTimeoutConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the timeout configuration for a job.
--
-- /See:/ 'newIoTJobTimeoutConfig' smart constructor.
data IoTJobTimeoutConfig = IoTJobTimeoutConfig'
  { -- | The amount of time, in minutes, that devices have to complete the job.
    -- The timer starts when the job status is set to @IN_PROGRESS@. If the job
    -- status doesn\'t change to a terminal state before the time expires, then
    -- the job status is set to @TIMED_OUT@.
    --
    -- The timeout interval must be between 1 minute and 7 days (10080
    -- minutes).
    inProgressTimeoutInMinutes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IoTJobTimeoutConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inProgressTimeoutInMinutes', 'ioTJobTimeoutConfig_inProgressTimeoutInMinutes' - The amount of time, in minutes, that devices have to complete the job.
-- The timer starts when the job status is set to @IN_PROGRESS@. If the job
-- status doesn\'t change to a terminal state before the time expires, then
-- the job status is set to @TIMED_OUT@.
--
-- The timeout interval must be between 1 minute and 7 days (10080
-- minutes).
newIoTJobTimeoutConfig ::
  IoTJobTimeoutConfig
newIoTJobTimeoutConfig =
  IoTJobTimeoutConfig'
    { inProgressTimeoutInMinutes =
        Prelude.Nothing
    }

-- | The amount of time, in minutes, that devices have to complete the job.
-- The timer starts when the job status is set to @IN_PROGRESS@. If the job
-- status doesn\'t change to a terminal state before the time expires, then
-- the job status is set to @TIMED_OUT@.
--
-- The timeout interval must be between 1 minute and 7 days (10080
-- minutes).
ioTJobTimeoutConfig_inProgressTimeoutInMinutes :: Lens.Lens' IoTJobTimeoutConfig (Prelude.Maybe Prelude.Integer)
ioTJobTimeoutConfig_inProgressTimeoutInMinutes = Lens.lens (\IoTJobTimeoutConfig' {inProgressTimeoutInMinutes} -> inProgressTimeoutInMinutes) (\s@IoTJobTimeoutConfig' {} a -> s {inProgressTimeoutInMinutes = a} :: IoTJobTimeoutConfig)

instance Data.FromJSON IoTJobTimeoutConfig where
  parseJSON =
    Data.withObject
      "IoTJobTimeoutConfig"
      ( \x ->
          IoTJobTimeoutConfig'
            Prelude.<$> (x Data..:? "inProgressTimeoutInMinutes")
      )

instance Prelude.Hashable IoTJobTimeoutConfig where
  hashWithSalt _salt IoTJobTimeoutConfig' {..} =
    _salt
      `Prelude.hashWithSalt` inProgressTimeoutInMinutes

instance Prelude.NFData IoTJobTimeoutConfig where
  rnf IoTJobTimeoutConfig' {..} =
    Prelude.rnf inProgressTimeoutInMinutes

instance Data.ToJSON IoTJobTimeoutConfig where
  toJSON IoTJobTimeoutConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inProgressTimeoutInMinutes" Data..=)
              Prelude.<$> inProgressTimeoutInMinutes
          ]
      )
