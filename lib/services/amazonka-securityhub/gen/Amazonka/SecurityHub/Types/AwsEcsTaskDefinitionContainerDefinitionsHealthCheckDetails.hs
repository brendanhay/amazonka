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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The container health check command and associated configuration
-- parameters for the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails = AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'
  { -- | The time period in seconds to wait for a health check to succeed before
    -- it is considered a failure. The default value is 5.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | The optional grace period in seconds that allows containers time to
    -- bootstrap before failed health checks count towards the maximum number
    -- of retries.
    startPeriod :: Prelude.Maybe Prelude.Int,
    -- | The time period in seconds between each health check execution. The
    -- default value is 30 seconds.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The command that the container runs to determine whether it is healthy.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The number of times to retry a failed health check before the container
    -- is considered unhealthy. The default value is 3.
    retries :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeout', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout' - The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. The default value is 5.
--
-- 'startPeriod', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod' - The optional grace period in seconds that allows containers time to
-- bootstrap before failed health checks count towards the maximum number
-- of retries.
--
-- 'interval', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval' - The time period in seconds between each health check execution. The
-- default value is 30 seconds.
--
-- 'command', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command' - The command that the container runs to determine whether it is healthy.
--
-- 'retries', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries' - The number of times to retry a failed health check before the container
-- is considered unhealthy. The default value is 3.
newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails =
  AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'
    { timeout =
        Prelude.Nothing,
      startPeriod =
        Prelude.Nothing,
      interval =
        Prelude.Nothing,
      command =
        Prelude.Nothing,
      retries =
        Prelude.Nothing
    }

-- | The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. The default value is 5.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {timeout} -> timeout) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {timeout = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

-- | The optional grace period in seconds that allows containers time to
-- bootstrap before failed health checks count towards the maximum number
-- of retries.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {startPeriod} -> startPeriod) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {startPeriod = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

-- | The time period in seconds between each health check execution. The
-- default value is 30 seconds.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {interval} -> interval) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {interval = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

-- | The command that the container runs to determine whether it is healthy.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {command} -> command) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {command = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of times to retry a failed health check before the container
-- is considered unhealthy. The default value is 3.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {retries} -> retries) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {retries = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'
            Prelude.<$> (x Data..:? "Timeout")
              Prelude.<*> (x Data..:? "StartPeriod")
              Prelude.<*> (x Data..:? "Interval")
              Prelude.<*> (x Data..:? "Command" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Retries")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {..} =
      _salt `Prelude.hashWithSalt` timeout
        `Prelude.hashWithSalt` startPeriod
        `Prelude.hashWithSalt` interval
        `Prelude.hashWithSalt` command
        `Prelude.hashWithSalt` retries

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {..} =
      Prelude.rnf timeout
        `Prelude.seq` Prelude.rnf startPeriod
        `Prelude.seq` Prelude.rnf interval
        `Prelude.seq` Prelude.rnf command
        `Prelude.seq` Prelude.rnf retries

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Timeout" Data..=) Prelude.<$> timeout,
              ("StartPeriod" Data..=) Prelude.<$> startPeriod,
              ("Interval" Data..=) Prelude.<$> interval,
              ("Command" Data..=) Prelude.<$> command,
              ("Retries" Data..=) Prelude.<$> retries
            ]
        )
