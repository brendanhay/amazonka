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
-- Module      : Network.AWS.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The container health check command and associated configuration
-- parameters for the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails = AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'
  { -- | The command that the container runs to determine whether it is healthy.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The optional grace period in seconds that allows containers time to
    -- bootstrap before failed health checks count towards the maximum number
    -- of retries.
    startPeriod :: Prelude.Maybe Prelude.Int,
    -- | The number of times to retry a failed health check before the container
    -- is considered unhealthy. The default value is 3.
    retries :: Prelude.Maybe Prelude.Int,
    -- | The time period in seconds between each health check execution. The
    -- default value is 30 seconds.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The time period in seconds to wait for a health check to succeed before
    -- it is considered a failure. The default value is 5.
    timeout :: Prelude.Maybe Prelude.Int
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
-- 'command', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command' - The command that the container runs to determine whether it is healthy.
--
-- 'startPeriod', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod' - The optional grace period in seconds that allows containers time to
-- bootstrap before failed health checks count towards the maximum number
-- of retries.
--
-- 'retries', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries' - The number of times to retry a failed health check before the container
-- is considered unhealthy. The default value is 3.
--
-- 'interval', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval' - The time period in seconds between each health check execution. The
-- default value is 30 seconds.
--
-- 'timeout', 'awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout' - The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. The default value is 5.
newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails =
  AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'
    { command =
        Prelude.Nothing,
      startPeriod =
        Prelude.Nothing,
      retries =
        Prelude.Nothing,
      interval =
        Prelude.Nothing,
      timeout =
        Prelude.Nothing
    }

-- | The command that the container runs to determine whether it is healthy.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {command} -> command) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {command = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails) Prelude.. Lens.mapping Lens.coerced

-- | The optional grace period in seconds that allows containers time to
-- bootstrap before failed health checks count towards the maximum number
-- of retries.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {startPeriod} -> startPeriod) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {startPeriod = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

-- | The number of times to retry a failed health check before the container
-- is considered unhealthy. The default value is 3.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {retries} -> retries) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {retries = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

-- | The time period in seconds between each health check execution. The
-- default value is 30 seconds.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {interval} -> interval) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {interval = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

-- | The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. The default value is 5.
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {timeout} -> timeout) (\s@AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {} a -> s {timeout = a} :: AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'
            Prelude.<$> (x Core..:? "Command" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "StartPeriod")
              Prelude.<*> (x Core..:? "Retries")
              Prelude.<*> (x Core..:? "Interval")
              Prelude.<*> (x Core..:? "Timeout")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Command" Core..=) Prelude.<$> command,
              ("StartPeriod" Core..=) Prelude.<$> startPeriod,
              ("Retries" Core..=) Prelude.<$> retries,
              ("Interval" Core..=) Prelude.<$> interval,
              ("Timeout" Core..=) Prelude.<$> timeout
            ]
        )
