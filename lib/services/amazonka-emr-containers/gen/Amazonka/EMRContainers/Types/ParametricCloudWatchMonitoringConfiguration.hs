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
-- Module      : Amazonka.EMRContainers.Types.ParametricCloudWatchMonitoringConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ParametricCloudWatchMonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A configuration for CloudWatch monitoring. You can configure your jobs
-- to send log information to CloudWatch Logs. This data type allows job
-- template parameters to be specified within.
--
-- /See:/ 'newParametricCloudWatchMonitoringConfiguration' smart constructor.
data ParametricCloudWatchMonitoringConfiguration = ParametricCloudWatchMonitoringConfiguration'
  { -- | The specified name prefix for log streams.
    logStreamNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group for log publishing.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParametricCloudWatchMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamNamePrefix', 'parametricCloudWatchMonitoringConfiguration_logStreamNamePrefix' - The specified name prefix for log streams.
--
-- 'logGroupName', 'parametricCloudWatchMonitoringConfiguration_logGroupName' - The name of the log group for log publishing.
newParametricCloudWatchMonitoringConfiguration ::
  ParametricCloudWatchMonitoringConfiguration
newParametricCloudWatchMonitoringConfiguration =
  ParametricCloudWatchMonitoringConfiguration'
    { logStreamNamePrefix =
        Prelude.Nothing,
      logGroupName = Prelude.Nothing
    }

-- | The specified name prefix for log streams.
parametricCloudWatchMonitoringConfiguration_logStreamNamePrefix :: Lens.Lens' ParametricCloudWatchMonitoringConfiguration (Prelude.Maybe Prelude.Text)
parametricCloudWatchMonitoringConfiguration_logStreamNamePrefix = Lens.lens (\ParametricCloudWatchMonitoringConfiguration' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@ParametricCloudWatchMonitoringConfiguration' {} a -> s {logStreamNamePrefix = a} :: ParametricCloudWatchMonitoringConfiguration)

-- | The name of the log group for log publishing.
parametricCloudWatchMonitoringConfiguration_logGroupName :: Lens.Lens' ParametricCloudWatchMonitoringConfiguration (Prelude.Maybe Prelude.Text)
parametricCloudWatchMonitoringConfiguration_logGroupName = Lens.lens (\ParametricCloudWatchMonitoringConfiguration' {logGroupName} -> logGroupName) (\s@ParametricCloudWatchMonitoringConfiguration' {} a -> s {logGroupName = a} :: ParametricCloudWatchMonitoringConfiguration)

instance
  Core.FromJSON
    ParametricCloudWatchMonitoringConfiguration
  where
  parseJSON =
    Core.withObject
      "ParametricCloudWatchMonitoringConfiguration"
      ( \x ->
          ParametricCloudWatchMonitoringConfiguration'
            Prelude.<$> (x Core..:? "logStreamNamePrefix")
              Prelude.<*> (x Core..:? "logGroupName")
      )

instance
  Prelude.Hashable
    ParametricCloudWatchMonitoringConfiguration
  where
  hashWithSalt
    _salt
    ParametricCloudWatchMonitoringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` logStreamNamePrefix
        `Prelude.hashWithSalt` logGroupName

instance
  Prelude.NFData
    ParametricCloudWatchMonitoringConfiguration
  where
  rnf ParametricCloudWatchMonitoringConfiguration' {..} =
    Prelude.rnf logStreamNamePrefix
      `Prelude.seq` Prelude.rnf logGroupName

instance
  Core.ToJSON
    ParametricCloudWatchMonitoringConfiguration
  where
  toJSON
    ParametricCloudWatchMonitoringConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("logStreamNamePrefix" Core..=)
                Prelude.<$> logStreamNamePrefix,
              ("logGroupName" Core..=) Prelude.<$> logGroupName
            ]
        )
