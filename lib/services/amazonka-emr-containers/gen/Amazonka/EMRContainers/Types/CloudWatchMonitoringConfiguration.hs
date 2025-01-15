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
-- Module      : Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration for CloudWatch monitoring. You can configure your jobs
-- to send log information to CloudWatch Logs.
--
-- /See:/ 'newCloudWatchMonitoringConfiguration' smart constructor.
data CloudWatchMonitoringConfiguration = CloudWatchMonitoringConfiguration'
  { -- | The specified name prefix for log streams.
    logStreamNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group for log publishing.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamNamePrefix', 'cloudWatchMonitoringConfiguration_logStreamNamePrefix' - The specified name prefix for log streams.
--
-- 'logGroupName', 'cloudWatchMonitoringConfiguration_logGroupName' - The name of the log group for log publishing.
newCloudWatchMonitoringConfiguration ::
  -- | 'logGroupName'
  Prelude.Text ->
  CloudWatchMonitoringConfiguration
newCloudWatchMonitoringConfiguration pLogGroupName_ =
  CloudWatchMonitoringConfiguration'
    { logStreamNamePrefix =
        Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The specified name prefix for log streams.
cloudWatchMonitoringConfiguration_logStreamNamePrefix :: Lens.Lens' CloudWatchMonitoringConfiguration (Prelude.Maybe Prelude.Text)
cloudWatchMonitoringConfiguration_logStreamNamePrefix = Lens.lens (\CloudWatchMonitoringConfiguration' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@CloudWatchMonitoringConfiguration' {} a -> s {logStreamNamePrefix = a} :: CloudWatchMonitoringConfiguration)

-- | The name of the log group for log publishing.
cloudWatchMonitoringConfiguration_logGroupName :: Lens.Lens' CloudWatchMonitoringConfiguration Prelude.Text
cloudWatchMonitoringConfiguration_logGroupName = Lens.lens (\CloudWatchMonitoringConfiguration' {logGroupName} -> logGroupName) (\s@CloudWatchMonitoringConfiguration' {} a -> s {logGroupName = a} :: CloudWatchMonitoringConfiguration)

instance
  Data.FromJSON
    CloudWatchMonitoringConfiguration
  where
  parseJSON =
    Data.withObject
      "CloudWatchMonitoringConfiguration"
      ( \x ->
          CloudWatchMonitoringConfiguration'
            Prelude.<$> (x Data..:? "logStreamNamePrefix")
            Prelude.<*> (x Data..: "logGroupName")
      )

instance
  Prelude.Hashable
    CloudWatchMonitoringConfiguration
  where
  hashWithSalt
    _salt
    CloudWatchMonitoringConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` logStreamNamePrefix
        `Prelude.hashWithSalt` logGroupName

instance
  Prelude.NFData
    CloudWatchMonitoringConfiguration
  where
  rnf CloudWatchMonitoringConfiguration' {..} =
    Prelude.rnf logStreamNamePrefix `Prelude.seq`
      Prelude.rnf logGroupName

instance
  Data.ToJSON
    CloudWatchMonitoringConfiguration
  where
  toJSON CloudWatchMonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logStreamNamePrefix" Data..=)
              Prelude.<$> logStreamNamePrefix,
            Prelude.Just ("logGroupName" Data..= logGroupName)
          ]
      )
