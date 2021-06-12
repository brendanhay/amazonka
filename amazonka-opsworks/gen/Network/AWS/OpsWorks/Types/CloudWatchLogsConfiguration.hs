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
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
-- /See:/ 'newCloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { -- | Whether CloudWatch Logs is enabled for a layer.
    enabled :: Core.Maybe Core.Bool,
    -- | A list of configuration options for CloudWatch Logs.
    logStreams :: Core.Maybe [CloudWatchLogsLogStream]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudWatchLogsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'cloudWatchLogsConfiguration_enabled' - Whether CloudWatch Logs is enabled for a layer.
--
-- 'logStreams', 'cloudWatchLogsConfiguration_logStreams' - A list of configuration options for CloudWatch Logs.
newCloudWatchLogsConfiguration ::
  CloudWatchLogsConfiguration
newCloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    { enabled =
        Core.Nothing,
      logStreams = Core.Nothing
    }

-- | Whether CloudWatch Logs is enabled for a layer.
cloudWatchLogsConfiguration_enabled :: Lens.Lens' CloudWatchLogsConfiguration (Core.Maybe Core.Bool)
cloudWatchLogsConfiguration_enabled = Lens.lens (\CloudWatchLogsConfiguration' {enabled} -> enabled) (\s@CloudWatchLogsConfiguration' {} a -> s {enabled = a} :: CloudWatchLogsConfiguration)

-- | A list of configuration options for CloudWatch Logs.
cloudWatchLogsConfiguration_logStreams :: Lens.Lens' CloudWatchLogsConfiguration (Core.Maybe [CloudWatchLogsLogStream])
cloudWatchLogsConfiguration_logStreams = Lens.lens (\CloudWatchLogsConfiguration' {logStreams} -> logStreams) (\s@CloudWatchLogsConfiguration' {} a -> s {logStreams = a} :: CloudWatchLogsConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    Core.withObject
      "CloudWatchLogsConfiguration"
      ( \x ->
          CloudWatchLogsConfiguration'
            Core.<$> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "LogStreams" Core..!= Core.mempty)
      )

instance Core.Hashable CloudWatchLogsConfiguration

instance Core.NFData CloudWatchLogsConfiguration

instance Core.ToJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("LogStreams" Core..=) Core.<$> logStreams
          ]
      )
