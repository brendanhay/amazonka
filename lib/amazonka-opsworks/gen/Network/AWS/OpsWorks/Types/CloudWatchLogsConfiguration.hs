{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
  ( CloudWatchLogsConfiguration (..),

    -- * Smart constructor
    mkCloudWatchLogsConfiguration,

    -- * Lenses
    cwlcEnabled,
    cwlcLogStreams,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
-- /See:/ 'mkCloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { -- | Whether CloudWatch Logs is enabled for a layer.
    enabled :: Core.Maybe Core.Bool,
    -- | A list of configuration options for CloudWatch Logs.
    logStreams :: Core.Maybe [Types.CloudWatchLogsLogStream]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLogsConfiguration' value with any optional fields omitted.
mkCloudWatchLogsConfiguration ::
  CloudWatchLogsConfiguration
mkCloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    { enabled = Core.Nothing,
      logStreams = Core.Nothing
    }

-- | Whether CloudWatch Logs is enabled for a layer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcEnabled :: Lens.Lens' CloudWatchLogsConfiguration (Core.Maybe Core.Bool)
cwlcEnabled = Lens.field @"enabled"
{-# DEPRECATED cwlcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A list of configuration options for CloudWatch Logs.
--
-- /Note:/ Consider using 'logStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcLogStreams :: Lens.Lens' CloudWatchLogsConfiguration (Core.Maybe [Types.CloudWatchLogsLogStream])
cwlcLogStreams = Lens.field @"logStreams"
{-# DEPRECATED cwlcLogStreams "Use generic-lens or generic-optics with 'logStreams' instead." #-}

instance Core.FromJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("LogStreams" Core..=) Core.<$> logStreams
          ]
      )

instance Core.FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    Core.withObject "CloudWatchLogsConfiguration" Core.$
      \x ->
        CloudWatchLogsConfiguration'
          Core.<$> (x Core..:? "Enabled") Core.<*> (x Core..:? "LogStreams")
