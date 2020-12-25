{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOption
  ( LogPublishingOption (..),

    -- * Smart constructor
    mkLogPublishingOption,

    -- * Lenses
    lpoCloudWatchLogsLogGroupArn,
    lpoEnabled,
  )
where

import qualified Network.AWS.ElasticSearch.Types.CloudWatchLogsLogGroupArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Log Publishing option that is set for given domain.
--
-- Attributes and their details:
--     * CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.
--
--     * Enabled: Whether the log publishing for given log type is enabled or not
--
--
--
-- /See:/ 'mkLogPublishingOption' smart constructor.
data LogPublishingOption = LogPublishingOption'
  { cloudWatchLogsLogGroupArn :: Core.Maybe Types.CloudWatchLogsLogGroupArn,
    -- | Specifies whether given log publishing option is enabled or not.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogPublishingOption' value with any optional fields omitted.
mkLogPublishingOption ::
  LogPublishingOption
mkLogPublishingOption =
  LogPublishingOption'
    { cloudWatchLogsLogGroupArn = Core.Nothing,
      enabled = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpoCloudWatchLogsLogGroupArn :: Lens.Lens' LogPublishingOption (Core.Maybe Types.CloudWatchLogsLogGroupArn)
lpoCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED lpoCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

-- | Specifies whether given log publishing option is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpoEnabled :: Lens.Lens' LogPublishingOption (Core.Maybe Core.Bool)
lpoEnabled = Lens.field @"enabled"
{-# DEPRECATED lpoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON LogPublishingOption where
  toJSON LogPublishingOption {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchLogsLogGroupArn" Core..=)
              Core.<$> cloudWatchLogsLogGroupArn,
            ("Enabled" Core..=) Core.<$> enabled
          ]
      )

instance Core.FromJSON LogPublishingOption where
  parseJSON =
    Core.withObject "LogPublishingOption" Core.$
      \x ->
        LogPublishingOption'
          Core.<$> (x Core..:? "CloudWatchLogsLogGroupArn")
          Core.<*> (x Core..:? "Enabled")
