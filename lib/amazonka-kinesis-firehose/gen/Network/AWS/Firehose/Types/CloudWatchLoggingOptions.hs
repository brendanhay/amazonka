{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CloudWatchLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CloudWatchLoggingOptions
  ( CloudWatchLoggingOptions (..),

    -- * Smart constructor
    mkCloudWatchLoggingOptions,

    -- * Lenses
    cwloEnabled,
    cwloLogGroupName,
    cwloLogStreamName,
  )
where

import qualified Network.AWS.Firehose.Types.LogGroupName as Types
import qualified Network.AWS.Firehose.Types.LogStreamName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon CloudWatch logging options for your delivery stream.
--
-- /See:/ 'mkCloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
  { -- | Enables or disables CloudWatch logging.
    enabled :: Core.Maybe Core.Bool,
    -- | The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
    logGroupName :: Core.Maybe Types.LogGroupName,
    -- | The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
    logStreamName :: Core.Maybe Types.LogStreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLoggingOptions' value with any optional fields omitted.
mkCloudWatchLoggingOptions ::
  CloudWatchLoggingOptions
mkCloudWatchLoggingOptions =
  CloudWatchLoggingOptions'
    { enabled = Core.Nothing,
      logGroupName = Core.Nothing,
      logStreamName = Core.Nothing
    }

-- | Enables or disables CloudWatch logging.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloEnabled :: Lens.Lens' CloudWatchLoggingOptions (Core.Maybe Core.Bool)
cwloEnabled = Lens.field @"enabled"
{-# DEPRECATED cwloEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloLogGroupName :: Lens.Lens' CloudWatchLoggingOptions (Core.Maybe Types.LogGroupName)
cwloLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED cwloLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloLogStreamName :: Lens.Lens' CloudWatchLoggingOptions (Core.Maybe Types.LogStreamName)
cwloLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED cwloLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Core.FromJSON CloudWatchLoggingOptions where
  toJSON CloudWatchLoggingOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("LogGroupName" Core..=) Core.<$> logGroupName,
            ("LogStreamName" Core..=) Core.<$> logStreamName
          ]
      )

instance Core.FromJSON CloudWatchLoggingOptions where
  parseJSON =
    Core.withObject "CloudWatchLoggingOptions" Core.$
      \x ->
        CloudWatchLoggingOptions'
          Core.<$> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "LogGroupName")
          Core.<*> (x Core..:? "LogStreamName")
