{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CloudWatchOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CloudWatchOutputConfig
  ( CloudWatchOutputConfig (..),

    -- * Smart constructor
    mkCloudWatchOutputConfig,

    -- * Lenses
    cwocCloudWatchLogGroupName,
    cwocCloudWatchOutputEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.CloudWatchLogGroupName as Types

-- | Configuration options for sending command output to CloudWatch Logs.
--
-- /See:/ 'mkCloudWatchOutputConfig' smart constructor.
data CloudWatchOutputConfig = CloudWatchOutputConfig'
  { -- | The name of the CloudWatch log group where you want to send command output. If you don't specify a group name, Systems Manager automatically creates a log group for you. The log group uses the following naming format: aws/ssm//SystemsManagerDocumentName/ .
    cloudWatchLogGroupName :: Core.Maybe Types.CloudWatchLogGroupName,
    -- | Enables Systems Manager to send command output to CloudWatch Logs.
    cloudWatchOutputEnabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchOutputConfig' value with any optional fields omitted.
mkCloudWatchOutputConfig ::
  CloudWatchOutputConfig
mkCloudWatchOutputConfig =
  CloudWatchOutputConfig'
    { cloudWatchLogGroupName = Core.Nothing,
      cloudWatchOutputEnabled = Core.Nothing
    }

-- | The name of the CloudWatch log group where you want to send command output. If you don't specify a group name, Systems Manager automatically creates a log group for you. The log group uses the following naming format: aws/ssm//SystemsManagerDocumentName/ .
--
-- /Note:/ Consider using 'cloudWatchLogGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwocCloudWatchLogGroupName :: Lens.Lens' CloudWatchOutputConfig (Core.Maybe Types.CloudWatchLogGroupName)
cwocCloudWatchLogGroupName = Lens.field @"cloudWatchLogGroupName"
{-# DEPRECATED cwocCloudWatchLogGroupName "Use generic-lens or generic-optics with 'cloudWatchLogGroupName' instead." #-}

-- | Enables Systems Manager to send command output to CloudWatch Logs.
--
-- /Note:/ Consider using 'cloudWatchOutputEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwocCloudWatchOutputEnabled :: Lens.Lens' CloudWatchOutputConfig (Core.Maybe Core.Bool)
cwocCloudWatchOutputEnabled = Lens.field @"cloudWatchOutputEnabled"
{-# DEPRECATED cwocCloudWatchOutputEnabled "Use generic-lens or generic-optics with 'cloudWatchOutputEnabled' instead." #-}

instance Core.FromJSON CloudWatchOutputConfig where
  toJSON CloudWatchOutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchLogGroupName" Core..=)
              Core.<$> cloudWatchLogGroupName,
            ("CloudWatchOutputEnabled" Core..=)
              Core.<$> cloudWatchOutputEnabled
          ]
      )

instance Core.FromJSON CloudWatchOutputConfig where
  parseJSON =
    Core.withObject "CloudWatchOutputConfig" Core.$
      \x ->
        CloudWatchOutputConfig'
          Core.<$> (x Core..:? "CloudWatchLogGroupName")
          Core.<*> (x Core..:? "CloudWatchOutputEnabled")
