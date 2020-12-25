{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogDestination
  ( LogDestination (..),

    -- * Smart constructor
    mkLogDestination,

    -- * Lenses
    ldCloudWatchLogsLogGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup as Types

-- |
--
-- /See:/ 'mkLogDestination' smart constructor.
newtype LogDestination = LogDestination'
  { -- | An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
    cloudWatchLogsLogGroup :: Core.Maybe Types.CloudWatchLogsLogGroup
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LogDestination' value with any optional fields omitted.
mkLogDestination ::
  LogDestination
mkLogDestination =
  LogDestination' {cloudWatchLogsLogGroup = Core.Nothing}

-- | An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCloudWatchLogsLogGroup :: Lens.Lens' LogDestination (Core.Maybe Types.CloudWatchLogsLogGroup)
ldCloudWatchLogsLogGroup = Lens.field @"cloudWatchLogsLogGroup"
{-# DEPRECATED ldCloudWatchLogsLogGroup "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroup' instead." #-}

instance Core.FromJSON LogDestination where
  toJSON LogDestination {..} =
    Core.object
      ( Core.catMaybes
          [ ("cloudWatchLogsLogGroup" Core..=)
              Core.<$> cloudWatchLogsLogGroup
          ]
      )

instance Core.FromJSON LogDestination where
  parseJSON =
    Core.withObject "LogDestination" Core.$
      \x ->
        LogDestination' Core.<$> (x Core..:? "cloudWatchLogsLogGroup")
