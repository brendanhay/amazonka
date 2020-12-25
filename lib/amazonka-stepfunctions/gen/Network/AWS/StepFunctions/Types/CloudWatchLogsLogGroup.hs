{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
  ( CloudWatchLogsLogGroup (..),

    -- * Smart constructor
    mkCloudWatchLogsLogGroup,

    -- * Lenses
    cwllgLogGroupArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.LogGroupArn as Types

-- |
--
-- /See:/ 'mkCloudWatchLogsLogGroup' smart constructor.
newtype CloudWatchLogsLogGroup = CloudWatchLogsLogGroup'
  { -- | The ARN of the the CloudWatch log group to which you want your logs emitted to. The ARN must end with @:*@
    logGroupArn :: Core.Maybe Types.LogGroupArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLogsLogGroup' value with any optional fields omitted.
mkCloudWatchLogsLogGroup ::
  CloudWatchLogsLogGroup
mkCloudWatchLogsLogGroup =
  CloudWatchLogsLogGroup' {logGroupArn = Core.Nothing}

-- | The ARN of the the CloudWatch log group to which you want your logs emitted to. The ARN must end with @:*@
--
-- /Note:/ Consider using 'logGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllgLogGroupArn :: Lens.Lens' CloudWatchLogsLogGroup (Core.Maybe Types.LogGroupArn)
cwllgLogGroupArn = Lens.field @"logGroupArn"
{-# DEPRECATED cwllgLogGroupArn "Use generic-lens or generic-optics with 'logGroupArn' instead." #-}

instance Core.FromJSON CloudWatchLogsLogGroup where
  toJSON CloudWatchLogsLogGroup {..} =
    Core.object
      (Core.catMaybes [("logGroupArn" Core..=) Core.<$> logGroupArn])

instance Core.FromJSON CloudWatchLogsLogGroup where
  parseJSON =
    Core.withObject "CloudWatchLogsLogGroup" Core.$
      \x -> CloudWatchLogsLogGroup' Core.<$> (x Core..:? "logGroupArn")
