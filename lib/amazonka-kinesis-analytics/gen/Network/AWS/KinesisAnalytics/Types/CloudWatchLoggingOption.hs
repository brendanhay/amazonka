{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
  ( CloudWatchLoggingOption (..)
  -- * Smart constructor
  , mkCloudWatchLoggingOption
  -- * Lenses
  , cwloLogStreamARN
  , cwloRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.LogStreamARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.
--
-- /See:/ 'mkCloudWatchLoggingOption' smart constructor.
data CloudWatchLoggingOption = CloudWatchLoggingOption'
  { logStreamARN :: Types.LogStreamARN
    -- ^ ARN of the CloudWatch log to receive application messages.
  , roleARN :: Types.RoleARN
    -- ^ IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLoggingOption' value with any optional fields omitted.
mkCloudWatchLoggingOption
    :: Types.LogStreamARN -- ^ 'logStreamARN'
    -> Types.RoleARN -- ^ 'roleARN'
    -> CloudWatchLoggingOption
mkCloudWatchLoggingOption logStreamARN roleARN
  = CloudWatchLoggingOption'{logStreamARN, roleARN}

-- | ARN of the CloudWatch log to receive application messages.
--
-- /Note:/ Consider using 'logStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloLogStreamARN :: Lens.Lens' CloudWatchLoggingOption Types.LogStreamARN
cwloLogStreamARN = Lens.field @"logStreamARN"
{-# INLINEABLE cwloLogStreamARN #-}
{-# DEPRECATED logStreamARN "Use generic-lens or generic-optics with 'logStreamARN' instead"  #-}

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloRoleARN :: Lens.Lens' CloudWatchLoggingOption Types.RoleARN
cwloRoleARN = Lens.field @"roleARN"
{-# INLINEABLE cwloRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON CloudWatchLoggingOption where
        toJSON CloudWatchLoggingOption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LogStreamARN" Core..= logStreamARN),
                  Core.Just ("RoleARN" Core..= roleARN)])
