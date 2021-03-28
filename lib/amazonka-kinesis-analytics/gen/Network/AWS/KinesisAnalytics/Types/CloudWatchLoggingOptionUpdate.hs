{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
  ( CloudWatchLoggingOptionUpdate (..)
  -- * Smart constructor
  , mkCloudWatchLoggingOptionUpdate
  -- * Lenses
  , cwlouCloudWatchLoggingOptionId
  , cwlouLogStreamARNUpdate
  , cwlouRoleARNUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionId as Types
import qualified Network.AWS.KinesisAnalytics.Types.LogStreamARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARNUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes CloudWatch logging option updates.
--
-- /See:/ 'mkCloudWatchLoggingOptionUpdate' smart constructor.
data CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate'
  { cloudWatchLoggingOptionId :: Types.CloudWatchLoggingOptionId
    -- ^ ID of the CloudWatch logging option to update
  , logStreamARNUpdate :: Core.Maybe Types.LogStreamARN
    -- ^ ARN of the CloudWatch log to receive application messages.
  , roleARNUpdate :: Core.Maybe Types.RoleARNUpdate
    -- ^ IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLoggingOptionUpdate' value with any optional fields omitted.
mkCloudWatchLoggingOptionUpdate
    :: Types.CloudWatchLoggingOptionId -- ^ 'cloudWatchLoggingOptionId'
    -> CloudWatchLoggingOptionUpdate
mkCloudWatchLoggingOptionUpdate cloudWatchLoggingOptionId
  = CloudWatchLoggingOptionUpdate'{cloudWatchLoggingOptionId,
                                   logStreamARNUpdate = Core.Nothing, roleARNUpdate = Core.Nothing}

-- | ID of the CloudWatch logging option to update
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlouCloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionUpdate Types.CloudWatchLoggingOptionId
cwlouCloudWatchLoggingOptionId = Lens.field @"cloudWatchLoggingOptionId"
{-# INLINEABLE cwlouCloudWatchLoggingOptionId #-}
{-# DEPRECATED cloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead"  #-}

-- | ARN of the CloudWatch log to receive application messages.
--
-- /Note:/ Consider using 'logStreamARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlouLogStreamARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Core.Maybe Types.LogStreamARN)
cwlouLogStreamARNUpdate = Lens.field @"logStreamARNUpdate"
{-# INLINEABLE cwlouLogStreamARNUpdate #-}
{-# DEPRECATED logStreamARNUpdate "Use generic-lens or generic-optics with 'logStreamARNUpdate' instead"  #-}

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlouRoleARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Core.Maybe Types.RoleARNUpdate)
cwlouRoleARNUpdate = Lens.field @"roleARNUpdate"
{-# INLINEABLE cwlouRoleARNUpdate #-}
{-# DEPRECATED roleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead"  #-}

instance Core.FromJSON CloudWatchLoggingOptionUpdate where
        toJSON CloudWatchLoggingOptionUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CloudWatchLoggingOptionId" Core..= cloudWatchLoggingOptionId),
                  ("LogStreamARNUpdate" Core..=) Core.<$> logStreamARNUpdate,
                  ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate])
