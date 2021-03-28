{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
  ( CloudWatchLoggingOptionDescription (..)
  -- * Smart constructor
  , mkCloudWatchLoggingOptionDescription
  -- * Lenses
  , cwlodLogStreamARN
  , cwlodRoleARN
  , cwlodCloudWatchLoggingOptionId
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionId as Types
import qualified Network.AWS.KinesisAnalytics.Types.LogStreamARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Description of the CloudWatch logging option.
--
-- /See:/ 'mkCloudWatchLoggingOptionDescription' smart constructor.
data CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription'
  { logStreamARN :: Types.LogStreamARN
    -- ^ ARN of the CloudWatch log to receive application messages.
  , roleARN :: Types.RoleARN
    -- ^ IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
  , cloudWatchLoggingOptionId :: Core.Maybe Types.CloudWatchLoggingOptionId
    -- ^ ID of the CloudWatch logging option description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLoggingOptionDescription' value with any optional fields omitted.
mkCloudWatchLoggingOptionDescription
    :: Types.LogStreamARN -- ^ 'logStreamARN'
    -> Types.RoleARN -- ^ 'roleARN'
    -> CloudWatchLoggingOptionDescription
mkCloudWatchLoggingOptionDescription logStreamARN roleARN
  = CloudWatchLoggingOptionDescription'{logStreamARN, roleARN,
                                        cloudWatchLoggingOptionId = Core.Nothing}

-- | ARN of the CloudWatch log to receive application messages.
--
-- /Note:/ Consider using 'logStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlodLogStreamARN :: Lens.Lens' CloudWatchLoggingOptionDescription Types.LogStreamARN
cwlodLogStreamARN = Lens.field @"logStreamARN"
{-# INLINEABLE cwlodLogStreamARN #-}
{-# DEPRECATED logStreamARN "Use generic-lens or generic-optics with 'logStreamARN' instead"  #-}

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlodRoleARN :: Lens.Lens' CloudWatchLoggingOptionDescription Types.RoleARN
cwlodRoleARN = Lens.field @"roleARN"
{-# INLINEABLE cwlodRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | ID of the CloudWatch logging option description.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlodCloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionDescription (Core.Maybe Types.CloudWatchLoggingOptionId)
cwlodCloudWatchLoggingOptionId = Lens.field @"cloudWatchLoggingOptionId"
{-# INLINEABLE cwlodCloudWatchLoggingOptionId #-}
{-# DEPRECATED cloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead"  #-}

instance Core.FromJSON CloudWatchLoggingOptionDescription where
        parseJSON
          = Core.withObject "CloudWatchLoggingOptionDescription" Core.$
              \ x ->
                CloudWatchLoggingOptionDescription' Core.<$>
                  (x Core..: "LogStreamARN") Core.<*> x Core..: "RoleARN" Core.<*>
                    x Core..:? "CloudWatchLoggingOptionId"
