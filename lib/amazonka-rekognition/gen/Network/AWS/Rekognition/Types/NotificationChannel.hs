{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.NotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.NotificationChannel
  ( NotificationChannel (..)
  -- * Smart constructor
  , mkNotificationChannel
  -- * Lenses
  , ncSNSTopicArn
  , ncRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.RoleArn as Types
import qualified Network.AWS.Rekognition.Types.SNSTopicArn as Types

-- | The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see 'api-video' .
--
-- /See:/ 'mkNotificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { sNSTopicArn :: Types.SNSTopicArn
    -- ^ The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
  , roleArn :: Types.RoleArn
    -- ^ The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationChannel' value with any optional fields omitted.
mkNotificationChannel
    :: Types.SNSTopicArn -- ^ 'sNSTopicArn'
    -> Types.RoleArn -- ^ 'roleArn'
    -> NotificationChannel
mkNotificationChannel sNSTopicArn roleArn
  = NotificationChannel'{sNSTopicArn, roleArn}

-- | The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
--
-- /Note:/ Consider using 'sNSTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncSNSTopicArn :: Lens.Lens' NotificationChannel Types.SNSTopicArn
ncSNSTopicArn = Lens.field @"sNSTopicArn"
{-# INLINEABLE ncSNSTopicArn #-}
{-# DEPRECATED sNSTopicArn "Use generic-lens or generic-optics with 'sNSTopicArn' instead"  #-}

-- | The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic. 
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncRoleArn :: Lens.Lens' NotificationChannel Types.RoleArn
ncRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ncRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON NotificationChannel where
        toJSON NotificationChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SNSTopicArn" Core..= sNSTopicArn),
                  Core.Just ("RoleArn" Core..= roleArn)])
