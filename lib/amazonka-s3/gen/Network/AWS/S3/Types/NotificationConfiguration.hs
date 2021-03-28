{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.NotificationConfiguration
  ( NotificationConfiguration (..)
  -- * Smart constructor
  , mkNotificationConfiguration
  -- * Lenses
  , ncLambdaFunctionConfigurations
  , ncQueueConfigurations
  , ncTopicConfigurations
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.LambdaFunctionConfiguration as Types
import qualified Network.AWS.S3.Types.QueueConfiguration as Types
import qualified Network.AWS.S3.Types.TopicConfiguration as Types

-- | A container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off for the bucket.
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { lambdaFunctionConfigurations :: Core.Maybe [Types.LambdaFunctionConfiguration]
    -- ^ Describes the AWS Lambda functions to invoke and the events for which to invoke them.
  , queueConfigurations :: Core.Maybe [Types.QueueConfiguration]
    -- ^ The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
  , topicConfigurations :: Core.Maybe [Types.TopicConfiguration]
    -- ^ The topic to which notifications are sent and the events for which notifications are generated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationConfiguration' value with any optional fields omitted.
mkNotificationConfiguration
    :: NotificationConfiguration
mkNotificationConfiguration
  = NotificationConfiguration'{lambdaFunctionConfigurations =
                                 Core.Nothing,
                               queueConfigurations = Core.Nothing,
                               topicConfigurations = Core.Nothing}

-- | Describes the AWS Lambda functions to invoke and the events for which to invoke them.
--
-- /Note:/ Consider using 'lambdaFunctionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncLambdaFunctionConfigurations :: Lens.Lens' NotificationConfiguration (Core.Maybe [Types.LambdaFunctionConfiguration])
ncLambdaFunctionConfigurations = Lens.field @"lambdaFunctionConfigurations"
{-# INLINEABLE ncLambdaFunctionConfigurations #-}
{-# DEPRECATED lambdaFunctionConfigurations "Use generic-lens or generic-optics with 'lambdaFunctionConfigurations' instead"  #-}

-- | The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
--
-- /Note:/ Consider using 'queueConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncQueueConfigurations :: Lens.Lens' NotificationConfiguration (Core.Maybe [Types.QueueConfiguration])
ncQueueConfigurations = Lens.field @"queueConfigurations"
{-# INLINEABLE ncQueueConfigurations #-}
{-# DEPRECATED queueConfigurations "Use generic-lens or generic-optics with 'queueConfigurations' instead"  #-}

-- | The topic to which notifications are sent and the events for which notifications are generated.
--
-- /Note:/ Consider using 'topicConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicConfigurations :: Lens.Lens' NotificationConfiguration (Core.Maybe [Types.TopicConfiguration])
ncTopicConfigurations = Lens.field @"topicConfigurations"
{-# INLINEABLE ncTopicConfigurations #-}
{-# DEPRECATED topicConfigurations "Use generic-lens or generic-optics with 'topicConfigurations' instead"  #-}

instance Core.ToXML NotificationConfiguration where
        toXML NotificationConfiguration{..}
          = Core.maybe Core.mempty
              (Core.toXMLList "CloudFunctionConfiguration")
              lambdaFunctionConfigurations
              Core.<>
              Core.maybe Core.mempty (Core.toXMLList "QueueConfiguration")
                queueConfigurations
              Core.<>
              Core.maybe Core.mempty (Core.toXMLList "TopicConfiguration")
                topicConfigurations

instance Core.FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' Core.<$>
              (x Core..@? "CloudFunctionConfiguration") Core.<*>
                x Core..@? "QueueConfiguration"
                Core.<*> x Core..@? "TopicConfiguration"
