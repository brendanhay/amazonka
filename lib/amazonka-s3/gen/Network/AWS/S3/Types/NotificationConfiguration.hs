{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NotificationConfiguration
  ( NotificationConfiguration (..),

    -- * Smart constructor
    mkNotificationConfiguration,

    -- * Lenses
    ncQueueConfigurations,
    ncTopicConfigurations,
    ncLambdaFunctionConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LambdaFunctionConfiguration
import Network.AWS.S3.Types.QueueConfiguration
import Network.AWS.S3.Types.TopicConfiguration

-- | A container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off for the bucket.
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
    queueConfigurations :: Lude.Maybe [QueueConfiguration],
    -- | The topic to which notifications are sent and the events for which notifications are generated.
    topicConfigurations :: Lude.Maybe [TopicConfiguration],
    -- | Describes the AWS Lambda functions to invoke and the events for which to invoke them.
    lambdaFunctionConfigurations :: Lude.Maybe [LambdaFunctionConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'queueConfigurations' - The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
-- * 'topicConfigurations' - The topic to which notifications are sent and the events for which notifications are generated.
-- * 'lambdaFunctionConfigurations' - Describes the AWS Lambda functions to invoke and the events for which to invoke them.
mkNotificationConfiguration ::
  NotificationConfiguration
mkNotificationConfiguration =
  NotificationConfiguration'
    { queueConfigurations = Lude.Nothing,
      topicConfigurations = Lude.Nothing,
      lambdaFunctionConfigurations = Lude.Nothing
    }

-- | The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
--
-- /Note:/ Consider using 'queueConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncQueueConfigurations :: Lens.Lens' NotificationConfiguration (Lude.Maybe [QueueConfiguration])
ncQueueConfigurations = Lens.lens (queueConfigurations :: NotificationConfiguration -> Lude.Maybe [QueueConfiguration]) (\s a -> s {queueConfigurations = a} :: NotificationConfiguration)
{-# DEPRECATED ncQueueConfigurations "Use generic-lens or generic-optics with 'queueConfigurations' instead." #-}

-- | The topic to which notifications are sent and the events for which notifications are generated.
--
-- /Note:/ Consider using 'topicConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicConfigurations :: Lens.Lens' NotificationConfiguration (Lude.Maybe [TopicConfiguration])
ncTopicConfigurations = Lens.lens (topicConfigurations :: NotificationConfiguration -> Lude.Maybe [TopicConfiguration]) (\s a -> s {topicConfigurations = a} :: NotificationConfiguration)
{-# DEPRECATED ncTopicConfigurations "Use generic-lens or generic-optics with 'topicConfigurations' instead." #-}

-- | Describes the AWS Lambda functions to invoke and the events for which to invoke them.
--
-- /Note:/ Consider using 'lambdaFunctionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncLambdaFunctionConfigurations :: Lens.Lens' NotificationConfiguration (Lude.Maybe [LambdaFunctionConfiguration])
ncLambdaFunctionConfigurations = Lens.lens (lambdaFunctionConfigurations :: NotificationConfiguration -> Lude.Maybe [LambdaFunctionConfiguration]) (\s a -> s {lambdaFunctionConfigurations = a} :: NotificationConfiguration)
{-# DEPRECATED ncLambdaFunctionConfigurations "Use generic-lens or generic-optics with 'lambdaFunctionConfigurations' instead." #-}

instance Lude.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Lude.<$> (Lude.may (Lude.parseXMLList "QueueConfiguration") x)
      Lude.<*> (Lude.may (Lude.parseXMLList "TopicConfiguration") x)
      Lude.<*> (Lude.may (Lude.parseXMLList "CloudFunctionConfiguration") x)

instance Lude.ToXML NotificationConfiguration where
  toXML NotificationConfiguration' {..} =
    Lude.mconcat
      [ Lude.toXML
          (Lude.toXMLList "QueueConfiguration" Lude.<$> queueConfigurations),
        Lude.toXML
          (Lude.toXMLList "TopicConfiguration" Lude.<$> topicConfigurations),
        Lude.toXML
          ( Lude.toXMLList "CloudFunctionConfiguration"
              Lude.<$> lambdaFunctionConfigurations
          )
      ]
