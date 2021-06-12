{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NotificationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LambdaFunctionConfiguration
import Network.AWS.S3.Types.QueueConfiguration
import Network.AWS.S3.Types.TopicConfiguration

-- | A container for specifying the notification configuration of the bucket.
-- If this element is empty, notifications are turned off for the bucket.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | Describes the AWS Lambda functions to invoke and the events for which to
    -- invoke them.
    lambdaFunctionConfigurations :: Core.Maybe [LambdaFunctionConfiguration],
    -- | The Amazon Simple Queue Service queues to publish messages to and the
    -- events for which to publish messages.
    queueConfigurations :: Core.Maybe [QueueConfiguration],
    -- | The topic to which notifications are sent and the events for which
    -- notifications are generated.
    topicConfigurations :: Core.Maybe [TopicConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionConfigurations', 'notificationConfiguration_lambdaFunctionConfigurations' - Describes the AWS Lambda functions to invoke and the events for which to
-- invoke them.
--
-- 'queueConfigurations', 'notificationConfiguration_queueConfigurations' - The Amazon Simple Queue Service queues to publish messages to and the
-- events for which to publish messages.
--
-- 'topicConfigurations', 'notificationConfiguration_topicConfigurations' - The topic to which notifications are sent and the events for which
-- notifications are generated.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { lambdaFunctionConfigurations =
        Core.Nothing,
      queueConfigurations = Core.Nothing,
      topicConfigurations = Core.Nothing
    }

-- | Describes the AWS Lambda functions to invoke and the events for which to
-- invoke them.
notificationConfiguration_lambdaFunctionConfigurations :: Lens.Lens' NotificationConfiguration (Core.Maybe [LambdaFunctionConfiguration])
notificationConfiguration_lambdaFunctionConfigurations = Lens.lens (\NotificationConfiguration' {lambdaFunctionConfigurations} -> lambdaFunctionConfigurations) (\s@NotificationConfiguration' {} a -> s {lambdaFunctionConfigurations = a} :: NotificationConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Simple Queue Service queues to publish messages to and the
-- events for which to publish messages.
notificationConfiguration_queueConfigurations :: Lens.Lens' NotificationConfiguration (Core.Maybe [QueueConfiguration])
notificationConfiguration_queueConfigurations = Lens.lens (\NotificationConfiguration' {queueConfigurations} -> queueConfigurations) (\s@NotificationConfiguration' {} a -> s {queueConfigurations = a} :: NotificationConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The topic to which notifications are sent and the events for which
-- notifications are generated.
notificationConfiguration_topicConfigurations :: Lens.Lens' NotificationConfiguration (Core.Maybe [TopicConfiguration])
notificationConfiguration_topicConfigurations = Lens.lens (\NotificationConfiguration' {topicConfigurations} -> topicConfigurations) (\s@NotificationConfiguration' {} a -> s {topicConfigurations = a} :: NotificationConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Core.<$> ( Core.may
                   (Core.parseXMLList "CloudFunctionConfiguration")
                   x
               )
      Core.<*> (Core.may (Core.parseXMLList "QueueConfiguration") x)
      Core.<*> (Core.may (Core.parseXMLList "TopicConfiguration") x)

instance Core.Hashable NotificationConfiguration

instance Core.NFData NotificationConfiguration

instance Core.ToXML NotificationConfiguration where
  toXML NotificationConfiguration' {..} =
    Core.mconcat
      [ Core.toXML
          ( Core.toXMLList "CloudFunctionConfiguration"
              Core.<$> lambdaFunctionConfigurations
          ),
        Core.toXML
          ( Core.toXMLList "QueueConfiguration"
              Core.<$> queueConfigurations
          ),
        Core.toXML
          ( Core.toXMLList "TopicConfiguration"
              Core.<$> topicConfigurations
          )
      ]
