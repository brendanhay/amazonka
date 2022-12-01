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
-- Module      : Amazonka.S3.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.NotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.EventBridgeConfiguration
import Amazonka.S3.Types.LambdaFunctionConfiguration
import Amazonka.S3.Types.QueueConfiguration
import Amazonka.S3.Types.TopicConfiguration

-- | A container for specifying the notification configuration of the bucket.
-- If this element is empty, notifications are turned off for the bucket.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | Enables delivery of events to Amazon EventBridge.
    eventBridgeConfiguration :: Prelude.Maybe EventBridgeConfiguration,
    -- | The Amazon Simple Queue Service queues to publish messages to and the
    -- events for which to publish messages.
    queueConfigurations :: Prelude.Maybe [QueueConfiguration],
    -- | The topic to which notifications are sent and the events for which
    -- notifications are generated.
    topicConfigurations :: Prelude.Maybe [TopicConfiguration],
    -- | Describes the Lambda functions to invoke and the events for which to
    -- invoke them.
    lambdaFunctionConfigurations :: Prelude.Maybe [LambdaFunctionConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBridgeConfiguration', 'notificationConfiguration_eventBridgeConfiguration' - Enables delivery of events to Amazon EventBridge.
--
-- 'queueConfigurations', 'notificationConfiguration_queueConfigurations' - The Amazon Simple Queue Service queues to publish messages to and the
-- events for which to publish messages.
--
-- 'topicConfigurations', 'notificationConfiguration_topicConfigurations' - The topic to which notifications are sent and the events for which
-- notifications are generated.
--
-- 'lambdaFunctionConfigurations', 'notificationConfiguration_lambdaFunctionConfigurations' - Describes the Lambda functions to invoke and the events for which to
-- invoke them.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { eventBridgeConfiguration =
        Prelude.Nothing,
      queueConfigurations = Prelude.Nothing,
      topicConfigurations = Prelude.Nothing,
      lambdaFunctionConfigurations = Prelude.Nothing
    }

-- | Enables delivery of events to Amazon EventBridge.
notificationConfiguration_eventBridgeConfiguration :: Lens.Lens' NotificationConfiguration (Prelude.Maybe EventBridgeConfiguration)
notificationConfiguration_eventBridgeConfiguration = Lens.lens (\NotificationConfiguration' {eventBridgeConfiguration} -> eventBridgeConfiguration) (\s@NotificationConfiguration' {} a -> s {eventBridgeConfiguration = a} :: NotificationConfiguration)

-- | The Amazon Simple Queue Service queues to publish messages to and the
-- events for which to publish messages.
notificationConfiguration_queueConfigurations :: Lens.Lens' NotificationConfiguration (Prelude.Maybe [QueueConfiguration])
notificationConfiguration_queueConfigurations = Lens.lens (\NotificationConfiguration' {queueConfigurations} -> queueConfigurations) (\s@NotificationConfiguration' {} a -> s {queueConfigurations = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The topic to which notifications are sent and the events for which
-- notifications are generated.
notificationConfiguration_topicConfigurations :: Lens.Lens' NotificationConfiguration (Prelude.Maybe [TopicConfiguration])
notificationConfiguration_topicConfigurations = Lens.lens (\NotificationConfiguration' {topicConfigurations} -> topicConfigurations) (\s@NotificationConfiguration' {} a -> s {topicConfigurations = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Describes the Lambda functions to invoke and the events for which to
-- invoke them.
notificationConfiguration_lambdaFunctionConfigurations :: Lens.Lens' NotificationConfiguration (Prelude.Maybe [LambdaFunctionConfiguration])
notificationConfiguration_lambdaFunctionConfigurations = Lens.lens (\NotificationConfiguration' {lambdaFunctionConfigurations} -> lambdaFunctionConfigurations) (\s@NotificationConfiguration' {} a -> s {lambdaFunctionConfigurations = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Prelude.<$> (x Core..@? "EventBridgeConfiguration")
      Prelude.<*> (Core.may (Core.parseXMLList "QueueConfiguration") x)
      Prelude.<*> (Core.may (Core.parseXMLList "TopicConfiguration") x)
      Prelude.<*> ( Core.may
                      (Core.parseXMLList "CloudFunctionConfiguration")
                      x
                  )

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` eventBridgeConfiguration
      `Prelude.hashWithSalt` queueConfigurations
      `Prelude.hashWithSalt` topicConfigurations
      `Prelude.hashWithSalt` lambdaFunctionConfigurations

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf eventBridgeConfiguration
      `Prelude.seq` Prelude.rnf queueConfigurations
      `Prelude.seq` Prelude.rnf topicConfigurations
      `Prelude.seq` Prelude.rnf lambdaFunctionConfigurations

instance Core.ToXML NotificationConfiguration where
  toXML NotificationConfiguration' {..} =
    Prelude.mconcat
      [ "EventBridgeConfiguration"
          Core.@= eventBridgeConfiguration,
        Core.toXML
          ( Core.toXMLList "QueueConfiguration"
              Prelude.<$> queueConfigurations
          ),
        Core.toXML
          ( Core.toXMLList "TopicConfiguration"
              Prelude.<$> topicConfigurations
          ),
        Core.toXML
          ( Core.toXMLList "CloudFunctionConfiguration"
              Prelude.<$> lambdaFunctionConfigurations
          )
      ]
