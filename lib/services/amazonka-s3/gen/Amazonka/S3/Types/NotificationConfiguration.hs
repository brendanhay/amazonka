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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.NotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | Describes the Lambda functions to invoke and the events for which to
    -- invoke them.
    lambdaFunctionConfigurations :: Prelude.Maybe [LambdaFunctionConfiguration],
    -- | The Amazon Simple Queue Service queues to publish messages to and the
    -- events for which to publish messages.
    queueConfigurations :: Prelude.Maybe [QueueConfiguration],
    -- | The topic to which notifications are sent and the events for which
    -- notifications are generated.
    topicConfigurations :: Prelude.Maybe [TopicConfiguration]
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
-- 'lambdaFunctionConfigurations', 'notificationConfiguration_lambdaFunctionConfigurations' - Describes the Lambda functions to invoke and the events for which to
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
    { eventBridgeConfiguration =
        Prelude.Nothing,
      lambdaFunctionConfigurations = Prelude.Nothing,
      queueConfigurations = Prelude.Nothing,
      topicConfigurations = Prelude.Nothing
    }

-- | Enables delivery of events to Amazon EventBridge.
notificationConfiguration_eventBridgeConfiguration :: Lens.Lens' NotificationConfiguration (Prelude.Maybe EventBridgeConfiguration)
notificationConfiguration_eventBridgeConfiguration = Lens.lens (\NotificationConfiguration' {eventBridgeConfiguration} -> eventBridgeConfiguration) (\s@NotificationConfiguration' {} a -> s {eventBridgeConfiguration = a} :: NotificationConfiguration)

-- | Describes the Lambda functions to invoke and the events for which to
-- invoke them.
notificationConfiguration_lambdaFunctionConfigurations :: Lens.Lens' NotificationConfiguration (Prelude.Maybe [LambdaFunctionConfiguration])
notificationConfiguration_lambdaFunctionConfigurations = Lens.lens (\NotificationConfiguration' {lambdaFunctionConfigurations} -> lambdaFunctionConfigurations) (\s@NotificationConfiguration' {} a -> s {lambdaFunctionConfigurations = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Simple Queue Service queues to publish messages to and the
-- events for which to publish messages.
notificationConfiguration_queueConfigurations :: Lens.Lens' NotificationConfiguration (Prelude.Maybe [QueueConfiguration])
notificationConfiguration_queueConfigurations = Lens.lens (\NotificationConfiguration' {queueConfigurations} -> queueConfigurations) (\s@NotificationConfiguration' {} a -> s {queueConfigurations = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The topic to which notifications are sent and the events for which
-- notifications are generated.
notificationConfiguration_topicConfigurations :: Lens.Lens' NotificationConfiguration (Prelude.Maybe [TopicConfiguration])
notificationConfiguration_topicConfigurations = Lens.lens (\NotificationConfiguration' {topicConfigurations} -> topicConfigurations) (\s@NotificationConfiguration' {} a -> s {topicConfigurations = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Prelude.<$> (x Data..@? "EventBridgeConfiguration")
      Prelude.<*> ( Core.may
                      (Data.parseXMLList "CloudFunctionConfiguration")
                      x
                  )
      Prelude.<*> (Core.may (Data.parseXMLList "QueueConfiguration") x)
      Prelude.<*> (Core.may (Data.parseXMLList "TopicConfiguration") x)

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` eventBridgeConfiguration
      `Prelude.hashWithSalt` lambdaFunctionConfigurations
      `Prelude.hashWithSalt` queueConfigurations
      `Prelude.hashWithSalt` topicConfigurations

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf eventBridgeConfiguration `Prelude.seq`
      Prelude.rnf lambdaFunctionConfigurations `Prelude.seq`
        Prelude.rnf queueConfigurations `Prelude.seq`
          Prelude.rnf topicConfigurations

instance Data.ToXML NotificationConfiguration where
  toXML NotificationConfiguration' {..} =
    Prelude.mconcat
      [ "EventBridgeConfiguration"
          Data.@= eventBridgeConfiguration,
        Data.toXML
          ( Data.toXMLList "CloudFunctionConfiguration"
              Prelude.<$> lambdaFunctionConfigurations
          ),
        Data.toXML
          ( Data.toXMLList "QueueConfiguration"
              Prelude.<$> queueConfigurations
          ),
        Data.toXML
          ( Data.toXMLList "TopicConfiguration"
              Prelude.<$> topicConfigurations
          )
      ]
