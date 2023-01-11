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
-- Module      : Amazonka.IoT.Types.MqttHeaders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MqttHeaders where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.UserProperty
import qualified Amazonka.Prelude as Prelude

-- | Specifies MQTT Version 5.0 headers information. For more information,
-- see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html MQTT>
-- from Amazon Web Services IoT Core Developer Guide.
--
-- /See:/ 'newMqttHeaders' smart constructor.
data MqttHeaders = MqttHeaders'
  { -- | A UTF-8 encoded string that describes the content of the publishing
    -- message.
    --
    -- For more information, see
    -- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901118 Content Type>
    -- from the MQTT Version 5.0 specification.
    --
    -- Supports
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded binary data used by the sender of the request message
    -- to identify which request the response message is for when it\'s
    -- received.
    --
    -- For more information, see
    -- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901115 Correlation Data>
    -- from the MQTT Version 5.0 specification.
    --
    -- This binary data must be based64-encoded.
    --
    -- Supports
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
    correlationData :: Prelude.Maybe Prelude.Text,
    -- | A user-defined integer value that will persist a message at the message
    -- broker for a specified amount of time to ensure that the message will
    -- expire if it\'s no longer relevant to the subscriber. The value of
    -- @messageExpiry@ represents the number of seconds before it expires. For
    -- more information about the limits of @messageExpiry@, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html Amazon Web Services IoT Core message broker and protocol limits and quotas>
    -- from the Amazon Web Services Reference Guide.
    --
    -- Supports
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
    messageExpiry :: Prelude.Maybe Prelude.Text,
    -- | An @Enum@ string value that indicates whether the payload is formatted
    -- as UTF-8.
    --
    -- Valid values are @UNSPECIFIED_BYTES@ and @UTF8_DATA@.
    --
    -- For more information, see
    -- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901111 Payload Format Indicator>
    -- from the MQTT Version 5.0 specification.
    --
    -- Supports
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
    payloadFormatIndicator :: Prelude.Maybe Prelude.Text,
    -- | A UTF-8 encoded string that\'s used as the topic name for a response
    -- message. The response topic is used to describe the topic which the
    -- receiver should publish to as part of the request-response flow. The
    -- topic must not contain wildcard characters.
    --
    -- For more information, see
    -- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901114 Response Topic>
    -- from the MQTT Version 5.0 specification.
    --
    -- Supports
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
    responseTopic :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs that you define in the MQTT5 header.
    userProperties :: Prelude.Maybe (Prelude.NonEmpty UserProperty)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MqttHeaders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'mqttHeaders_contentType' - A UTF-8 encoded string that describes the content of the publishing
-- message.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901118 Content Type>
-- from the MQTT Version 5.0 specification.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
--
-- 'correlationData', 'mqttHeaders_correlationData' - The base64-encoded binary data used by the sender of the request message
-- to identify which request the response message is for when it\'s
-- received.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901115 Correlation Data>
-- from the MQTT Version 5.0 specification.
--
-- This binary data must be based64-encoded.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
--
-- 'messageExpiry', 'mqttHeaders_messageExpiry' - A user-defined integer value that will persist a message at the message
-- broker for a specified amount of time to ensure that the message will
-- expire if it\'s no longer relevant to the subscriber. The value of
-- @messageExpiry@ represents the number of seconds before it expires. For
-- more information about the limits of @messageExpiry@, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html Amazon Web Services IoT Core message broker and protocol limits and quotas>
-- from the Amazon Web Services Reference Guide.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
--
-- 'payloadFormatIndicator', 'mqttHeaders_payloadFormatIndicator' - An @Enum@ string value that indicates whether the payload is formatted
-- as UTF-8.
--
-- Valid values are @UNSPECIFIED_BYTES@ and @UTF8_DATA@.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901111 Payload Format Indicator>
-- from the MQTT Version 5.0 specification.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
--
-- 'responseTopic', 'mqttHeaders_responseTopic' - A UTF-8 encoded string that\'s used as the topic name for a response
-- message. The response topic is used to describe the topic which the
-- receiver should publish to as part of the request-response flow. The
-- topic must not contain wildcard characters.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901114 Response Topic>
-- from the MQTT Version 5.0 specification.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
--
-- 'userProperties', 'mqttHeaders_userProperties' - An array of key-value pairs that you define in the MQTT5 header.
newMqttHeaders ::
  MqttHeaders
newMqttHeaders =
  MqttHeaders'
    { contentType = Prelude.Nothing,
      correlationData = Prelude.Nothing,
      messageExpiry = Prelude.Nothing,
      payloadFormatIndicator = Prelude.Nothing,
      responseTopic = Prelude.Nothing,
      userProperties = Prelude.Nothing
    }

-- | A UTF-8 encoded string that describes the content of the publishing
-- message.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901118 Content Type>
-- from the MQTT Version 5.0 specification.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
mqttHeaders_contentType :: Lens.Lens' MqttHeaders (Prelude.Maybe Prelude.Text)
mqttHeaders_contentType = Lens.lens (\MqttHeaders' {contentType} -> contentType) (\s@MqttHeaders' {} a -> s {contentType = a} :: MqttHeaders)

-- | The base64-encoded binary data used by the sender of the request message
-- to identify which request the response message is for when it\'s
-- received.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901115 Correlation Data>
-- from the MQTT Version 5.0 specification.
--
-- This binary data must be based64-encoded.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
mqttHeaders_correlationData :: Lens.Lens' MqttHeaders (Prelude.Maybe Prelude.Text)
mqttHeaders_correlationData = Lens.lens (\MqttHeaders' {correlationData} -> correlationData) (\s@MqttHeaders' {} a -> s {correlationData = a} :: MqttHeaders)

-- | A user-defined integer value that will persist a message at the message
-- broker for a specified amount of time to ensure that the message will
-- expire if it\'s no longer relevant to the subscriber. The value of
-- @messageExpiry@ represents the number of seconds before it expires. For
-- more information about the limits of @messageExpiry@, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html Amazon Web Services IoT Core message broker and protocol limits and quotas>
-- from the Amazon Web Services Reference Guide.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
mqttHeaders_messageExpiry :: Lens.Lens' MqttHeaders (Prelude.Maybe Prelude.Text)
mqttHeaders_messageExpiry = Lens.lens (\MqttHeaders' {messageExpiry} -> messageExpiry) (\s@MqttHeaders' {} a -> s {messageExpiry = a} :: MqttHeaders)

-- | An @Enum@ string value that indicates whether the payload is formatted
-- as UTF-8.
--
-- Valid values are @UNSPECIFIED_BYTES@ and @UTF8_DATA@.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901111 Payload Format Indicator>
-- from the MQTT Version 5.0 specification.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
mqttHeaders_payloadFormatIndicator :: Lens.Lens' MqttHeaders (Prelude.Maybe Prelude.Text)
mqttHeaders_payloadFormatIndicator = Lens.lens (\MqttHeaders' {payloadFormatIndicator} -> payloadFormatIndicator) (\s@MqttHeaders' {} a -> s {payloadFormatIndicator = a} :: MqttHeaders)

-- | A UTF-8 encoded string that\'s used as the topic name for a response
-- message. The response topic is used to describe the topic which the
-- receiver should publish to as part of the request-response flow. The
-- topic must not contain wildcard characters.
--
-- For more information, see
-- <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901114 Response Topic>
-- from the MQTT Version 5.0 specification.
--
-- Supports
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
mqttHeaders_responseTopic :: Lens.Lens' MqttHeaders (Prelude.Maybe Prelude.Text)
mqttHeaders_responseTopic = Lens.lens (\MqttHeaders' {responseTopic} -> responseTopic) (\s@MqttHeaders' {} a -> s {responseTopic = a} :: MqttHeaders)

-- | An array of key-value pairs that you define in the MQTT5 header.
mqttHeaders_userProperties :: Lens.Lens' MqttHeaders (Prelude.Maybe (Prelude.NonEmpty UserProperty))
mqttHeaders_userProperties = Lens.lens (\MqttHeaders' {userProperties} -> userProperties) (\s@MqttHeaders' {} a -> s {userProperties = a} :: MqttHeaders) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MqttHeaders where
  parseJSON =
    Data.withObject
      "MqttHeaders"
      ( \x ->
          MqttHeaders'
            Prelude.<$> (x Data..:? "contentType")
            Prelude.<*> (x Data..:? "correlationData")
            Prelude.<*> (x Data..:? "messageExpiry")
            Prelude.<*> (x Data..:? "payloadFormatIndicator")
            Prelude.<*> (x Data..:? "responseTopic")
            Prelude.<*> (x Data..:? "userProperties")
      )

instance Prelude.Hashable MqttHeaders where
  hashWithSalt _salt MqttHeaders' {..} =
    _salt `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` correlationData
      `Prelude.hashWithSalt` messageExpiry
      `Prelude.hashWithSalt` payloadFormatIndicator
      `Prelude.hashWithSalt` responseTopic
      `Prelude.hashWithSalt` userProperties

instance Prelude.NFData MqttHeaders where
  rnf MqttHeaders' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf correlationData
      `Prelude.seq` Prelude.rnf messageExpiry
      `Prelude.seq` Prelude.rnf payloadFormatIndicator
      `Prelude.seq` Prelude.rnf responseTopic
      `Prelude.seq` Prelude.rnf userProperties

instance Data.ToJSON MqttHeaders where
  toJSON MqttHeaders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentType" Data..=) Prelude.<$> contentType,
            ("correlationData" Data..=)
              Prelude.<$> correlationData,
            ("messageExpiry" Data..=) Prelude.<$> messageExpiry,
            ("payloadFormatIndicator" Data..=)
              Prelude.<$> payloadFormatIndicator,
            ("responseTopic" Data..=) Prelude.<$> responseTopic,
            ("userProperties" Data..=)
              Prelude.<$> userProperties
          ]
      )
