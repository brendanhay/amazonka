{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTData.Publish
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes an MQTT message.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions Publish>
-- action.
--
-- For more information about MQTT messages, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html MQTT Protocol>
-- in the IoT Developer Guide.
--
-- For more information about messaging costs, see
-- <http://aws.amazon.com/iot-core/pricing/#Messaging Amazon Web Services IoT Core pricing - Messaging>.
module Amazonka.IoTData.Publish
  ( -- * Creating a Request
    Publish (..),
    newPublish,

    -- * Request Lenses
    publish_contentType,
    publish_correlationData,
    publish_messageExpiry,
    publish_payload,
    publish_payloadFormatIndicator,
    publish_qos,
    publish_responseTopic,
    publish_retain,
    publish_userProperties,
    publish_topic,

    -- * Destructuring the Response
    PublishResponse (..),
    newPublishResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the Publish operation.
--
-- /See:/ 'newPublish' smart constructor.
data Publish = Publish'
  { -- | A UTF-8 encoded string that describes the content of the publishing
    -- message.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded binary data used by the sender of the request message
    -- to identify which request the response message is for when it\'s
    -- received. @correlationData@ is an HTTP header value in the API.
    correlationData :: Prelude.Maybe Prelude.Text,
    -- | A user-defined integer value that represents the message expiry interval
    -- in seconds. If absent, the message doesn\'t expire. For more information
    -- about the limits of @messageExpiry@, see
    -- <https://docs.aws.amazon.com/general/latest/gr/iot-core.html#message-broker-limits Amazon Web Services IoT Core message broker and protocol limits and quotas>
    -- from the Amazon Web Services Reference Guide.
    messageExpiry :: Prelude.Maybe Prelude.Integer,
    -- | The message body. MQTT accepts text, binary, and empty (null) message
    -- payloads.
    --
    -- Publishing an empty (null) payload with __retain__ = @true@ deletes the
    -- retained message identified by __topic__ from Amazon Web Services IoT
    -- Core.
    payload :: Prelude.Maybe Prelude.ByteString,
    -- | An @Enum@ string value that indicates whether the payload is formatted
    -- as UTF-8. @payloadFormatIndicator@ is an HTTP header value in the API.
    payloadFormatIndicator :: Prelude.Maybe PayloadFormatIndicator,
    -- | The Quality of Service (QoS) level. The default QoS level is 0.
    qos :: Prelude.Maybe Prelude.Natural,
    -- | A UTF-8 encoded string that\'s used as the topic name for a response
    -- message. The response topic is used to describe the topic which the
    -- receiver should publish to as part of the request-response flow. The
    -- topic must not contain wildcard characters.
    responseTopic :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that determines whether to set the RETAIN flag when the
    -- message is published.
    --
    -- Setting the RETAIN flag causes the message to be retained and sent to
    -- new subscribers to the topic.
    --
    -- Valid values: @true@ | @false@
    --
    -- Default value: @false@
    retain :: Prelude.Maybe Prelude.Bool,
    -- | A JSON string that contains an array of JSON objects. If you don’t use
    -- Amazon Web Services SDK or CLI, you must encode the JSON string to
    -- base64 format before adding it to the HTTP header. @userProperties@ is
    -- an HTTP header value in the API.
    --
    -- The following example @userProperties@ parameter is a JSON string which
    -- represents two User Properties. Note that it needs to be base64-encoded:
    --
    -- @[{\"deviceName\": \"alpha\"}, {\"deviceCnt\": \"45\"}]@
    userProperties :: Prelude.Maybe Prelude.Text,
    -- | The name of the MQTT topic.
    topic :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Publish' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'publish_contentType' - A UTF-8 encoded string that describes the content of the publishing
-- message.
--
-- 'correlationData', 'publish_correlationData' - The base64-encoded binary data used by the sender of the request message
-- to identify which request the response message is for when it\'s
-- received. @correlationData@ is an HTTP header value in the API.
--
-- 'messageExpiry', 'publish_messageExpiry' - A user-defined integer value that represents the message expiry interval
-- in seconds. If absent, the message doesn\'t expire. For more information
-- about the limits of @messageExpiry@, see
-- <https://docs.aws.amazon.com/general/latest/gr/iot-core.html#message-broker-limits Amazon Web Services IoT Core message broker and protocol limits and quotas>
-- from the Amazon Web Services Reference Guide.
--
-- 'payload', 'publish_payload' - The message body. MQTT accepts text, binary, and empty (null) message
-- payloads.
--
-- Publishing an empty (null) payload with __retain__ = @true@ deletes the
-- retained message identified by __topic__ from Amazon Web Services IoT
-- Core.
--
-- 'payloadFormatIndicator', 'publish_payloadFormatIndicator' - An @Enum@ string value that indicates whether the payload is formatted
-- as UTF-8. @payloadFormatIndicator@ is an HTTP header value in the API.
--
-- 'qos', 'publish_qos' - The Quality of Service (QoS) level. The default QoS level is 0.
--
-- 'responseTopic', 'publish_responseTopic' - A UTF-8 encoded string that\'s used as the topic name for a response
-- message. The response topic is used to describe the topic which the
-- receiver should publish to as part of the request-response flow. The
-- topic must not contain wildcard characters.
--
-- 'retain', 'publish_retain' - A Boolean value that determines whether to set the RETAIN flag when the
-- message is published.
--
-- Setting the RETAIN flag causes the message to be retained and sent to
-- new subscribers to the topic.
--
-- Valid values: @true@ | @false@
--
-- Default value: @false@
--
-- 'userProperties', 'publish_userProperties' - A JSON string that contains an array of JSON objects. If you don’t use
-- Amazon Web Services SDK or CLI, you must encode the JSON string to
-- base64 format before adding it to the HTTP header. @userProperties@ is
-- an HTTP header value in the API.
--
-- The following example @userProperties@ parameter is a JSON string which
-- represents two User Properties. Note that it needs to be base64-encoded:
--
-- @[{\"deviceName\": \"alpha\"}, {\"deviceCnt\": \"45\"}]@
--
-- 'topic', 'publish_topic' - The name of the MQTT topic.
newPublish ::
  -- | 'topic'
  Prelude.Text ->
  Publish
newPublish pTopic_ =
  Publish'
    { contentType = Prelude.Nothing,
      correlationData = Prelude.Nothing,
      messageExpiry = Prelude.Nothing,
      payload = Prelude.Nothing,
      payloadFormatIndicator = Prelude.Nothing,
      qos = Prelude.Nothing,
      responseTopic = Prelude.Nothing,
      retain = Prelude.Nothing,
      userProperties = Prelude.Nothing,
      topic = pTopic_
    }

-- | A UTF-8 encoded string that describes the content of the publishing
-- message.
publish_contentType :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_contentType = Lens.lens (\Publish' {contentType} -> contentType) (\s@Publish' {} a -> s {contentType = a} :: Publish)

-- | The base64-encoded binary data used by the sender of the request message
-- to identify which request the response message is for when it\'s
-- received. @correlationData@ is an HTTP header value in the API.
publish_correlationData :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_correlationData = Lens.lens (\Publish' {correlationData} -> correlationData) (\s@Publish' {} a -> s {correlationData = a} :: Publish)

-- | A user-defined integer value that represents the message expiry interval
-- in seconds. If absent, the message doesn\'t expire. For more information
-- about the limits of @messageExpiry@, see
-- <https://docs.aws.amazon.com/general/latest/gr/iot-core.html#message-broker-limits Amazon Web Services IoT Core message broker and protocol limits and quotas>
-- from the Amazon Web Services Reference Guide.
publish_messageExpiry :: Lens.Lens' Publish (Prelude.Maybe Prelude.Integer)
publish_messageExpiry = Lens.lens (\Publish' {messageExpiry} -> messageExpiry) (\s@Publish' {} a -> s {messageExpiry = a} :: Publish)

-- | The message body. MQTT accepts text, binary, and empty (null) message
-- payloads.
--
-- Publishing an empty (null) payload with __retain__ = @true@ deletes the
-- retained message identified by __topic__ from Amazon Web Services IoT
-- Core.
publish_payload :: Lens.Lens' Publish (Prelude.Maybe Prelude.ByteString)
publish_payload = Lens.lens (\Publish' {payload} -> payload) (\s@Publish' {} a -> s {payload = a} :: Publish)

-- | An @Enum@ string value that indicates whether the payload is formatted
-- as UTF-8. @payloadFormatIndicator@ is an HTTP header value in the API.
publish_payloadFormatIndicator :: Lens.Lens' Publish (Prelude.Maybe PayloadFormatIndicator)
publish_payloadFormatIndicator = Lens.lens (\Publish' {payloadFormatIndicator} -> payloadFormatIndicator) (\s@Publish' {} a -> s {payloadFormatIndicator = a} :: Publish)

-- | The Quality of Service (QoS) level. The default QoS level is 0.
publish_qos :: Lens.Lens' Publish (Prelude.Maybe Prelude.Natural)
publish_qos = Lens.lens (\Publish' {qos} -> qos) (\s@Publish' {} a -> s {qos = a} :: Publish)

-- | A UTF-8 encoded string that\'s used as the topic name for a response
-- message. The response topic is used to describe the topic which the
-- receiver should publish to as part of the request-response flow. The
-- topic must not contain wildcard characters.
publish_responseTopic :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_responseTopic = Lens.lens (\Publish' {responseTopic} -> responseTopic) (\s@Publish' {} a -> s {responseTopic = a} :: Publish)

-- | A Boolean value that determines whether to set the RETAIN flag when the
-- message is published.
--
-- Setting the RETAIN flag causes the message to be retained and sent to
-- new subscribers to the topic.
--
-- Valid values: @true@ | @false@
--
-- Default value: @false@
publish_retain :: Lens.Lens' Publish (Prelude.Maybe Prelude.Bool)
publish_retain = Lens.lens (\Publish' {retain} -> retain) (\s@Publish' {} a -> s {retain = a} :: Publish)

-- | A JSON string that contains an array of JSON objects. If you don’t use
-- Amazon Web Services SDK or CLI, you must encode the JSON string to
-- base64 format before adding it to the HTTP header. @userProperties@ is
-- an HTTP header value in the API.
--
-- The following example @userProperties@ parameter is a JSON string which
-- represents two User Properties. Note that it needs to be base64-encoded:
--
-- @[{\"deviceName\": \"alpha\"}, {\"deviceCnt\": \"45\"}]@
publish_userProperties :: Lens.Lens' Publish (Prelude.Maybe Prelude.Text)
publish_userProperties = Lens.lens (\Publish' {userProperties} -> userProperties) (\s@Publish' {} a -> s {userProperties = a} :: Publish)

-- | The name of the MQTT topic.
publish_topic :: Lens.Lens' Publish Prelude.Text
publish_topic = Lens.lens (\Publish' {topic} -> topic) (\s@Publish' {} a -> s {topic = a} :: Publish)

instance Core.AWSRequest Publish where
  type AWSResponse Publish = PublishResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response = Response.receiveNull PublishResponse'

instance Prelude.Hashable Publish where
  hashWithSalt _salt Publish' {..} =
    _salt `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` correlationData
      `Prelude.hashWithSalt` messageExpiry
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` payloadFormatIndicator
      `Prelude.hashWithSalt` qos
      `Prelude.hashWithSalt` responseTopic
      `Prelude.hashWithSalt` retain
      `Prelude.hashWithSalt` userProperties
      `Prelude.hashWithSalt` topic

instance Prelude.NFData Publish where
  rnf Publish' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf correlationData
      `Prelude.seq` Prelude.rnf messageExpiry
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf payloadFormatIndicator
      `Prelude.seq` Prelude.rnf qos
      `Prelude.seq` Prelude.rnf responseTopic
      `Prelude.seq` Prelude.rnf retain
      `Prelude.seq` Prelude.rnf userProperties
      `Prelude.seq` Prelude.rnf topic

instance Data.ToBody Publish where
  toBody Publish' {..} = Data.toBody payload

instance Data.ToHeaders Publish where
  toHeaders Publish' {..} =
    Prelude.mconcat
      [ "x-amz-mqtt5-correlation-data"
          Data.=# correlationData,
        "x-amz-mqtt5-payload-format-indicator"
          Data.=# payloadFormatIndicator,
        "x-amz-mqtt5-user-properties" Data.=# userProperties
      ]

instance Data.ToPath Publish where
  toPath Publish' {..} =
    Prelude.mconcat ["/topics/", Data.toBS topic]

instance Data.ToQuery Publish where
  toQuery Publish' {..} =
    Prelude.mconcat
      [ "contentType" Data.=: contentType,
        "messageExpiry" Data.=: messageExpiry,
        "qos" Data.=: qos,
        "responseTopic" Data.=: responseTopic,
        "retain" Data.=: retain
      ]

-- | /See:/ 'newPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPublishResponse ::
  PublishResponse
newPublishResponse = PublishResponse'

instance Prelude.NFData PublishResponse where
  rnf _ = ()
