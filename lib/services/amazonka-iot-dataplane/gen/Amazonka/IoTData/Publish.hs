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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    publish_retain,
    publish_payload,
    publish_qos,
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
  { -- | A Boolean value that determines whether to set the RETAIN flag when the
    -- message is published.
    --
    -- Setting the RETAIN flag causes the message to be retained and sent to
    -- new subscribers to the topic.
    --
    -- Valid values: @true@ | @false@
    --
    -- Default value: @false@
    retain :: Prelude.Maybe Prelude.Bool,
    -- | The message body. MQTT accepts text, binary, and empty (null) message
    -- payloads.
    --
    -- Publishing an empty (null) payload with __retain__ = @true@ deletes the
    -- retained message identified by __topic__ from Amazon Web Services IoT
    -- Core.
    payload :: Prelude.Maybe Prelude.ByteString,
    -- | The Quality of Service (QoS) level.
    qos :: Prelude.Maybe Prelude.Natural,
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
-- 'payload', 'publish_payload' - The message body. MQTT accepts text, binary, and empty (null) message
-- payloads.
--
-- Publishing an empty (null) payload with __retain__ = @true@ deletes the
-- retained message identified by __topic__ from Amazon Web Services IoT
-- Core.
--
-- 'qos', 'publish_qos' - The Quality of Service (QoS) level.
--
-- 'topic', 'publish_topic' - The name of the MQTT topic.
newPublish ::
  -- | 'topic'
  Prelude.Text ->
  Publish
newPublish pTopic_ =
  Publish'
    { retain = Prelude.Nothing,
      payload = Prelude.Nothing,
      qos = Prelude.Nothing,
      topic = pTopic_
    }

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

-- | The message body. MQTT accepts text, binary, and empty (null) message
-- payloads.
--
-- Publishing an empty (null) payload with __retain__ = @true@ deletes the
-- retained message identified by __topic__ from Amazon Web Services IoT
-- Core.
publish_payload :: Lens.Lens' Publish (Prelude.Maybe Prelude.ByteString)
publish_payload = Lens.lens (\Publish' {payload} -> payload) (\s@Publish' {} a -> s {payload = a} :: Publish)

-- | The Quality of Service (QoS) level.
publish_qos :: Lens.Lens' Publish (Prelude.Maybe Prelude.Natural)
publish_qos = Lens.lens (\Publish' {qos} -> qos) (\s@Publish' {} a -> s {qos = a} :: Publish)

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
    _salt `Prelude.hashWithSalt` retain
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` qos
      `Prelude.hashWithSalt` topic

instance Prelude.NFData Publish where
  rnf Publish' {..} =
    Prelude.rnf retain
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf qos
      `Prelude.seq` Prelude.rnf topic

instance Data.ToBody Publish where
  toBody Publish' {..} = Data.toBody payload

instance Data.ToHeaders Publish where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath Publish where
  toPath Publish' {..} =
    Prelude.mconcat ["/topics/", Data.toBS topic]

instance Data.ToQuery Publish where
  toQuery Publish' {..} =
    Prelude.mconcat
      ["retain" Data.=: retain, "qos" Data.=: qos]

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
