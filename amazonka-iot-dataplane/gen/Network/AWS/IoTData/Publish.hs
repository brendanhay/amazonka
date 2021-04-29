{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTData.Publish
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes state information.
--
-- For more information, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http HTTP Protocol>
-- in the AWS IoT Developer Guide.
module Network.AWS.IoTData.Publish
  ( -- * Creating a Request
    Publish (..),
    newPublish,

    -- * Request Lenses
    publish_payload,
    publish_qos,
    publish_topic,

    -- * Destructuring the Response
    PublishResponse (..),
    newPublishResponse,
  )
where

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the Publish operation.
--
-- /See:/ 'newPublish' smart constructor.
data Publish = Publish'
  { -- | The state information, in JSON format.
    payload :: Prelude.Maybe Prelude.ByteString,
    -- | The Quality of Service (QoS) level.
    qos :: Prelude.Maybe Prelude.Natural,
    -- | The name of the MQTT topic.
    topic :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Publish' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'publish_payload' - The state information, in JSON format.
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
    { payload = Prelude.Nothing,
      qos = Prelude.Nothing,
      topic = pTopic_
    }

-- | The state information, in JSON format.
publish_payload :: Lens.Lens' Publish (Prelude.Maybe Prelude.ByteString)
publish_payload = Lens.lens (\Publish' {payload} -> payload) (\s@Publish' {} a -> s {payload = a} :: Publish)

-- | The Quality of Service (QoS) level.
publish_qos :: Lens.Lens' Publish (Prelude.Maybe Prelude.Natural)
publish_qos = Lens.lens (\Publish' {qos} -> qos) (\s@Publish' {} a -> s {qos = a} :: Publish)

-- | The name of the MQTT topic.
publish_topic :: Lens.Lens' Publish Prelude.Text
publish_topic = Lens.lens (\Publish' {topic} -> topic) (\s@Publish' {} a -> s {topic = a} :: Publish)

instance Prelude.AWSRequest Publish where
  type Rs Publish = PublishResponse
  request = Request.postBody defaultService
  response = Response.receiveNull PublishResponse'

instance Prelude.Hashable Publish

instance Prelude.NFData Publish

instance Prelude.ToBody Publish where
  toBody Publish' {..} = Prelude.toBody payload

instance Prelude.ToHeaders Publish where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath Publish where
  toPath Publish' {..} =
    Prelude.mconcat ["/topics/", Prelude.toBS topic]

instance Prelude.ToQuery Publish where
  toQuery Publish' {..} =
    Prelude.mconcat ["qos" Prelude.=: qos]

-- | /See:/ 'newPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PublishResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPublishResponse ::
  PublishResponse
newPublishResponse = PublishResponse'

instance Prelude.NFData PublishResponse
