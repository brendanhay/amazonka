{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.Publish
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes state information.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http HTTP Protocol> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.Publish
  ( -- * Creating a request
    Publish (..),
    mkPublish,

    -- ** Request lenses
    pPayload,
    pTopic,
    pQos,

    -- * Destructuring the response
    PublishResponse (..),
    mkPublishResponse,
  )
where

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the Publish operation.
--
-- /See:/ 'mkPublish' smart constructor.
data Publish = Publish'
  { -- | The state information, in JSON format.
    payload :: Lude.Maybe Lude.ByteString,
    -- | The name of the MQTT topic.
    topic :: Lude.Text,
    -- | The Quality of Service (QoS) level.
    qos :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Publish' with the minimum fields required to make a request.
--
-- * 'payload' - The state information, in JSON format.
-- * 'topic' - The name of the MQTT topic.
-- * 'qos' - The Quality of Service (QoS) level.
mkPublish ::
  -- | 'topic'
  Lude.Text ->
  Publish
mkPublish pTopic_ =
  Publish'
    { payload = Lude.Nothing,
      topic = pTopic_,
      qos = Lude.Nothing
    }

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPayload :: Lens.Lens' Publish (Lude.Maybe Lude.ByteString)
pPayload = Lens.lens (payload :: Publish -> Lude.Maybe Lude.ByteString) (\s a -> s {payload = a} :: Publish)
{-# DEPRECATED pPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The name of the MQTT topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTopic :: Lens.Lens' Publish Lude.Text
pTopic = Lens.lens (topic :: Publish -> Lude.Text) (\s a -> s {topic = a} :: Publish)
{-# DEPRECATED pTopic "Use generic-lens or generic-optics with 'topic' instead." #-}

-- | The Quality of Service (QoS) level.
--
-- /Note:/ Consider using 'qos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pQos :: Lens.Lens' Publish (Lude.Maybe Lude.Natural)
pQos = Lens.lens (qos :: Publish -> Lude.Maybe Lude.Natural) (\s a -> s {qos = a} :: Publish)
{-# DEPRECATED pQos "Use generic-lens or generic-optics with 'qos' instead." #-}

instance Lude.AWSRequest Publish where
  type Rs Publish = PublishResponse
  request = Req.postBody ioTDataService
  response = Res.receiveNull PublishResponse'

instance Lude.ToBody Publish where
  toBody = Lude.toBody Lude.. payload

instance Lude.ToHeaders Publish where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath Publish where
  toPath Publish' {..} = Lude.mconcat ["/topics/", Lude.toBS topic]

instance Lude.ToQuery Publish where
  toQuery Publish' {..} = Lude.mconcat ["qos" Lude.=: qos]

-- | /See:/ 'mkPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishResponse' with the minimum fields required to make a request.
mkPublishResponse ::
  PublishResponse
mkPublishResponse = PublishResponse'
