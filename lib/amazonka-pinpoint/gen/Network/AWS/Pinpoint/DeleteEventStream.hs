{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event stream for an application.
module Network.AWS.Pinpoint.DeleteEventStream
  ( -- * Creating a request
    DeleteEventStream (..),
    mkDeleteEventStream,

    -- ** Request lenses
    desApplicationId,

    -- * Destructuring the response
    DeleteEventStreamResponse (..),
    mkDeleteEventStreamResponse,

    -- ** Response lenses
    desrsEventStream,
    desrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEventStream' smart constructor.
newtype DeleteEventStream = DeleteEventStream'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventStream' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteEventStream ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteEventStream
mkDeleteEventStream pApplicationId_ =
  DeleteEventStream' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desApplicationId :: Lens.Lens' DeleteEventStream Lude.Text
desApplicationId = Lens.lens (applicationId :: DeleteEventStream -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteEventStream)
{-# DEPRECATED desApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteEventStream where
  type Rs DeleteEventStream = DeleteEventStreamResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteEventStreamResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEventStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteEventStream where
  toPath DeleteEventStream' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/eventstream"]

instance Lude.ToQuery DeleteEventStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEventStreamResponse' smart constructor.
data DeleteEventStreamResponse = DeleteEventStreamResponse'
  { eventStream :: EventStream,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventStreamResponse' with the minimum fields required to make a request.
--
-- * 'eventStream' -
-- * 'responseStatus' - The response status code.
mkDeleteEventStreamResponse ::
  -- | 'eventStream'
  EventStream ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEventStreamResponse
mkDeleteEventStreamResponse pEventStream_ pResponseStatus_ =
  DeleteEventStreamResponse'
    { eventStream = pEventStream_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEventStream :: Lens.Lens' DeleteEventStreamResponse EventStream
desrsEventStream = Lens.lens (eventStream :: DeleteEventStreamResponse -> EventStream) (\s a -> s {eventStream = a} :: DeleteEventStreamResponse)
{-# DEPRECATED desrsEventStream "Use generic-lens or generic-optics with 'eventStream' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DeleteEventStreamResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DeleteEventStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEventStreamResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
