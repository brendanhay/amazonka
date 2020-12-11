{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the event stream settings for an application.
module Network.AWS.Pinpoint.GetEventStream
  ( -- * Creating a request
    GetEventStream (..),
    mkGetEventStream,

    -- ** Request lenses
    gesApplicationId,

    -- * Destructuring the response
    GetEventStreamResponse (..),
    mkGetEventStreamResponse,

    -- ** Response lenses
    gesrsResponseStatus,
    gesrsEventStream,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEventStream' smart constructor.
newtype GetEventStream = GetEventStream'
  { applicationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEventStream' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetEventStream ::
  -- | 'applicationId'
  Lude.Text ->
  GetEventStream
mkGetEventStream pApplicationId_ =
  GetEventStream' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesApplicationId :: Lens.Lens' GetEventStream Lude.Text
gesApplicationId = Lens.lens (applicationId :: GetEventStream -> Lude.Text) (\s a -> s {applicationId = a} :: GetEventStream)
{-# DEPRECATED gesApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetEventStream where
  type Rs GetEventStream = GetEventStreamResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEventStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetEventStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetEventStream where
  toPath GetEventStream' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/eventstream"]

instance Lude.ToQuery GetEventStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetEventStreamResponse' smart constructor.
data GetEventStreamResponse = GetEventStreamResponse'
  { responseStatus ::
      Lude.Int,
    eventStream :: EventStream
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEventStreamResponse' with the minimum fields required to make a request.
--
-- * 'eventStream' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetEventStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'eventStream'
  EventStream ->
  GetEventStreamResponse
mkGetEventStreamResponse pResponseStatus_ pEventStream_ =
  GetEventStreamResponse'
    { responseStatus = pResponseStatus_,
      eventStream = pEventStream_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrsResponseStatus :: Lens.Lens' GetEventStreamResponse Lude.Int
gesrsResponseStatus = Lens.lens (responseStatus :: GetEventStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEventStreamResponse)
{-# DEPRECATED gesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrsEventStream :: Lens.Lens' GetEventStreamResponse EventStream
gesrsEventStream = Lens.lens (eventStream :: GetEventStreamResponse -> EventStream) (\s a -> s {eventStream = a} :: GetEventStreamResponse)
{-# DEPRECATED gesrsEventStream "Use generic-lens or generic-optics with 'eventStream' instead." #-}
