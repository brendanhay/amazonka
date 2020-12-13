{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.PutEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event stream for an application or updates the settings of an existing event stream for an application.
module Network.AWS.Pinpoint.PutEventStream
  ( -- * Creating a request
    PutEventStream (..),
    mkPutEventStream,

    -- ** Request lenses
    pesApplicationId,
    pesWriteEventStream,

    -- * Destructuring the response
    PutEventStreamResponse (..),
    mkPutEventStreamResponse,

    -- ** Response lenses
    pesrsEventStream,
    pesrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutEventStream' smart constructor.
data PutEventStream = PutEventStream'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    writeEventStream :: WriteEventStream
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEventStream' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'writeEventStream' -
mkPutEventStream ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'writeEventStream'
  WriteEventStream ->
  PutEventStream
mkPutEventStream pApplicationId_ pWriteEventStream_ =
  PutEventStream'
    { applicationId = pApplicationId_,
      writeEventStream = pWriteEventStream_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesApplicationId :: Lens.Lens' PutEventStream Lude.Text
pesApplicationId = Lens.lens (applicationId :: PutEventStream -> Lude.Text) (\s a -> s {applicationId = a} :: PutEventStream)
{-# DEPRECATED pesApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeEventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesWriteEventStream :: Lens.Lens' PutEventStream WriteEventStream
pesWriteEventStream = Lens.lens (writeEventStream :: PutEventStream -> WriteEventStream) (\s a -> s {writeEventStream = a} :: PutEventStream)
{-# DEPRECATED pesWriteEventStream "Use generic-lens or generic-optics with 'writeEventStream' instead." #-}

instance Lude.AWSRequest PutEventStream where
  type Rs PutEventStream = PutEventStreamResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutEventStreamResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutEventStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutEventStream where
  toJSON PutEventStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WriteEventStream" Lude..= writeEventStream)]
      )

instance Lude.ToPath PutEventStream where
  toPath PutEventStream' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/eventstream"]

instance Lude.ToQuery PutEventStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutEventStreamResponse' smart constructor.
data PutEventStreamResponse = PutEventStreamResponse'
  { eventStream :: EventStream,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEventStreamResponse' with the minimum fields required to make a request.
--
-- * 'eventStream' -
-- * 'responseStatus' - The response status code.
mkPutEventStreamResponse ::
  -- | 'eventStream'
  EventStream ->
  -- | 'responseStatus'
  Lude.Int ->
  PutEventStreamResponse
mkPutEventStreamResponse pEventStream_ pResponseStatus_ =
  PutEventStreamResponse'
    { eventStream = pEventStream_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrsEventStream :: Lens.Lens' PutEventStreamResponse EventStream
pesrsEventStream = Lens.lens (eventStream :: PutEventStreamResponse -> EventStream) (\s a -> s {eventStream = a} :: PutEventStreamResponse)
{-# DEPRECATED pesrsEventStream "Use generic-lens or generic-optics with 'eventStream' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrsResponseStatus :: Lens.Lens' PutEventStreamResponse Lude.Int
pesrsResponseStatus = Lens.lens (responseStatus :: PutEventStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutEventStreamResponse)
{-# DEPRECATED pesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
