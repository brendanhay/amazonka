{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a journey for an application.
module Network.AWS.Pinpoint.CreateJourney
  ( -- * Creating a request
    CreateJourney (..),
    mkCreateJourney,

    -- ** Request lenses
    cjWriteJourneyRequest,
    cjApplicationId,

    -- * Destructuring the response
    CreateJourneyResponse (..),
    mkCreateJourneyResponse,

    -- ** Response lenses
    cjrsJourneyResponse,
    cjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateJourney' smart constructor.
data CreateJourney = CreateJourney'
  { writeJourneyRequest :: WriteJourneyRequest,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJourney' with the minimum fields required to make a request.
--
-- * 'writeJourneyRequest' -
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkCreateJourney ::
  -- | 'writeJourneyRequest'
  WriteJourneyRequest ->
  -- | 'applicationId'
  Lude.Text ->
  CreateJourney
mkCreateJourney pWriteJourneyRequest_ pApplicationId_ =
  CreateJourney'
    { writeJourneyRequest = pWriteJourneyRequest_,
      applicationId = pApplicationId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeJourneyRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjWriteJourneyRequest :: Lens.Lens' CreateJourney WriteJourneyRequest
cjWriteJourneyRequest = Lens.lens (writeJourneyRequest :: CreateJourney -> WriteJourneyRequest) (\s a -> s {writeJourneyRequest = a} :: CreateJourney)
{-# DEPRECATED cjWriteJourneyRequest "Use generic-lens or generic-optics with 'writeJourneyRequest' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjApplicationId :: Lens.Lens' CreateJourney Lude.Text
cjApplicationId = Lens.lens (applicationId :: CreateJourney -> Lude.Text) (\s a -> s {applicationId = a} :: CreateJourney)
{-# DEPRECATED cjApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest CreateJourney where
  type Rs CreateJourney = CreateJourneyResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJourneyResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJourney where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateJourney where
  toJSON CreateJourney' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WriteJourneyRequest" Lude..= writeJourneyRequest)]
      )

instance Lude.ToPath CreateJourney where
  toPath CreateJourney' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/journeys"]

instance Lude.ToQuery CreateJourney where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJourneyResponse' smart constructor.
data CreateJourneyResponse = CreateJourneyResponse'
  { journeyResponse :: JourneyResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJourneyResponse' with the minimum fields required to make a request.
--
-- * 'journeyResponse' -
-- * 'responseStatus' - The response status code.
mkCreateJourneyResponse ::
  -- | 'journeyResponse'
  JourneyResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateJourneyResponse
mkCreateJourneyResponse pJourneyResponse_ pResponseStatus_ =
  CreateJourneyResponse'
    { journeyResponse = pJourneyResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJourneyResponse :: Lens.Lens' CreateJourneyResponse JourneyResponse
cjrsJourneyResponse = Lens.lens (journeyResponse :: CreateJourneyResponse -> JourneyResponse) (\s a -> s {journeyResponse = a} :: CreateJourneyResponse)
{-# DEPRECATED cjrsJourneyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJourneyResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJourneyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJourneyResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
