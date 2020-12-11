{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a journey.
module Network.AWS.Pinpoint.UpdateJourney
  ( -- * Creating a request
    UpdateJourney (..),
    mkUpdateJourney,

    -- ** Request lenses
    ujJourneyId,
    ujApplicationId,
    ujWriteJourneyRequest,

    -- * Destructuring the response
    UpdateJourneyResponse (..),
    mkUpdateJourneyResponse,

    -- ** Response lenses
    ujrsResponseStatus,
    ujrsJourneyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJourney' smart constructor.
data UpdateJourney = UpdateJourney'
  { journeyId :: Lude.Text,
    applicationId :: Lude.Text,
    writeJourneyRequest :: WriteJourneyRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJourney' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'journeyId' - The unique identifier for the journey.
-- * 'writeJourneyRequest' - Undocumented field.
mkUpdateJourney ::
  -- | 'journeyId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'writeJourneyRequest'
  WriteJourneyRequest ->
  UpdateJourney
mkUpdateJourney pJourneyId_ pApplicationId_ pWriteJourneyRequest_ =
  UpdateJourney'
    { journeyId = pJourneyId_,
      applicationId = pApplicationId_,
      writeJourneyRequest = pWriteJourneyRequest_
    }

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJourneyId :: Lens.Lens' UpdateJourney Lude.Text
ujJourneyId = Lens.lens (journeyId :: UpdateJourney -> Lude.Text) (\s a -> s {journeyId = a} :: UpdateJourney)
{-# DEPRECATED ujJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujApplicationId :: Lens.Lens' UpdateJourney Lude.Text
ujApplicationId = Lens.lens (applicationId :: UpdateJourney -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateJourney)
{-# DEPRECATED ujApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeJourneyRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujWriteJourneyRequest :: Lens.Lens' UpdateJourney WriteJourneyRequest
ujWriteJourneyRequest = Lens.lens (writeJourneyRequest :: UpdateJourney -> WriteJourneyRequest) (\s a -> s {writeJourneyRequest = a} :: UpdateJourney)
{-# DEPRECATED ujWriteJourneyRequest "Use generic-lens or generic-optics with 'writeJourneyRequest' instead." #-}

instance Lude.AWSRequest UpdateJourney where
  type Rs UpdateJourney = UpdateJourneyResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateJourneyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateJourney where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJourney where
  toJSON UpdateJourney' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WriteJourneyRequest" Lude..= writeJourneyRequest)]
      )

instance Lude.ToPath UpdateJourney where
  toPath UpdateJourney' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/journeys/",
        Lude.toBS journeyId
      ]

instance Lude.ToQuery UpdateJourney where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJourneyResponse' smart constructor.
data UpdateJourneyResponse = UpdateJourneyResponse'
  { responseStatus ::
      Lude.Int,
    journeyResponse :: JourneyResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJourneyResponse' with the minimum fields required to make a request.
--
-- * 'journeyResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateJourneyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  UpdateJourneyResponse
mkUpdateJourneyResponse pResponseStatus_ pJourneyResponse_ =
  UpdateJourneyResponse'
    { responseStatus = pResponseStatus_,
      journeyResponse = pJourneyResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsResponseStatus :: Lens.Lens' UpdateJourneyResponse Lude.Int
ujrsResponseStatus = Lens.lens (responseStatus :: UpdateJourneyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJourneyResponse)
{-# DEPRECATED ujrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsJourneyResponse :: Lens.Lens' UpdateJourneyResponse JourneyResponse
ujrsJourneyResponse = Lens.lens (journeyResponse :: UpdateJourneyResponse -> JourneyResponse) (\s a -> s {journeyResponse = a} :: UpdateJourneyResponse)
{-# DEPRECATED ujrsJourneyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead." #-}
