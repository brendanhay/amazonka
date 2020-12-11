{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateJourneyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) an active journey.
module Network.AWS.Pinpoint.UpdateJourneyState
  ( -- * Creating a request
    UpdateJourneyState (..),
    mkUpdateJourneyState,

    -- ** Request lenses
    ujsJourneyId,
    ujsApplicationId,
    ujsJourneyStateRequest,

    -- * Destructuring the response
    UpdateJourneyStateResponse (..),
    mkUpdateJourneyStateResponse,

    -- ** Response lenses
    ujsrsResponseStatus,
    ujsrsJourneyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJourneyState' smart constructor.
data UpdateJourneyState = UpdateJourneyState'
  { journeyId ::
      Lude.Text,
    applicationId :: Lude.Text,
    journeyStateRequest :: JourneyStateRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJourneyState' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'journeyId' - The unique identifier for the journey.
-- * 'journeyStateRequest' - Undocumented field.
mkUpdateJourneyState ::
  -- | 'journeyId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'journeyStateRequest'
  JourneyStateRequest ->
  UpdateJourneyState
mkUpdateJourneyState
  pJourneyId_
  pApplicationId_
  pJourneyStateRequest_ =
    UpdateJourneyState'
      { journeyId = pJourneyId_,
        applicationId = pApplicationId_,
        journeyStateRequest = pJourneyStateRequest_
      }

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsJourneyId :: Lens.Lens' UpdateJourneyState Lude.Text
ujsJourneyId = Lens.lens (journeyId :: UpdateJourneyState -> Lude.Text) (\s a -> s {journeyId = a} :: UpdateJourneyState)
{-# DEPRECATED ujsJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsApplicationId :: Lens.Lens' UpdateJourneyState Lude.Text
ujsApplicationId = Lens.lens (applicationId :: UpdateJourneyState -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateJourneyState)
{-# DEPRECATED ujsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyStateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsJourneyStateRequest :: Lens.Lens' UpdateJourneyState JourneyStateRequest
ujsJourneyStateRequest = Lens.lens (journeyStateRequest :: UpdateJourneyState -> JourneyStateRequest) (\s a -> s {journeyStateRequest = a} :: UpdateJourneyState)
{-# DEPRECATED ujsJourneyStateRequest "Use generic-lens or generic-optics with 'journeyStateRequest' instead." #-}

instance Lude.AWSRequest UpdateJourneyState where
  type Rs UpdateJourneyState = UpdateJourneyStateResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateJourneyStateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateJourneyState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJourneyState where
  toJSON UpdateJourneyState' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("JourneyStateRequest" Lude..= journeyStateRequest)]
      )

instance Lude.ToPath UpdateJourneyState where
  toPath UpdateJourneyState' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/journeys/",
        Lude.toBS journeyId,
        "/state"
      ]

instance Lude.ToQuery UpdateJourneyState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJourneyStateResponse' smart constructor.
data UpdateJourneyStateResponse = UpdateJourneyStateResponse'
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

-- | Creates a value of 'UpdateJourneyStateResponse' with the minimum fields required to make a request.
--
-- * 'journeyResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateJourneyStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  UpdateJourneyStateResponse
mkUpdateJourneyStateResponse pResponseStatus_ pJourneyResponse_ =
  UpdateJourneyStateResponse'
    { responseStatus = pResponseStatus_,
      journeyResponse = pJourneyResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsrsResponseStatus :: Lens.Lens' UpdateJourneyStateResponse Lude.Int
ujsrsResponseStatus = Lens.lens (responseStatus :: UpdateJourneyStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJourneyStateResponse)
{-# DEPRECATED ujsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsrsJourneyResponse :: Lens.Lens' UpdateJourneyStateResponse JourneyResponse
ujsrsJourneyResponse = Lens.lens (journeyResponse :: UpdateJourneyStateResponse -> JourneyResponse) (\s a -> s {journeyResponse = a} :: UpdateJourneyStateResponse)
{-# DEPRECATED ujsrsJourneyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead." #-}
