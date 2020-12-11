{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for a journey.
module Network.AWS.Pinpoint.GetJourney
  ( -- * Creating a request
    GetJourney (..),
    mkGetJourney,

    -- ** Request lenses
    gjJourneyId,
    gjApplicationId,

    -- * Destructuring the response
    GetJourneyResponse (..),
    mkGetJourneyResponse,

    -- ** Response lenses
    gjrsResponseStatus,
    gjrsJourneyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJourney' smart constructor.
data GetJourney = GetJourney'
  { journeyId :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourney' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'journeyId' - The unique identifier for the journey.
mkGetJourney ::
  -- | 'journeyId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetJourney
mkGetJourney pJourneyId_ pApplicationId_ =
  GetJourney'
    { journeyId = pJourneyId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjJourneyId :: Lens.Lens' GetJourney Lude.Text
gjJourneyId = Lens.lens (journeyId :: GetJourney -> Lude.Text) (\s a -> s {journeyId = a} :: GetJourney)
{-# DEPRECATED gjJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjApplicationId :: Lens.Lens' GetJourney Lude.Text
gjApplicationId = Lens.lens (applicationId :: GetJourney -> Lude.Text) (\s a -> s {applicationId = a} :: GetJourney)
{-# DEPRECATED gjApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetJourney where
  type Rs GetJourney = GetJourneyResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJourneyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetJourney where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetJourney where
  toPath GetJourney' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/journeys/",
        Lude.toBS journeyId
      ]

instance Lude.ToQuery GetJourney where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJourneyResponse' smart constructor.
data GetJourneyResponse = GetJourneyResponse'
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

-- | Creates a value of 'GetJourneyResponse' with the minimum fields required to make a request.
--
-- * 'journeyResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetJourneyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  GetJourneyResponse
mkGetJourneyResponse pResponseStatus_ pJourneyResponse_ =
  GetJourneyResponse'
    { responseStatus = pResponseStatus_,
      journeyResponse = pJourneyResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsResponseStatus :: Lens.Lens' GetJourneyResponse Lude.Int
gjrsResponseStatus = Lens.lens (responseStatus :: GetJourneyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJourneyResponse)
{-# DEPRECATED gjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsJourneyResponse :: Lens.Lens' GetJourneyResponse JourneyResponse
gjrsJourneyResponse = Lens.lens (journeyResponse :: GetJourneyResponse -> JourneyResponse) (\s a -> s {journeyResponse = a} :: GetJourneyResponse)
{-# DEPRECATED gjrsJourneyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead." #-}
