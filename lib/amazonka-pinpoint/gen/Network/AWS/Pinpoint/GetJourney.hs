{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gjApplicationId,
    gjJourneyId,

    -- * Destructuring the response
    GetJourneyResponse (..),
    mkGetJourneyResponse,

    -- ** Response lenses
    gjrsJourneyResponse,
    gjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJourney' smart constructor.
data GetJourney = GetJourney'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The unique identifier for the journey.
    journeyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourney' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'journeyId' - The unique identifier for the journey.
mkGetJourney ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'journeyId'
  Lude.Text ->
  GetJourney
mkGetJourney pApplicationId_ pJourneyId_ =
  GetJourney'
    { applicationId = pApplicationId_,
      journeyId = pJourneyId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjApplicationId :: Lens.Lens' GetJourney Lude.Text
gjApplicationId = Lens.lens (applicationId :: GetJourney -> Lude.Text) (\s a -> s {applicationId = a} :: GetJourney)
{-# DEPRECATED gjApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjJourneyId :: Lens.Lens' GetJourney Lude.Text
gjJourneyId = Lens.lens (journeyId :: GetJourney -> Lude.Text) (\s a -> s {journeyId = a} :: GetJourney)
{-# DEPRECATED gjJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

instance Lude.AWSRequest GetJourney where
  type Rs GetJourney = GetJourneyResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJourneyResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { journeyResponse :: JourneyResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJourneyResponse' with the minimum fields required to make a request.
--
-- * 'journeyResponse' -
-- * 'responseStatus' - The response status code.
mkGetJourneyResponse ::
  -- | 'journeyResponse'
  JourneyResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetJourneyResponse
mkGetJourneyResponse pJourneyResponse_ pResponseStatus_ =
  GetJourneyResponse'
    { journeyResponse = pJourneyResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsJourneyResponse :: Lens.Lens' GetJourneyResponse JourneyResponse
gjrsJourneyResponse = Lens.lens (journeyResponse :: GetJourneyResponse -> JourneyResponse) (\s a -> s {journeyResponse = a} :: GetJourneyResponse)
{-# DEPRECATED gjrsJourneyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsResponseStatus :: Lens.Lens' GetJourneyResponse Lude.Int
gjrsResponseStatus = Lens.lens (responseStatus :: GetJourneyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJourneyResponse)
{-# DEPRECATED gjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
