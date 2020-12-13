{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListJourneys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all the journeys that are associated with an application.
module Network.AWS.Pinpoint.ListJourneys
  ( -- * Creating a request
    ListJourneys (..),
    mkListJourneys,

    -- ** Request lenses
    ljToken,
    ljApplicationId,
    ljPageSize,

    -- * Destructuring the response
    ListJourneysResponse (..),
    mkListJourneysResponse,

    -- ** Response lenses
    ljrsJourneysResponse,
    ljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJourneys' smart constructor.
data ListJourneys = ListJourneys'
  { -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJourneys' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkListJourneys ::
  -- | 'applicationId'
  Lude.Text ->
  ListJourneys
mkListJourneys pApplicationId_ =
  ListJourneys'
    { token = Lude.Nothing,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljToken :: Lens.Lens' ListJourneys (Lude.Maybe Lude.Text)
ljToken = Lens.lens (token :: ListJourneys -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: ListJourneys)
{-# DEPRECATED ljToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljApplicationId :: Lens.Lens' ListJourneys Lude.Text
ljApplicationId = Lens.lens (applicationId :: ListJourneys -> Lude.Text) (\s a -> s {applicationId = a} :: ListJourneys)
{-# DEPRECATED ljApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljPageSize :: Lens.Lens' ListJourneys (Lude.Maybe Lude.Text)
ljPageSize = Lens.lens (pageSize :: ListJourneys -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: ListJourneys)
{-# DEPRECATED ljPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest ListJourneys where
  type Rs ListJourneys = ListJourneysResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJourneysResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJourneys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListJourneys where
  toPath ListJourneys' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/journeys"]

instance Lude.ToQuery ListJourneys where
  toQuery ListJourneys' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkListJourneysResponse' smart constructor.
data ListJourneysResponse = ListJourneysResponse'
  { journeysResponse :: JourneysResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJourneysResponse' with the minimum fields required to make a request.
--
-- * 'journeysResponse' -
-- * 'responseStatus' - The response status code.
mkListJourneysResponse ::
  -- | 'journeysResponse'
  JourneysResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  ListJourneysResponse
mkListJourneysResponse pJourneysResponse_ pResponseStatus_ =
  ListJourneysResponse'
    { journeysResponse = pJourneysResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeysResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJourneysResponse :: Lens.Lens' ListJourneysResponse JourneysResponse
ljrsJourneysResponse = Lens.lens (journeysResponse :: ListJourneysResponse -> JourneysResponse) (\s a -> s {journeysResponse = a} :: ListJourneysResponse)
{-# DEPRECATED ljrsJourneysResponse "Use generic-lens or generic-optics with 'journeysResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJourneysResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJourneysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJourneysResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
