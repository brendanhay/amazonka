{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists applications owned by the requester.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplications
  ( -- * Creating a request
    ListApplications (..),
    mkListApplications,

    -- ** Request lenses
    laNextToken,
    laMaxItems,

    -- * Destructuring the response
    ListApplicationsResponse (..),
    mkListApplicationsResponse,

    -- ** Response lenses
    larsNextToken,
    larsApplications,
    larsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkListApplications' smart constructor.
data ListApplications = ListApplications'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- * 'maxItems' - The total number of items to return.
-- * 'nextToken' - A token to specify where to start paginating.
mkListApplications ::
  ListApplications
mkListApplications =
  ListApplications'
    { nextToken = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | A token to specify where to start paginating.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApplications (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListApplications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplications)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxItems :: Lens.Lens' ListApplications (Lude.Maybe Lude.Natural)
laMaxItems = Lens.lens (maxItems :: ListApplications -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListApplications)
{-# DEPRECATED laMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListApplications where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsApplications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListApplications where
  type Rs ListApplications = ListApplicationsResponse
  request = Req.get serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "applications" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListApplications where
  toPath = Lude.const "/applications"

instance Lude.ToQuery ListApplications where
  toQuery ListApplications' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxItems" Lude.=: maxItems]

-- | /See:/ 'mkListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    applications ::
      Lude.Maybe [ApplicationSummary],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'applications' - An array of application summaries.
-- * 'nextToken' - The token to request the next page of results.
-- * 'responseStatus' - The response status code.
mkListApplicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationsResponse
mkListApplicationsResponse pResponseStatus_ =
  ListApplicationsResponse'
    { nextToken = Lude.Nothing,
      applications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListApplicationsResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListApplicationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationsResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of application summaries.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsApplications :: Lens.Lens' ListApplicationsResponse (Lude.Maybe [ApplicationSummary])
larsApplications = Lens.lens (applications :: ListApplicationsResponse -> Lude.Maybe [ApplicationSummary]) (\s a -> s {applications = a} :: ListApplicationsResponse)
{-# DEPRECATED larsApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListApplicationsResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationsResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
