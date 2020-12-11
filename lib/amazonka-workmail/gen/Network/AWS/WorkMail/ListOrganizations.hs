{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListOrganizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the customer's organizations.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListOrganizations
  ( -- * Creating a request
    ListOrganizations (..),
    mkListOrganizations,

    -- ** Request lenses
    loNextToken,
    loMaxResults,

    -- * Destructuring the response
    ListOrganizationsResponse (..),
    mkListOrganizationsResponse,

    -- ** Response lenses
    lorsNextToken,
    lorsOrganizationSummaries,
    lorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListOrganizations' smart constructor.
data ListOrganizations = ListOrganizations'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizations' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
mkListOrganizations ::
  ListOrganizations
mkListOrganizations =
  ListOrganizations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOrganizations (Lude.Maybe Lude.Text)
loNextToken = Lens.lens (nextToken :: ListOrganizations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOrganizations)
{-# DEPRECATED loNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxResults :: Lens.Lens' ListOrganizations (Lude.Maybe Lude.Natural)
loMaxResults = Lens.lens (maxResults :: ListOrganizations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOrganizations)
{-# DEPRECATED loMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOrganizations where
  page rq rs
    | Page.stop (rs Lens.^. lorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lorsOrganizationSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loNextToken Lens..~ rs Lens.^. lorsNextToken

instance Lude.AWSRequest ListOrganizations where
  type Rs ListOrganizations = ListOrganizationsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOrganizationsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "OrganizationSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOrganizations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListOrganizations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOrganizations where
  toJSON ListOrganizations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListOrganizations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOrganizations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListOrganizationsResponse' smart constructor.
data ListOrganizationsResponse = ListOrganizationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    organizationSummaries ::
      Lude.Maybe [OrganizationSummary],
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

-- | Creates a value of 'ListOrganizationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
-- * 'organizationSummaries' - The overview of owned organizations presented as a list of organization summaries.
-- * 'responseStatus' - The response status code.
mkListOrganizationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOrganizationsResponse
mkListOrganizationsResponse pResponseStatus_ =
  ListOrganizationsResponse'
    { nextToken = Lude.Nothing,
      organizationSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsNextToken :: Lens.Lens' ListOrganizationsResponse (Lude.Maybe Lude.Text)
lorsNextToken = Lens.lens (nextToken :: ListOrganizationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOrganizationsResponse)
{-# DEPRECATED lorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The overview of owned organizations presented as a list of organization summaries.
--
-- /Note:/ Consider using 'organizationSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsOrganizationSummaries :: Lens.Lens' ListOrganizationsResponse (Lude.Maybe [OrganizationSummary])
lorsOrganizationSummaries = Lens.lens (organizationSummaries :: ListOrganizationsResponse -> Lude.Maybe [OrganizationSummary]) (\s a -> s {organizationSummaries = a} :: ListOrganizationsResponse)
{-# DEPRECATED lorsOrganizationSummaries "Use generic-lens or generic-optics with 'organizationSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsResponseStatus :: Lens.Lens' ListOrganizationsResponse Lude.Int
lorsResponseStatus = Lens.lens (responseStatus :: ListOrganizationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOrganizationsResponse)
{-# DEPRECATED lorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
