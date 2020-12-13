{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's resources.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResources
  ( -- * Creating a request
    ListResources (..),
    mkListResources,

    -- ** Request lenses
    lrNextToken,
    lrMaxResults,
    lrOrganizationId,

    -- * Destructuring the response
    ListResourcesResponse (..),
    mkListResourcesResponse,

    -- ** Response lenses
    lrrsResources,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListResources' smart constructor.
data ListResources = ListResources'
  { -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The identifier for the organization under which the resources exist.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResources' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'organizationId' - The identifier for the organization under which the resources exist.
mkListResources ::
  -- | 'organizationId'
  Lude.Text ->
  ListResources
mkListResources pOrganizationId_ =
  ListResources'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListResources (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResources)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListResources (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListResources -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResources)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the organization under which the resources exist.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrOrganizationId :: Lens.Lens' ListResources Lude.Text
lrOrganizationId = Lens.lens (organizationId :: ListResources -> Lude.Text) (\s a -> s {organizationId = a} :: ListResources)
{-# DEPRECATED lrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager ListResources where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsResources) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListResources where
  type Rs ListResources = ListResourcesResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Lude.<$> (x Lude..?> "Resources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListResources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResources where
  toJSON ListResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListResources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { -- | One page of the organization's resource representation.
    resources :: Lude.Maybe [Resource],
    -- | The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesResponse' with the minimum fields required to make a request.
--
-- * 'resources' - One page of the organization's resource representation.
-- * 'nextToken' - The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
-- * 'responseStatus' - The response status code.
mkListResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourcesResponse
mkListResourcesResponse pResponseStatus_ =
  ListResourcesResponse'
    { resources = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One page of the organization's resource representation.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResources :: Lens.Lens' ListResourcesResponse (Lude.Maybe [Resource])
lrrsResources = Lens.lens (resources :: ListResourcesResponse -> Lude.Maybe [Resource]) (\s a -> s {resources = a} :: ListResourcesResponse)
{-# DEPRECATED lrrsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListResourcesResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourcesResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListResourcesResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourcesResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
