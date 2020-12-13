{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListResourceDelegates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the delegates associated with a resource. Users and groups can be resource delegates and answer requests on behalf of the resource.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResourceDelegates
  ( -- * Creating a request
    ListResourceDelegates (..),
    mkListResourceDelegates,

    -- ** Request lenses
    lrdResourceId,
    lrdNextToken,
    lrdMaxResults,
    lrdOrganizationId,

    -- * Destructuring the response
    ListResourceDelegatesResponse (..),
    mkListResourceDelegatesResponse,

    -- ** Response lenses
    lrdrsDelegates,
    lrdrsNextToken,
    lrdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListResourceDelegates' smart constructor.
data ListResourceDelegates = ListResourceDelegates'
  { -- | The identifier for the resource whose delegates are listed.
    resourceId :: Lude.Text,
    -- | The token used to paginate through the delegates associated with a resource.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of maximum results in a page.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The identifier for the organization that contains the resource for which delegates are listed.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceDelegates' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier for the resource whose delegates are listed.
-- * 'nextToken' - The token used to paginate through the delegates associated with a resource.
-- * 'maxResults' - The number of maximum results in a page.
-- * 'organizationId' - The identifier for the organization that contains the resource for which delegates are listed.
mkListResourceDelegates ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  ListResourceDelegates
mkListResourceDelegates pResourceId_ pOrganizationId_ =
  ListResourceDelegates'
    { resourceId = pResourceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The identifier for the resource whose delegates are listed.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdResourceId :: Lens.Lens' ListResourceDelegates Lude.Text
lrdResourceId = Lens.lens (resourceId :: ListResourceDelegates -> Lude.Text) (\s a -> s {resourceId = a} :: ListResourceDelegates)
{-# DEPRECATED lrdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The token used to paginate through the delegates associated with a resource.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdNextToken :: Lens.Lens' ListResourceDelegates (Lude.Maybe Lude.Text)
lrdNextToken = Lens.lens (nextToken :: ListResourceDelegates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDelegates)
{-# DEPRECATED lrdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of maximum results in a page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdMaxResults :: Lens.Lens' ListResourceDelegates (Lude.Maybe Lude.Natural)
lrdMaxResults = Lens.lens (maxResults :: ListResourceDelegates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResourceDelegates)
{-# DEPRECATED lrdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the organization that contains the resource for which delegates are listed.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdOrganizationId :: Lens.Lens' ListResourceDelegates Lude.Text
lrdOrganizationId = Lens.lens (organizationId :: ListResourceDelegates -> Lude.Text) (\s a -> s {organizationId = a} :: ListResourceDelegates)
{-# DEPRECATED lrdOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager ListResourceDelegates where
  page rq rs
    | Page.stop (rs Lens.^. lrdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrdrsDelegates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrdNextToken Lens..~ rs Lens.^. lrdrsNextToken

instance Lude.AWSRequest ListResourceDelegates where
  type Rs ListResourceDelegates = ListResourceDelegatesResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceDelegatesResponse'
            Lude.<$> (x Lude..?> "Delegates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourceDelegates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListResourceDelegates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourceDelegates where
  toJSON ListResourceDelegates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListResourceDelegates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourceDelegates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourceDelegatesResponse' smart constructor.
data ListResourceDelegatesResponse = ListResourceDelegatesResponse'
  { -- | One page of the resource's delegates.
    delegates :: Lude.Maybe [Delegate],
    -- | The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceDelegatesResponse' with the minimum fields required to make a request.
--
-- * 'delegates' - One page of the resource's delegates.
-- * 'nextToken' - The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty.
-- * 'responseStatus' - The response status code.
mkListResourceDelegatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceDelegatesResponse
mkListResourceDelegatesResponse pResponseStatus_ =
  ListResourceDelegatesResponse'
    { delegates = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One page of the resource's delegates.
--
-- /Note:/ Consider using 'delegates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrsDelegates :: Lens.Lens' ListResourceDelegatesResponse (Lude.Maybe [Delegate])
lrdrsDelegates = Lens.lens (delegates :: ListResourceDelegatesResponse -> Lude.Maybe [Delegate]) (\s a -> s {delegates = a} :: ListResourceDelegatesResponse)
{-# DEPRECATED lrdrsDelegates "Use generic-lens or generic-optics with 'delegates' instead." #-}

-- | The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrsNextToken :: Lens.Lens' ListResourceDelegatesResponse (Lude.Maybe Lude.Text)
lrdrsNextToken = Lens.lens (nextToken :: ListResourceDelegatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDelegatesResponse)
{-# DEPRECATED lrdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrsResponseStatus :: Lens.Lens' ListResourceDelegatesResponse Lude.Int
lrdrsResponseStatus = Lens.lens (responseStatus :: ListResourceDelegatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceDelegatesResponse)
{-# DEPRECATED lrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
