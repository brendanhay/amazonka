{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroups
  ( -- * Creating a request
    ListGroups (..),
    mkListGroups,

    -- ** Request lenses
    lgNextToken,
    lgMaxResults,
    lgOrganizationId,

    -- * Destructuring the response
    ListGroupsResponse (..),
    mkListGroupsResponse,

    -- ** Response lenses
    lgrsGroups,
    lgrsNextToken,
    lgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The identifier for the organization under which the groups exist.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'organizationId' - The identifier for the organization under which the groups exist.
mkListGroups ::
  -- | 'organizationId'
  Lude.Text ->
  ListGroups
mkListGroups pOrganizationId_ =
  ListGroups'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgNextToken = Lens.lens (nextToken :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroups)
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGroups (Lude.Maybe Lude.Natural)
lgMaxResults = Lens.lens (maxResults :: ListGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGroups)
{-# DEPRECATED lgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the organization under which the groups exist.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgOrganizationId :: Lens.Lens' ListGroups Lude.Text
lgOrganizationId = Lens.lens (organizationId :: ListGroups -> Lude.Text) (\s a -> s {organizationId = a} :: ListGroups)
{-# DEPRECATED lgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager ListGroups where
  page rq rs
    | Page.stop (rs Lens.^. lgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrsGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgNextToken Lens..~ rs Lens.^. lgrsNextToken

instance Lude.AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | The overview of groups for an organization.
    groups :: Lude.Maybe [Group],
    -- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groups' - The overview of groups for an organization.
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupsResponse
mkListGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { groups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The overview of groups for an organization.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGroups :: Lens.Lens' ListGroupsResponse (Lude.Maybe [Group])
lgrsGroups = Lens.lens (groups :: ListGroupsResponse -> Lude.Maybe [Group]) (\s a -> s {groups = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsNextToken :: Lens.Lens' ListGroupsResponse (Lude.Maybe Lude.Text)
lgrsNextToken = Lens.lens (nextToken :: ListGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGroupsResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
