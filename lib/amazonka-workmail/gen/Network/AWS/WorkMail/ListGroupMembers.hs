{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an overview of the members of a group. Users and groups can be members of a group.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroupMembers
  ( -- * Creating a request
    ListGroupMembers (..),
    mkListGroupMembers,

    -- ** Request lenses
    lgmNextToken,
    lgmGroupId,
    lgmMaxResults,
    lgmOrganizationId,

    -- * Destructuring the response
    ListGroupMembersResponse (..),
    mkListGroupMembersResponse,

    -- ** Response lenses
    lgmrsMembers,
    lgmrsNextToken,
    lgmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListGroupMembers' smart constructor.
data ListGroupMembers = ListGroupMembers'
  { -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The identifier for the group to which the members (users or groups) are associated.
    groupId :: Lude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The identifier for the organization under which the group exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupMembers' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'groupId' - The identifier for the group to which the members (users or groups) are associated.
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'organizationId' - The identifier for the organization under which the group exists.
mkListGroupMembers ::
  -- | 'groupId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  ListGroupMembers
mkListGroupMembers pGroupId_ pOrganizationId_ =
  ListGroupMembers'
    { nextToken = Lude.Nothing,
      groupId = pGroupId_,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmNextToken :: Lens.Lens' ListGroupMembers (Lude.Maybe Lude.Text)
lgmNextToken = Lens.lens (nextToken :: ListGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupMembers)
{-# DEPRECATED lgmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifier for the group to which the members (users or groups) are associated.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmGroupId :: Lens.Lens' ListGroupMembers Lude.Text
lgmGroupId = Lens.lens (groupId :: ListGroupMembers -> Lude.Text) (\s a -> s {groupId = a} :: ListGroupMembers)
{-# DEPRECATED lgmGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmMaxResults :: Lens.Lens' ListGroupMembers (Lude.Maybe Lude.Natural)
lgmMaxResults = Lens.lens (maxResults :: ListGroupMembers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGroupMembers)
{-# DEPRECATED lgmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmOrganizationId :: Lens.Lens' ListGroupMembers Lude.Text
lgmOrganizationId = Lens.lens (organizationId :: ListGroupMembers -> Lude.Text) (\s a -> s {organizationId = a} :: ListGroupMembers)
{-# DEPRECATED lgmOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager ListGroupMembers where
  page rq rs
    | Page.stop (rs Lens.^. lgmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgmrsMembers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgmNextToken Lens..~ rs Lens.^. lgmrsNextToken

instance Lude.AWSRequest ListGroupMembers where
  type Rs ListGroupMembers = ListGroupMembersResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupMembersResponse'
            Lude.<$> (x Lude..?> "Members" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroupMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListGroupMembers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGroupMembers where
  toJSON ListGroupMembers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("GroupId" Lude..= groupId),
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListGroupMembers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGroupMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGroupMembersResponse' smart constructor.
data ListGroupMembersResponse = ListGroupMembersResponse'
  { -- | The members associated to the group.
    members :: Lude.Maybe [Member],
    -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupMembersResponse' with the minimum fields required to make a request.
--
-- * 'members' - The members associated to the group.
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'responseStatus' - The response status code.
mkListGroupMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupMembersResponse
mkListGroupMembersResponse pResponseStatus_ =
  ListGroupMembersResponse'
    { members = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The members associated to the group.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmrsMembers :: Lens.Lens' ListGroupMembersResponse (Lude.Maybe [Member])
lgmrsMembers = Lens.lens (members :: ListGroupMembersResponse -> Lude.Maybe [Member]) (\s a -> s {members = a} :: ListGroupMembersResponse)
{-# DEPRECATED lgmrsMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmrsNextToken :: Lens.Lens' ListGroupMembersResponse (Lude.Maybe Lude.Text)
lgmrsNextToken = Lens.lens (nextToken :: ListGroupMembersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupMembersResponse)
{-# DEPRECATED lgmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgmrsResponseStatus :: Lens.Lens' ListGroupMembersResponse Lude.Int
lgmrsResponseStatus = Lens.lens (responseStatus :: ListGroupMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupMembersResponse)
{-# DEPRECATED lgmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
