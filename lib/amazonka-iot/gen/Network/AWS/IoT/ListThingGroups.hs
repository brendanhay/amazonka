{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroups
  ( -- * Creating a request
    ListThingGroups (..),
    mkListThingGroups,

    -- ** Request lenses
    ltgNamePrefixFilter,
    ltgParentGroup,
    ltgNextToken,
    ltgRecursive,
    ltgMaxResults,

    -- * Destructuring the response
    ListThingGroupsResponse (..),
    mkListThingGroupsResponse,

    -- ** Response lenses
    ltgrsThingGroups,
    ltgrsNextToken,
    ltgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThingGroups' smart constructor.
data ListThingGroups = ListThingGroups'
  { -- | A filter that limits the results to those with the specified name prefix.
    namePrefixFilter :: Lude.Maybe Lude.Text,
    -- | A filter that limits the results to those with the specified parent group.
    parentGroup :: Lude.Maybe Lude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | If true, return child groups as well.
    recursive :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingGroups' with the minimum fields required to make a request.
--
-- * 'namePrefixFilter' - A filter that limits the results to those with the specified name prefix.
-- * 'parentGroup' - A filter that limits the results to those with the specified parent group.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'recursive' - If true, return child groups as well.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListThingGroups ::
  ListThingGroups
mkListThingGroups =
  ListThingGroups'
    { namePrefixFilter = Lude.Nothing,
      parentGroup = Lude.Nothing,
      nextToken = Lude.Nothing,
      recursive = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A filter that limits the results to those with the specified name prefix.
--
-- /Note:/ Consider using 'namePrefixFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgNamePrefixFilter :: Lens.Lens' ListThingGroups (Lude.Maybe Lude.Text)
ltgNamePrefixFilter = Lens.lens (namePrefixFilter :: ListThingGroups -> Lude.Maybe Lude.Text) (\s a -> s {namePrefixFilter = a} :: ListThingGroups)
{-# DEPRECATED ltgNamePrefixFilter "Use generic-lens or generic-optics with 'namePrefixFilter' instead." #-}

-- | A filter that limits the results to those with the specified parent group.
--
-- /Note:/ Consider using 'parentGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgParentGroup :: Lens.Lens' ListThingGroups (Lude.Maybe Lude.Text)
ltgParentGroup = Lens.lens (parentGroup :: ListThingGroups -> Lude.Maybe Lude.Text) (\s a -> s {parentGroup = a} :: ListThingGroups)
{-# DEPRECATED ltgParentGroup "Use generic-lens or generic-optics with 'parentGroup' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgNextToken :: Lens.Lens' ListThingGroups (Lude.Maybe Lude.Text)
ltgNextToken = Lens.lens (nextToken :: ListThingGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingGroups)
{-# DEPRECATED ltgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If true, return child groups as well.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgRecursive :: Lens.Lens' ListThingGroups (Lude.Maybe Lude.Bool)
ltgRecursive = Lens.lens (recursive :: ListThingGroups -> Lude.Maybe Lude.Bool) (\s a -> s {recursive = a} :: ListThingGroups)
{-# DEPRECATED ltgRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgMaxResults :: Lens.Lens' ListThingGroups (Lude.Maybe Lude.Natural)
ltgMaxResults = Lens.lens (maxResults :: ListThingGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingGroups)
{-# DEPRECATED ltgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListThingGroups where
  page rq rs
    | Page.stop (rs Lens.^. ltgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltgrsThingGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltgNextToken Lens..~ rs Lens.^. ltgrsNextToken

instance Lude.AWSRequest ListThingGroups where
  type Rs ListThingGroups = ListThingGroupsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingGroupsResponse'
            Lude.<$> (x Lude..?> "thingGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingGroups where
  toPath = Lude.const "/thing-groups"

instance Lude.ToQuery ListThingGroups where
  toQuery ListThingGroups' {..} =
    Lude.mconcat
      [ "namePrefixFilter" Lude.=: namePrefixFilter,
        "parentGroup" Lude.=: parentGroup,
        "nextToken" Lude.=: nextToken,
        "recursive" Lude.=: recursive,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListThingGroupsResponse' smart constructor.
data ListThingGroupsResponse = ListThingGroupsResponse'
  { -- | The thing groups.
    thingGroups :: Lude.Maybe [GroupNameAndARN],
    -- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingGroupsResponse' with the minimum fields required to make a request.
--
-- * 'thingGroups' - The thing groups.
-- * 'nextToken' - The token to use to get the next set of results. Will not be returned if operation has returned all results.
-- * 'responseStatus' - The response status code.
mkListThingGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingGroupsResponse
mkListThingGroupsResponse pResponseStatus_ =
  ListThingGroupsResponse'
    { thingGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The thing groups.
--
-- /Note:/ Consider using 'thingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgrsThingGroups :: Lens.Lens' ListThingGroupsResponse (Lude.Maybe [GroupNameAndARN])
ltgrsThingGroups = Lens.lens (thingGroups :: ListThingGroupsResponse -> Lude.Maybe [GroupNameAndARN]) (\s a -> s {thingGroups = a} :: ListThingGroupsResponse)
{-# DEPRECATED ltgrsThingGroups "Use generic-lens or generic-optics with 'thingGroups' instead." #-}

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgrsNextToken :: Lens.Lens' ListThingGroupsResponse (Lude.Maybe Lude.Text)
ltgrsNextToken = Lens.lens (nextToken :: ListThingGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingGroupsResponse)
{-# DEPRECATED ltgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgrsResponseStatus :: Lens.Lens' ListThingGroupsResponse Lude.Int
ltgrsResponseStatus = Lens.lens (responseStatus :: ListThingGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingGroupsResponse)
{-# DEPRECATED ltgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
