{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingsInThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things in the specified group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInThingGroup
  ( -- * Creating a request
    ListThingsInThingGroup (..),
    mkListThingsInThingGroup,

    -- ** Request lenses
    ltitgNextToken,
    ltitgRecursive,
    ltitgMaxResults,
    ltitgThingGroupName,

    -- * Destructuring the response
    ListThingsInThingGroupResponse (..),
    mkListThingsInThingGroupResponse,

    -- ** Response lenses
    ltitgrsNextToken,
    ltitgrsThings,
    ltitgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThingsInThingGroup' smart constructor.
data ListThingsInThingGroup = ListThingsInThingGroup'
  { nextToken ::
      Lude.Maybe Lude.Text,
    recursive :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    thingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingsInThingGroup' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'recursive' - When true, list things in this thing group and in all child groups as well.
-- * 'thingGroupName' - The thing group name.
mkListThingsInThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  ListThingsInThingGroup
mkListThingsInThingGroup pThingGroupName_ =
  ListThingsInThingGroup'
    { nextToken = Lude.Nothing,
      recursive = Lude.Nothing,
      maxResults = Lude.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgNextToken :: Lens.Lens' ListThingsInThingGroup (Lude.Maybe Lude.Text)
ltitgNextToken = Lens.lens (nextToken :: ListThingsInThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingsInThingGroup)
{-# DEPRECATED ltitgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When true, list things in this thing group and in all child groups as well.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgRecursive :: Lens.Lens' ListThingsInThingGroup (Lude.Maybe Lude.Bool)
ltitgRecursive = Lens.lens (recursive :: ListThingsInThingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {recursive = a} :: ListThingsInThingGroup)
{-# DEPRECATED ltitgRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgMaxResults :: Lens.Lens' ListThingsInThingGroup (Lude.Maybe Lude.Natural)
ltitgMaxResults = Lens.lens (maxResults :: ListThingsInThingGroup -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingsInThingGroup)
{-# DEPRECATED ltitgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgThingGroupName :: Lens.Lens' ListThingsInThingGroup Lude.Text
ltitgThingGroupName = Lens.lens (thingGroupName :: ListThingsInThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: ListThingsInThingGroup)
{-# DEPRECATED ltitgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Page.AWSPager ListThingsInThingGroup where
  page rq rs
    | Page.stop (rs Lens.^. ltitgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltitgrsThings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltitgNextToken Lens..~ rs Lens.^. ltitgrsNextToken

instance Lude.AWSRequest ListThingsInThingGroup where
  type Rs ListThingsInThingGroup = ListThingsInThingGroupResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingsInThingGroupResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "things" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingsInThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingsInThingGroup where
  toPath ListThingsInThingGroup' {..} =
    Lude.mconcat
      ["/thing-groups/", Lude.toBS thingGroupName, "/things"]

instance Lude.ToQuery ListThingsInThingGroup where
  toQuery ListThingsInThingGroup' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "recursive" Lude.=: recursive,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListThingsInThingGroupResponse' smart constructor.
data ListThingsInThingGroupResponse = ListThingsInThingGroupResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    things ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListThingsInThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'things' - The things in the specified thing group.
mkListThingsInThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingsInThingGroupResponse
mkListThingsInThingGroupResponse pResponseStatus_ =
  ListThingsInThingGroupResponse'
    { nextToken = Lude.Nothing,
      things = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrsNextToken :: Lens.Lens' ListThingsInThingGroupResponse (Lude.Maybe Lude.Text)
ltitgrsNextToken = Lens.lens (nextToken :: ListThingsInThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingsInThingGroupResponse)
{-# DEPRECATED ltitgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The things in the specified thing group.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrsThings :: Lens.Lens' ListThingsInThingGroupResponse (Lude.Maybe [Lude.Text])
ltitgrsThings = Lens.lens (things :: ListThingsInThingGroupResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {things = a} :: ListThingsInThingGroupResponse)
{-# DEPRECATED ltitgrsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrsResponseStatus :: Lens.Lens' ListThingsInThingGroupResponse Lude.Int
ltitgrsResponseStatus = Lens.lens (responseStatus :: ListThingsInThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingsInThingGroupResponse)
{-# DEPRECATED ltitgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
