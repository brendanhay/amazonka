{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingGroupsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups to which the specified thing belongs.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroupsForThing
  ( -- * Creating a request
    ListThingGroupsForThing (..),
    mkListThingGroupsForThing,

    -- ** Request lenses
    ltgftNextToken,
    ltgftMaxResults,
    ltgftThingName,

    -- * Destructuring the response
    ListThingGroupsForThingResponse (..),
    mkListThingGroupsForThingResponse,

    -- ** Response lenses
    ltgftrsThingGroups,
    ltgftrsNextToken,
    ltgftrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThingGroupsForThing' smart constructor.
data ListThingGroupsForThing = ListThingGroupsForThing'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    thingName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingGroupsForThing' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'thingName' - The thing name.
mkListThingGroupsForThing ::
  -- | 'thingName'
  Lude.Text ->
  ListThingGroupsForThing
mkListThingGroupsForThing pThingName_ =
  ListThingGroupsForThing'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      thingName = pThingName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftNextToken :: Lens.Lens' ListThingGroupsForThing (Lude.Maybe Lude.Text)
ltgftNextToken = Lens.lens (nextToken :: ListThingGroupsForThing -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingGroupsForThing)
{-# DEPRECATED ltgftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftMaxResults :: Lens.Lens' ListThingGroupsForThing (Lude.Maybe Lude.Natural)
ltgftMaxResults = Lens.lens (maxResults :: ListThingGroupsForThing -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingGroupsForThing)
{-# DEPRECATED ltgftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftThingName :: Lens.Lens' ListThingGroupsForThing Lude.Text
ltgftThingName = Lens.lens (thingName :: ListThingGroupsForThing -> Lude.Text) (\s a -> s {thingName = a} :: ListThingGroupsForThing)
{-# DEPRECATED ltgftThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Page.AWSPager ListThingGroupsForThing where
  page rq rs
    | Page.stop (rs Lens.^. ltgftrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltgftrsThingGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltgftNextToken Lens..~ rs Lens.^. ltgftrsNextToken

instance Lude.AWSRequest ListThingGroupsForThing where
  type Rs ListThingGroupsForThing = ListThingGroupsForThingResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingGroupsForThingResponse'
            Lude.<$> (x Lude..?> "thingGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingGroupsForThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingGroupsForThing where
  toPath ListThingGroupsForThing' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/thing-groups"]

instance Lude.ToQuery ListThingGroupsForThing where
  toQuery ListThingGroupsForThing' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListThingGroupsForThingResponse' smart constructor.
data ListThingGroupsForThingResponse = ListThingGroupsForThingResponse'
  { thingGroups ::
      Lude.Maybe
        [GroupNameAndARN],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListThingGroupsForThingResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'thingGroups' - The thing groups.
mkListThingGroupsForThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingGroupsForThingResponse
mkListThingGroupsForThingResponse pResponseStatus_ =
  ListThingGroupsForThingResponse'
    { thingGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The thing groups.
--
-- /Note:/ Consider using 'thingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftrsThingGroups :: Lens.Lens' ListThingGroupsForThingResponse (Lude.Maybe [GroupNameAndARN])
ltgftrsThingGroups = Lens.lens (thingGroups :: ListThingGroupsForThingResponse -> Lude.Maybe [GroupNameAndARN]) (\s a -> s {thingGroups = a} :: ListThingGroupsForThingResponse)
{-# DEPRECATED ltgftrsThingGroups "Use generic-lens or generic-optics with 'thingGroups' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftrsNextToken :: Lens.Lens' ListThingGroupsForThingResponse (Lude.Maybe Lude.Text)
ltgftrsNextToken = Lens.lens (nextToken :: ListThingGroupsForThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingGroupsForThingResponse)
{-# DEPRECATED ltgftrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftrsResponseStatus :: Lens.Lens' ListThingGroupsForThingResponse Lude.Int
ltgftrsResponseStatus = Lens.lens (responseStatus :: ListThingGroupsForThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingGroupsForThingResponse)
{-# DEPRECATED ltgftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
