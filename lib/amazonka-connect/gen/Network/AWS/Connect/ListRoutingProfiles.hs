{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListRoutingProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the routing profiles for the specified Amazon Connect instance.
--
-- For more information about routing profiles, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing.html Routing Profiles> and <https://docs.aws.amazon.com/connect/latest/adminguide/routing-profiles.html Create a Routing Profile> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfiles
  ( -- * Creating a request
    ListRoutingProfiles (..),
    mkListRoutingProfiles,

    -- ** Request lenses
    lrpInstanceId,
    lrpNextToken,
    lrpMaxResults,

    -- * Destructuring the response
    ListRoutingProfilesResponse (..),
    mkListRoutingProfilesResponse,

    -- ** Response lenses
    lrprsRoutingProfileSummaryList,
    lrprsNextToken,
    lrprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRoutingProfiles' smart constructor.
data ListRoutingProfiles = ListRoutingProfiles'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximimum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRoutingProfiles' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximimum number of results to return per page.
mkListRoutingProfiles ::
  -- | 'instanceId'
  Lude.Text ->
  ListRoutingProfiles
mkListRoutingProfiles pInstanceId_ =
  ListRoutingProfiles'
    { instanceId = pInstanceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpInstanceId :: Lens.Lens' ListRoutingProfiles Lude.Text
lrpInstanceId = Lens.lens (instanceId :: ListRoutingProfiles -> Lude.Text) (\s a -> s {instanceId = a} :: ListRoutingProfiles)
{-# DEPRECATED lrpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpNextToken :: Lens.Lens' ListRoutingProfiles (Lude.Maybe Lude.Text)
lrpNextToken = Lens.lens (nextToken :: ListRoutingProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRoutingProfiles)
{-# DEPRECATED lrpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMaxResults :: Lens.Lens' ListRoutingProfiles (Lude.Maybe Lude.Natural)
lrpMaxResults = Lens.lens (maxResults :: ListRoutingProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListRoutingProfiles)
{-# DEPRECATED lrpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListRoutingProfiles where
  page rq rs
    | Page.stop (rs Lens.^. lrprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrprsRoutingProfileSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrpNextToken Lens..~ rs Lens.^. lrprsNextToken

instance Lude.AWSRequest ListRoutingProfiles where
  type Rs ListRoutingProfiles = ListRoutingProfilesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRoutingProfilesResponse'
            Lude.<$> (x Lude..?> "RoutingProfileSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRoutingProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListRoutingProfiles where
  toPath ListRoutingProfiles' {..} =
    Lude.mconcat ["/routing-profiles-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListRoutingProfiles where
  toQuery ListRoutingProfiles' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListRoutingProfilesResponse' smart constructor.
data ListRoutingProfilesResponse = ListRoutingProfilesResponse'
  { -- | Information about the routing profiles.
    routingProfileSummaryList :: Lude.Maybe [RoutingProfileSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRoutingProfilesResponse' with the minimum fields required to make a request.
--
-- * 'routingProfileSummaryList' - Information about the routing profiles.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListRoutingProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRoutingProfilesResponse
mkListRoutingProfilesResponse pResponseStatus_ =
  ListRoutingProfilesResponse'
    { routingProfileSummaryList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the routing profiles.
--
-- /Note:/ Consider using 'routingProfileSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsRoutingProfileSummaryList :: Lens.Lens' ListRoutingProfilesResponse (Lude.Maybe [RoutingProfileSummary])
lrprsRoutingProfileSummaryList = Lens.lens (routingProfileSummaryList :: ListRoutingProfilesResponse -> Lude.Maybe [RoutingProfileSummary]) (\s a -> s {routingProfileSummaryList = a} :: ListRoutingProfilesResponse)
{-# DEPRECATED lrprsRoutingProfileSummaryList "Use generic-lens or generic-optics with 'routingProfileSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsNextToken :: Lens.Lens' ListRoutingProfilesResponse (Lude.Maybe Lude.Text)
lrprsNextToken = Lens.lens (nextToken :: ListRoutingProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRoutingProfilesResponse)
{-# DEPRECATED lrprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsResponseStatus :: Lens.Lens' ListRoutingProfilesResponse Lude.Int
lrprsResponseStatus = Lens.lens (responseStatus :: ListRoutingProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRoutingProfilesResponse)
{-# DEPRECATED lrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
