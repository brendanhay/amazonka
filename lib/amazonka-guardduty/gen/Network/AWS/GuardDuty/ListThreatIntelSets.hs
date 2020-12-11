{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListThreatIntelSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the ThreatIntelSets associated with the master account are returned.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListThreatIntelSets
  ( -- * Creating a request
    ListThreatIntelSets (..),
    mkListThreatIntelSets,

    -- ** Request lenses
    ltisNextToken,
    ltisMaxResults,
    ltisDetectorId,

    -- * Destructuring the response
    ListThreatIntelSetsResponse (..),
    mkListThreatIntelSetsResponse,

    -- ** Response lenses
    ltisrsNextToken,
    ltisrsResponseStatus,
    ltisrsThreatIntelSetIds,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThreatIntelSets' smart constructor.
data ListThreatIntelSets = ListThreatIntelSets'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    detectorId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThreatIntelSets' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector that the threatIntelSet is associated with.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
-- * 'nextToken' - You can use this parameter to paginate results in the response. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
mkListThreatIntelSets ::
  -- | 'detectorId'
  Lude.Text ->
  ListThreatIntelSets
mkListThreatIntelSets pDetectorId_ =
  ListThreatIntelSets'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter to paginate results in the response. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisNextToken :: Lens.Lens' ListThreatIntelSets (Lude.Maybe Lude.Text)
ltisNextToken = Lens.lens (nextToken :: ListThreatIntelSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThreatIntelSets)
{-# DEPRECATED ltisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisMaxResults :: Lens.Lens' ListThreatIntelSets (Lude.Maybe Lude.Natural)
ltisMaxResults = Lens.lens (maxResults :: ListThreatIntelSets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThreatIntelSets)
{-# DEPRECATED ltisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique ID of the detector that the threatIntelSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisDetectorId :: Lens.Lens' ListThreatIntelSets Lude.Text
ltisDetectorId = Lens.lens (detectorId :: ListThreatIntelSets -> Lude.Text) (\s a -> s {detectorId = a} :: ListThreatIntelSets)
{-# DEPRECATED ltisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Page.AWSPager ListThreatIntelSets where
  page rq rs
    | Page.stop (rs Lens.^. ltisrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltisrsThreatIntelSetIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltisNextToken Lens..~ rs Lens.^. ltisrsNextToken

instance Lude.AWSRequest ListThreatIntelSets where
  type Rs ListThreatIntelSets = ListThreatIntelSetsResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThreatIntelSetsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "threatIntelSetIds" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListThreatIntelSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListThreatIntelSets where
  toPath ListThreatIntelSets' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/threatintelset"]

instance Lude.ToQuery ListThreatIntelSets where
  toQuery ListThreatIntelSets' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListThreatIntelSetsResponse' smart constructor.
data ListThreatIntelSetsResponse = ListThreatIntelSetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    threatIntelSetIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThreatIntelSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
-- * 'threatIntelSetIds' - The IDs of the ThreatIntelSet resources.
mkListThreatIntelSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThreatIntelSetsResponse
mkListThreatIntelSetsResponse pResponseStatus_ =
  ListThreatIntelSetsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      threatIntelSetIds = Lude.mempty
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisrsNextToken :: Lens.Lens' ListThreatIntelSetsResponse (Lude.Maybe Lude.Text)
ltisrsNextToken = Lens.lens (nextToken :: ListThreatIntelSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThreatIntelSetsResponse)
{-# DEPRECATED ltisrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisrsResponseStatus :: Lens.Lens' ListThreatIntelSetsResponse Lude.Int
ltisrsResponseStatus = Lens.lens (responseStatus :: ListThreatIntelSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThreatIntelSetsResponse)
{-# DEPRECATED ltisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The IDs of the ThreatIntelSet resources.
--
-- /Note:/ Consider using 'threatIntelSetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisrsThreatIntelSetIds :: Lens.Lens' ListThreatIntelSetsResponse [Lude.Text]
ltisrsThreatIntelSetIds = Lens.lens (threatIntelSetIds :: ListThreatIntelSetsResponse -> [Lude.Text]) (\s a -> s {threatIntelSetIds = a} :: ListThreatIntelSetsResponse)
{-# DEPRECATED ltisrsThreatIntelSetIds "Use generic-lens or generic-optics with 'threatIntelSetIds' instead." #-}
