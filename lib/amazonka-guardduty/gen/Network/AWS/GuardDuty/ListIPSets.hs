{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IPSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the IPSets returned are the IPSets from the associated master account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListIPSets
  ( -- * Creating a request
    ListIPSets (..),
    mkListIPSets,

    -- ** Request lenses
    lisNextToken,
    lisMaxResults,
    lisDetectorId,

    -- * Destructuring the response
    ListIPSetsResponse (..),
    mkListIPSetsResponse,

    -- ** Response lenses
    lisrsNextToken,
    lisrsResponseStatus,
    lisrsIPSetIds,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListIPSets' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector that the IPSet is associated with.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
mkListIPSets ::
  -- | 'detectorId'
  Lude.Text ->
  ListIPSets
mkListIPSets pDetectorId_ =
  ListIPSets'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisNextToken :: Lens.Lens' ListIPSets (Lude.Maybe Lude.Text)
lisNextToken = Lens.lens (nextToken :: ListIPSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIPSets)
{-# DEPRECATED lisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisMaxResults :: Lens.Lens' ListIPSets (Lude.Maybe Lude.Natural)
lisMaxResults = Lens.lens (maxResults :: ListIPSets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListIPSets)
{-# DEPRECATED lisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique ID of the detector that the IPSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisDetectorId :: Lens.Lens' ListIPSets Lude.Text
lisDetectorId = Lens.lens (detectorId :: ListIPSets -> Lude.Text) (\s a -> s {detectorId = a} :: ListIPSets)
{-# DEPRECATED lisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Page.AWSPager ListIPSets where
  page rq rs
    | Page.stop (rs Lens.^. lisrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lisrsIPSetIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lisNextToken Lens..~ rs Lens.^. lisrsNextToken

instance Lude.AWSRequest ListIPSets where
  type Rs ListIPSets = ListIPSetsResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ipSetIds" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListIPSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListIPSets where
  toPath ListIPSets' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/ipset"]

instance Lude.ToQuery ListIPSets where
  toQuery ListIPSets' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    ipSetIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIPSetsResponse' with the minimum fields required to make a request.
--
-- * 'ipSetIds' - The IDs of the IPSet resources.
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
mkListIPSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIPSetsResponse
mkListIPSetsResponse pResponseStatus_ =
  ListIPSetsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      ipSetIds = Lude.mempty
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisrsNextToken :: Lens.Lens' ListIPSetsResponse (Lude.Maybe Lude.Text)
lisrsNextToken = Lens.lens (nextToken :: ListIPSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIPSetsResponse)
{-# DEPRECATED lisrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisrsResponseStatus :: Lens.Lens' ListIPSetsResponse Lude.Int
lisrsResponseStatus = Lens.lens (responseStatus :: ListIPSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIPSetsResponse)
{-# DEPRECATED lisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The IDs of the IPSet resources.
--
-- /Note:/ Consider using 'ipSetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisrsIPSetIds :: Lens.Lens' ListIPSetsResponse [Lude.Text]
lisrsIPSetIds = Lens.lens (ipSetIds :: ListIPSetsResponse -> [Lude.Text]) (\s a -> s {ipSetIds = a} :: ListIPSetsResponse)
{-# DEPRECATED lisrsIPSetIds "Use generic-lens or generic-optics with 'ipSetIds' instead." #-}
