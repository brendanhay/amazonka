{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists detectorIds of all the existing Amazon GuardDuty detector resources.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListDetectors
  ( -- * Creating a request
    ListDetectors (..),
    mkListDetectors,

    -- ** Request lenses
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the response
    ListDetectorsResponse (..),
    mkListDetectorsResponse,

    -- ** Response lenses
    ldrsNextToken,
    ldrsDetectorIds,
    ldrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDetectors' smart constructor.
data ListDetectors = ListDetectors'
  { -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDetectors' with the minimum fields required to make a request.
--
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
mkListDetectors ::
  ListDetectors
mkListDetectors =
  ListDetectors'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDetectors (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDetectors -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDetectors)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDetectors (Lude.Maybe Lude.Natural)
ldMaxResults = Lens.lens (maxResults :: ListDetectors -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDetectors)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDetectors where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDetectorIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDetectors where
  type Rs ListDetectors = ListDetectorsResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDetectorsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "detectorIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDetectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListDetectors where
  toPath = Lude.const "/detector"

instance Lude.ToQuery ListDetectors where
  toQuery ListDetectors' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDetectorsResponse' smart constructor.
data ListDetectorsResponse = ListDetectorsResponse'
  { -- | The pagination parameter to be used on the next list operation to retrieve more items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of detector IDs.
    detectorIds :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDetectorsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'detectorIds' - A list of detector IDs.
-- * 'responseStatus' - The response status code.
mkListDetectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDetectorsResponse
mkListDetectorsResponse pResponseStatus_ =
  ListDetectorsResponse'
    { nextToken = Lude.Nothing,
      detectorIds = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDetectorsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDetectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDetectorsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of detector IDs.
--
-- /Note:/ Consider using 'detectorIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDetectorIds :: Lens.Lens' ListDetectorsResponse [Lude.Text]
ldrsDetectorIds = Lens.lens (detectorIds :: ListDetectorsResponse -> [Lude.Text]) (\s a -> s {detectorIds = a} :: ListDetectorsResponse)
{-# DEPRECATED ldrsDetectorIds "Use generic-lens or generic-optics with 'detectorIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDetectorsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDetectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDetectorsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
