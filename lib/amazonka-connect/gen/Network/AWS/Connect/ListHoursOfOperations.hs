{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListHoursOfOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the hours of operation for the specified Amazon Connect instance.
--
-- For more information about hours of operation, see <https://docs.aws.amazon.com/connect/latest/adminguide/set-hours-operation.html Set the Hours of Operation for a Queue> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListHoursOfOperations
  ( -- * Creating a request
    ListHoursOfOperations (..),
    mkListHoursOfOperations,

    -- ** Request lenses
    lhooNextToken,
    lhooMaxResults,
    lhooInstanceId,

    -- * Destructuring the response
    ListHoursOfOperationsResponse (..),
    mkListHoursOfOperationsResponse,

    -- ** Response lenses
    lhoorsNextToken,
    lhoorsHoursOfOperationSummaryList,
    lhoorsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListHoursOfOperations' smart constructor.
data ListHoursOfOperations = ListHoursOfOperations'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHoursOfOperations' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListHoursOfOperations ::
  -- | 'instanceId'
  Lude.Text ->
  ListHoursOfOperations
mkListHoursOfOperations pInstanceId_ =
  ListHoursOfOperations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhooNextToken :: Lens.Lens' ListHoursOfOperations (Lude.Maybe Lude.Text)
lhooNextToken = Lens.lens (nextToken :: ListHoursOfOperations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHoursOfOperations)
{-# DEPRECATED lhooNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhooMaxResults :: Lens.Lens' ListHoursOfOperations (Lude.Maybe Lude.Natural)
lhooMaxResults = Lens.lens (maxResults :: ListHoursOfOperations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListHoursOfOperations)
{-# DEPRECATED lhooMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhooInstanceId :: Lens.Lens' ListHoursOfOperations Lude.Text
lhooInstanceId = Lens.lens (instanceId :: ListHoursOfOperations -> Lude.Text) (\s a -> s {instanceId = a} :: ListHoursOfOperations)
{-# DEPRECATED lhooInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListHoursOfOperations where
  page rq rs
    | Page.stop (rs Lens.^. lhoorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhoorsHoursOfOperationSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhooNextToken Lens..~ rs Lens.^. lhoorsNextToken

instance Lude.AWSRequest ListHoursOfOperations where
  type Rs ListHoursOfOperations = ListHoursOfOperationsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHoursOfOperationsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "HoursOfOperationSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHoursOfOperations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListHoursOfOperations where
  toPath ListHoursOfOperations' {..} =
    Lude.mconcat
      ["/hours-of-operations-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListHoursOfOperations where
  toQuery ListHoursOfOperations' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListHoursOfOperationsResponse' smart constructor.
data ListHoursOfOperationsResponse = ListHoursOfOperationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    hoursOfOperationSummaryList ::
      Lude.Maybe
        [HoursOfOperationSummary],
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

-- | Creates a value of 'ListHoursOfOperationsResponse' with the minimum fields required to make a request.
--
-- * 'hoursOfOperationSummaryList' - Information about the hours of operation.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListHoursOfOperationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHoursOfOperationsResponse
mkListHoursOfOperationsResponse pResponseStatus_ =
  ListHoursOfOperationsResponse'
    { nextToken = Lude.Nothing,
      hoursOfOperationSummaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhoorsNextToken :: Lens.Lens' ListHoursOfOperationsResponse (Lude.Maybe Lude.Text)
lhoorsNextToken = Lens.lens (nextToken :: ListHoursOfOperationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHoursOfOperationsResponse)
{-# DEPRECATED lhoorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the hours of operation.
--
-- /Note:/ Consider using 'hoursOfOperationSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhoorsHoursOfOperationSummaryList :: Lens.Lens' ListHoursOfOperationsResponse (Lude.Maybe [HoursOfOperationSummary])
lhoorsHoursOfOperationSummaryList = Lens.lens (hoursOfOperationSummaryList :: ListHoursOfOperationsResponse -> Lude.Maybe [HoursOfOperationSummary]) (\s a -> s {hoursOfOperationSummaryList = a} :: ListHoursOfOperationsResponse)
{-# DEPRECATED lhoorsHoursOfOperationSummaryList "Use generic-lens or generic-optics with 'hoursOfOperationSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhoorsResponseStatus :: Lens.Lens' ListHoursOfOperationsResponse Lude.Int
lhoorsResponseStatus = Lens.lens (responseStatus :: ListHoursOfOperationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHoursOfOperationsResponse)
{-# DEPRECATED lhoorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
