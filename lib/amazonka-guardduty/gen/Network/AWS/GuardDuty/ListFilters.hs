{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of the current filters.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFilters
  ( -- * Creating a request
    ListFilters (..),
    mkListFilters,

    -- ** Request lenses
    lfNextToken,
    lfDetectorId,
    lfMaxResults,

    -- * Destructuring the response
    ListFiltersResponse (..),
    mkListFiltersResponse,

    -- ** Response lenses
    lrsFilterNames,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFilters' smart constructor.
data ListFilters = ListFilters'
  { -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Lude.Text,
    -- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFilters' with the minimum fields required to make a request.
--
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
-- * 'detectorId' - The unique ID of the detector that the filter is associated with.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
mkListFilters ::
  -- | 'detectorId'
  Lude.Text ->
  ListFilters
mkListFilters pDetectorId_ =
  ListFilters'
    { nextToken = Lude.Nothing,
      detectorId = pDetectorId_,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFilters (Lude.Maybe Lude.Text)
lfNextToken = Lens.lens (nextToken :: ListFilters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFilters)
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfDetectorId :: Lens.Lens' ListFilters Lude.Text
lfDetectorId = Lens.lens (detectorId :: ListFilters -> Lude.Text) (\s a -> s {detectorId = a} :: ListFilters)
{-# DEPRECATED lfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFilters (Lude.Maybe Lude.Natural)
lfMaxResults = Lens.lens (maxResults :: ListFilters -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFilters)
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFilters where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsFilterNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListFilters where
  type Rs ListFilters = ListFiltersResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFiltersResponse'
            Lude.<$> (x Lude..?> "filterNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFilters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListFilters where
  toPath ListFilters' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/filter"]

instance Lude.ToQuery ListFilters where
  toQuery ListFilters' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { -- | A list of filter names.
    filterNames :: [Lude.Text],
    -- | The pagination parameter to be used on the next list operation to retrieve more items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFiltersResponse' with the minimum fields required to make a request.
--
-- * 'filterNames' - A list of filter names.
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
mkListFiltersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFiltersResponse
mkListFiltersResponse pResponseStatus_ =
  ListFiltersResponse'
    { filterNames = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of filter names.
--
-- /Note:/ Consider using 'filterNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsFilterNames :: Lens.Lens' ListFiltersResponse [Lude.Text]
lrsFilterNames = Lens.lens (filterNames :: ListFiltersResponse -> [Lude.Text]) (\s a -> s {filterNames = a} :: ListFiltersResponse)
{-# DEPRECATED lrsFilterNames "Use generic-lens or generic-optics with 'filterNames' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListFiltersResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListFiltersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFiltersResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListFiltersResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListFiltersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFiltersResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
