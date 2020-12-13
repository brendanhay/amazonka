{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListRecordHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified requests or all performed requests.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListRecordHistory
  ( -- * Creating a request
    ListRecordHistory (..),
    mkListRecordHistory,

    -- ** Request lenses
    lrhSearchFilter,
    lrhAcceptLanguage,
    lrhAccessLevelFilter,
    lrhPageToken,
    lrhPageSize,

    -- * Destructuring the response
    ListRecordHistoryResponse (..),
    mkListRecordHistoryResponse,

    -- ** Response lenses
    lrhrsNextPageToken,
    lrhrsRecordDetails,
    lrhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListRecordHistory' smart constructor.
data ListRecordHistory = ListRecordHistory'
  { -- | The search filter to scope the results.
    searchFilter :: Lude.Maybe ListRecordHistorySearchFilter,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The access level to use to obtain results. The default is @User@ .
    accessLevelFilter :: Lude.Maybe AccessLevelFilter,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRecordHistory' with the minimum fields required to make a request.
--
-- * 'searchFilter' - The search filter to scope the results.
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'accessLevelFilter' - The access level to use to obtain results. The default is @User@ .
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
mkListRecordHistory ::
  ListRecordHistory
mkListRecordHistory =
  ListRecordHistory'
    { searchFilter = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      accessLevelFilter = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The search filter to scope the results.
--
-- /Note:/ Consider using 'searchFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhSearchFilter :: Lens.Lens' ListRecordHistory (Lude.Maybe ListRecordHistorySearchFilter)
lrhSearchFilter = Lens.lens (searchFilter :: ListRecordHistory -> Lude.Maybe ListRecordHistorySearchFilter) (\s a -> s {searchFilter = a} :: ListRecordHistory)
{-# DEPRECATED lrhSearchFilter "Use generic-lens or generic-optics with 'searchFilter' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhAcceptLanguage :: Lens.Lens' ListRecordHistory (Lude.Maybe Lude.Text)
lrhAcceptLanguage = Lens.lens (acceptLanguage :: ListRecordHistory -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListRecordHistory)
{-# DEPRECATED lrhAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhAccessLevelFilter :: Lens.Lens' ListRecordHistory (Lude.Maybe AccessLevelFilter)
lrhAccessLevelFilter = Lens.lens (accessLevelFilter :: ListRecordHistory -> Lude.Maybe AccessLevelFilter) (\s a -> s {accessLevelFilter = a} :: ListRecordHistory)
{-# DEPRECATED lrhAccessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhPageToken :: Lens.Lens' ListRecordHistory (Lude.Maybe Lude.Text)
lrhPageToken = Lens.lens (pageToken :: ListRecordHistory -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListRecordHistory)
{-# DEPRECATED lrhPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhPageSize :: Lens.Lens' ListRecordHistory (Lude.Maybe Lude.Natural)
lrhPageSize = Lens.lens (pageSize :: ListRecordHistory -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListRecordHistory)
{-# DEPRECATED lrhPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListRecordHistory where
  page rq rs
    | Page.stop (rs Lens.^. lrhrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrhrsRecordDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrhPageToken Lens..~ rs Lens.^. lrhrsNextPageToken

instance Lude.AWSRequest ListRecordHistory where
  type Rs ListRecordHistory = ListRecordHistoryResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRecordHistoryResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "RecordDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRecordHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListRecordHistory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRecordHistory where
  toJSON ListRecordHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SearchFilter" Lude..=) Lude.<$> searchFilter,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("AccessLevelFilter" Lude..=) Lude.<$> accessLevelFilter,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListRecordHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRecordHistory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRecordHistoryResponse' smart constructor.
data ListRecordHistoryResponse = ListRecordHistoryResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The records, in reverse chronological order.
    recordDetails :: Lude.Maybe [RecordDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRecordHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'recordDetails' - The records, in reverse chronological order.
-- * 'responseStatus' - The response status code.
mkListRecordHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRecordHistoryResponse
mkListRecordHistoryResponse pResponseStatus_ =
  ListRecordHistoryResponse'
    { nextPageToken = Lude.Nothing,
      recordDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhrsNextPageToken :: Lens.Lens' ListRecordHistoryResponse (Lude.Maybe Lude.Text)
lrhrsNextPageToken = Lens.lens (nextPageToken :: ListRecordHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListRecordHistoryResponse)
{-# DEPRECATED lrhrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The records, in reverse chronological order.
--
-- /Note:/ Consider using 'recordDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhrsRecordDetails :: Lens.Lens' ListRecordHistoryResponse (Lude.Maybe [RecordDetail])
lrhrsRecordDetails = Lens.lens (recordDetails :: ListRecordHistoryResponse -> Lude.Maybe [RecordDetail]) (\s a -> s {recordDetails = a} :: ListRecordHistoryResponse)
{-# DEPRECATED lrhrsRecordDetails "Use generic-lens or generic-optics with 'recordDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhrsResponseStatus :: Lens.Lens' ListRecordHistoryResponse Lude.Int
lrhrsResponseStatus = Lens.lens (responseStatus :: ListRecordHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRecordHistoryResponse)
{-# DEPRECATED lrhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
