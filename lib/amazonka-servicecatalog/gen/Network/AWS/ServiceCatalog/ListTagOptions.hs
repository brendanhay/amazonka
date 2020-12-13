{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListTagOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified TagOptions or all TagOptions.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListTagOptions
  ( -- * Creating a request
    ListTagOptions (..),
    mkListTagOptions,

    -- ** Request lenses
    ltoFilters,
    ltoPageToken,
    ltoPageSize,

    -- * Destructuring the response
    ListTagOptionsResponse (..),
    mkListTagOptionsResponse,

    -- ** Response lenses
    ltorsPageToken,
    ltorsTagOptionDetails,
    ltorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListTagOptions' smart constructor.
data ListTagOptions = ListTagOptions'
  { -- | The search filters. If no search filters are specified, the output includes all TagOptions.
    filters :: Lude.Maybe ListTagOptionsFilters,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagOptions' with the minimum fields required to make a request.
--
-- * 'filters' - The search filters. If no search filters are specified, the output includes all TagOptions.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
mkListTagOptions ::
  ListTagOptions
mkListTagOptions =
  ListTagOptions'
    { filters = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The search filters. If no search filters are specified, the output includes all TagOptions.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoFilters :: Lens.Lens' ListTagOptions (Lude.Maybe ListTagOptionsFilters)
ltoFilters = Lens.lens (filters :: ListTagOptions -> Lude.Maybe ListTagOptionsFilters) (\s a -> s {filters = a} :: ListTagOptions)
{-# DEPRECATED ltoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPageToken :: Lens.Lens' ListTagOptions (Lude.Maybe Lude.Text)
ltoPageToken = Lens.lens (pageToken :: ListTagOptions -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListTagOptions)
{-# DEPRECATED ltoPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPageSize :: Lens.Lens' ListTagOptions (Lude.Maybe Lude.Natural)
ltoPageSize = Lens.lens (pageSize :: ListTagOptions -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListTagOptions)
{-# DEPRECATED ltoPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListTagOptions where
  page rq rs
    | Page.stop (rs Lens.^. ltorsPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltorsTagOptionDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltoPageToken Lens..~ rs Lens.^. ltorsPageToken

instance Lude.AWSRequest ListTagOptions where
  type Rs ListTagOptions = ListTagOptionsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagOptionsResponse'
            Lude.<$> (x Lude..?> "PageToken")
            Lude.<*> (x Lude..?> "TagOptionDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagOptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.ListTagOptions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagOptions where
  toJSON ListTagOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListTagOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagOptionsResponse' smart constructor.
data ListTagOptionsResponse = ListTagOptionsResponse'
  { -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | Information about the TagOptions.
    tagOptionDetails :: Lude.Maybe [TagOptionDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagOptionsResponse' with the minimum fields required to make a request.
--
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'tagOptionDetails' - Information about the TagOptions.
-- * 'responseStatus' - The response status code.
mkListTagOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagOptionsResponse
mkListTagOptionsResponse pResponseStatus_ =
  ListTagOptionsResponse'
    { pageToken = Lude.Nothing,
      tagOptionDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorsPageToken :: Lens.Lens' ListTagOptionsResponse (Lude.Maybe Lude.Text)
ltorsPageToken = Lens.lens (pageToken :: ListTagOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListTagOptionsResponse)
{-# DEPRECATED ltorsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | Information about the TagOptions.
--
-- /Note:/ Consider using 'tagOptionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorsTagOptionDetails :: Lens.Lens' ListTagOptionsResponse (Lude.Maybe [TagOptionDetail])
ltorsTagOptionDetails = Lens.lens (tagOptionDetails :: ListTagOptionsResponse -> Lude.Maybe [TagOptionDetail]) (\s a -> s {tagOptionDetails = a} :: ListTagOptionsResponse)
{-# DEPRECATED ltorsTagOptionDetails "Use generic-lens or generic-optics with 'tagOptionDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorsResponseStatus :: Lens.Lens' ListTagOptionsResponse Lude.Int
ltorsResponseStatus = Lens.lens (responseStatus :: ListTagOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagOptionsResponse)
{-# DEPRECATED ltorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
