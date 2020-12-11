{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListResourcesForTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources associated with the specified TagOption.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListResourcesForTagOption
  ( -- * Creating a request
    ListResourcesForTagOption (..),
    mkListResourcesForTagOption,

    -- ** Request lenses
    lrftoResourceType,
    lrftoPageToken,
    lrftoPageSize,
    lrftoTagOptionId,

    -- * Destructuring the response
    ListResourcesForTagOptionResponse (..),
    mkListResourcesForTagOptionResponse,

    -- ** Response lenses
    lrftorsResourceDetails,
    lrftorsPageToken,
    lrftorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListResourcesForTagOption' smart constructor.
data ListResourcesForTagOption = ListResourcesForTagOption'
  { resourceType ::
      Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural,
    tagOptionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesForTagOption' with the minimum fields required to make a request.
--
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'resourceType' - The resource type.
--
--
--     * @Portfolio@
--
--
--     * @Product@
--
--
-- * 'tagOptionId' - The TagOption identifier.
mkListResourcesForTagOption ::
  -- | 'tagOptionId'
  Lude.Text ->
  ListResourcesForTagOption
mkListResourcesForTagOption pTagOptionId_ =
  ListResourcesForTagOption'
    { resourceType = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      tagOptionId = pTagOptionId_
    }

-- | The resource type.
--
--
--     * @Portfolio@
--
--
--     * @Product@
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoResourceType :: Lens.Lens' ListResourcesForTagOption (Lude.Maybe Lude.Text)
lrftoResourceType = Lens.lens (resourceType :: ListResourcesForTagOption -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ListResourcesForTagOption)
{-# DEPRECATED lrftoResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoPageToken :: Lens.Lens' ListResourcesForTagOption (Lude.Maybe Lude.Text)
lrftoPageToken = Lens.lens (pageToken :: ListResourcesForTagOption -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListResourcesForTagOption)
{-# DEPRECATED lrftoPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoPageSize :: Lens.Lens' ListResourcesForTagOption (Lude.Maybe Lude.Natural)
lrftoPageSize = Lens.lens (pageSize :: ListResourcesForTagOption -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListResourcesForTagOption)
{-# DEPRECATED lrftoPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoTagOptionId :: Lens.Lens' ListResourcesForTagOption Lude.Text
lrftoTagOptionId = Lens.lens (tagOptionId :: ListResourcesForTagOption -> Lude.Text) (\s a -> s {tagOptionId = a} :: ListResourcesForTagOption)
{-# DEPRECATED lrftoTagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead." #-}

instance Page.AWSPager ListResourcesForTagOption where
  page rq rs
    | Page.stop (rs Lens.^. lrftorsPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrftorsResourceDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrftoPageToken Lens..~ rs Lens.^. lrftorsPageToken

instance Lude.AWSRequest ListResourcesForTagOption where
  type
    Rs ListResourcesForTagOption =
      ListResourcesForTagOptionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourcesForTagOptionResponse'
            Lude.<$> (x Lude..?> "ResourceDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourcesForTagOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListResourcesForTagOption" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourcesForTagOption where
  toJSON ListResourcesForTagOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("TagOptionId" Lude..= tagOptionId)
          ]
      )

instance Lude.ToPath ListResourcesForTagOption where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourcesForTagOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourcesForTagOptionResponse' smart constructor.
data ListResourcesForTagOptionResponse = ListResourcesForTagOptionResponse'
  { resourceDetails ::
      Lude.Maybe
        [ResourceDetail],
    pageToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesForTagOptionResponse' with the minimum fields required to make a request.
--
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'resourceDetails' - Information about the resources.
-- * 'responseStatus' - The response status code.
mkListResourcesForTagOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourcesForTagOptionResponse
mkListResourcesForTagOptionResponse pResponseStatus_ =
  ListResourcesForTagOptionResponse'
    { resourceDetails =
        Lude.Nothing,
      pageToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the resources.
--
-- /Note:/ Consider using 'resourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftorsResourceDetails :: Lens.Lens' ListResourcesForTagOptionResponse (Lude.Maybe [ResourceDetail])
lrftorsResourceDetails = Lens.lens (resourceDetails :: ListResourcesForTagOptionResponse -> Lude.Maybe [ResourceDetail]) (\s a -> s {resourceDetails = a} :: ListResourcesForTagOptionResponse)
{-# DEPRECATED lrftorsResourceDetails "Use generic-lens or generic-optics with 'resourceDetails' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftorsPageToken :: Lens.Lens' ListResourcesForTagOptionResponse (Lude.Maybe Lude.Text)
lrftorsPageToken = Lens.lens (pageToken :: ListResourcesForTagOptionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListResourcesForTagOptionResponse)
{-# DEPRECATED lrftorsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftorsResponseStatus :: Lens.Lens' ListResourcesForTagOptionResponse Lude.Int
lrftorsResponseStatus = Lens.lens (responseStatus :: ListResourcesForTagOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourcesForTagOptionResponse)
{-# DEPRECATED lrftorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
