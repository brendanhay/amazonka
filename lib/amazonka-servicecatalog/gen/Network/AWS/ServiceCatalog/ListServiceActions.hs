{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListServiceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all self-service actions.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActions
  ( -- * Creating a request
    ListServiceActions (..),
    mkListServiceActions,

    -- ** Request lenses
    lsaAcceptLanguage,
    lsaPageToken,
    lsaPageSize,

    -- * Destructuring the response
    ListServiceActionsResponse (..),
    mkListServiceActionsResponse,

    -- ** Response lenses
    lsarsNextPageToken,
    lsarsServiceActionSummaries,
    lsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListServiceActions' smart constructor.
data ListServiceActions = ListServiceActions'
  { -- | The language code.
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
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServiceActions' with the minimum fields required to make a request.
--
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
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
mkListServiceActions ::
  ListServiceActions
mkListServiceActions =
  ListServiceActions'
    { acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

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
lsaAcceptLanguage :: Lens.Lens' ListServiceActions (Lude.Maybe Lude.Text)
lsaAcceptLanguage = Lens.lens (acceptLanguage :: ListServiceActions -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListServiceActions)
{-# DEPRECATED lsaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaPageToken :: Lens.Lens' ListServiceActions (Lude.Maybe Lude.Text)
lsaPageToken = Lens.lens (pageToken :: ListServiceActions -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListServiceActions)
{-# DEPRECATED lsaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaPageSize :: Lens.Lens' ListServiceActions (Lude.Maybe Lude.Natural)
lsaPageSize = Lens.lens (pageSize :: ListServiceActions -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListServiceActions)
{-# DEPRECATED lsaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListServiceActions where
  page rq rs
    | Page.stop (rs Lens.^. lsarsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsarsServiceActionSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsaPageToken Lens..~ rs Lens.^. lsarsNextPageToken

instance Lude.AWSRequest ListServiceActions where
  type Rs ListServiceActions = ListServiceActionsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListServiceActionsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ServiceActionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListServiceActions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListServiceActions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListServiceActions where
  toJSON ListServiceActions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListServiceActions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServiceActions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListServiceActionsResponse' smart constructor.
data ListServiceActionsResponse = ListServiceActionsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An object containing information about the service actions associated with the provisioning artifact.
    serviceActionSummaries :: Lude.Maybe [ServiceActionSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServiceActionsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'serviceActionSummaries' - An object containing information about the service actions associated with the provisioning artifact.
-- * 'responseStatus' - The response status code.
mkListServiceActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServiceActionsResponse
mkListServiceActionsResponse pResponseStatus_ =
  ListServiceActionsResponse'
    { nextPageToken = Lude.Nothing,
      serviceActionSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarsNextPageToken :: Lens.Lens' ListServiceActionsResponse (Lude.Maybe Lude.Text)
lsarsNextPageToken = Lens.lens (nextPageToken :: ListServiceActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListServiceActionsResponse)
{-# DEPRECATED lsarsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object containing information about the service actions associated with the provisioning artifact.
--
-- /Note:/ Consider using 'serviceActionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarsServiceActionSummaries :: Lens.Lens' ListServiceActionsResponse (Lude.Maybe [ServiceActionSummary])
lsarsServiceActionSummaries = Lens.lens (serviceActionSummaries :: ListServiceActionsResponse -> Lude.Maybe [ServiceActionSummary]) (\s a -> s {serviceActionSummaries = a} :: ListServiceActionsResponse)
{-# DEPRECATED lsarsServiceActionSummaries "Use generic-lens or generic-optics with 'serviceActionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarsResponseStatus :: Lens.Lens' ListServiceActionsResponse Lude.Int
lsarsResponseStatus = Lens.lens (responseStatus :: ListServiceActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServiceActionsResponse)
{-# DEPRECATED lsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
