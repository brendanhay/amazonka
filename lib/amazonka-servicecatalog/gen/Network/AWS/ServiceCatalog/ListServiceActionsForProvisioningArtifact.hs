{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of self-service actions associated with the specified Product ID and Provisioning Artifact ID.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
  ( -- * Creating a request
    ListServiceActionsForProvisioningArtifact (..),
    mkListServiceActionsForProvisioningArtifact,

    -- ** Request lenses
    lsafpaProvisioningArtifactId,
    lsafpaAcceptLanguage,
    lsafpaPageToken,
    lsafpaPageSize,
    lsafpaProductId,

    -- * Destructuring the response
    ListServiceActionsForProvisioningArtifactResponse (..),
    mkListServiceActionsForProvisioningArtifactResponse,

    -- ** Response lenses
    lsafparsNextPageToken,
    lsafparsServiceActionSummaries,
    lsafparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListServiceActionsForProvisioningArtifact' smart constructor.
data ListServiceActionsForProvisioningArtifact = ListServiceActionsForProvisioningArtifact'
  { -- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
    provisioningArtifactId :: Lude.Text,
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
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
    productId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServiceActionsForProvisioningArtifact' with the minimum fields required to make a request.
--
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
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
-- * 'productId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
mkListServiceActionsForProvisioningArtifact ::
  -- | 'provisioningArtifactId'
  Lude.Text ->
  -- | 'productId'
  Lude.Text ->
  ListServiceActionsForProvisioningArtifact
mkListServiceActionsForProvisioningArtifact
  pProvisioningArtifactId_
  pProductId_ =
    ListServiceActionsForProvisioningArtifact'
      { provisioningArtifactId =
          pProvisioningArtifactId_,
        acceptLanguage = Lude.Nothing,
        pageToken = Lude.Nothing,
        pageSize = Lude.Nothing,
        productId = pProductId_
      }

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaProvisioningArtifactId :: Lens.Lens' ListServiceActionsForProvisioningArtifact Lude.Text
lsafpaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ListServiceActionsForProvisioningArtifact -> Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ListServiceActionsForProvisioningArtifact)
{-# DEPRECATED lsafpaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

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
lsafpaAcceptLanguage :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Lude.Maybe Lude.Text)
lsafpaAcceptLanguage = Lens.lens (acceptLanguage :: ListServiceActionsForProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListServiceActionsForProvisioningArtifact)
{-# DEPRECATED lsafpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaPageToken :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Lude.Maybe Lude.Text)
lsafpaPageToken = Lens.lens (pageToken :: ListServiceActionsForProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListServiceActionsForProvisioningArtifact)
{-# DEPRECATED lsafpaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaPageSize :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Lude.Maybe Lude.Natural)
lsafpaPageSize = Lens.lens (pageSize :: ListServiceActionsForProvisioningArtifact -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListServiceActionsForProvisioningArtifact)
{-# DEPRECATED lsafpaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaProductId :: Lens.Lens' ListServiceActionsForProvisioningArtifact Lude.Text
lsafpaProductId = Lens.lens (productId :: ListServiceActionsForProvisioningArtifact -> Lude.Text) (\s a -> s {productId = a} :: ListServiceActionsForProvisioningArtifact)
{-# DEPRECATED lsafpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Page.AWSPager ListServiceActionsForProvisioningArtifact where
  page rq rs
    | Page.stop (rs Lens.^. lsafparsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsafparsServiceActionSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsafpaPageToken Lens..~ rs Lens.^. lsafparsNextPageToken

instance Lude.AWSRequest ListServiceActionsForProvisioningArtifact where
  type
    Rs ListServiceActionsForProvisioningArtifact =
      ListServiceActionsForProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListServiceActionsForProvisioningArtifactResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ServiceActionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListServiceActionsForProvisioningArtifact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListServiceActionsForProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListServiceActionsForProvisioningArtifact where
  toJSON ListServiceActionsForProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ProvisioningArtifactId" Lude..= provisioningArtifactId),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )

instance Lude.ToPath ListServiceActionsForProvisioningArtifact where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServiceActionsForProvisioningArtifact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListServiceActionsForProvisioningArtifactResponse' smart constructor.
data ListServiceActionsForProvisioningArtifactResponse = ListServiceActionsForProvisioningArtifactResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An object containing information about the self-service actions associated with the provisioning artifact.
    serviceActionSummaries :: Lude.Maybe [ServiceActionSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServiceActionsForProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'serviceActionSummaries' - An object containing information about the self-service actions associated with the provisioning artifact.
-- * 'responseStatus' - The response status code.
mkListServiceActionsForProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServiceActionsForProvisioningArtifactResponse
mkListServiceActionsForProvisioningArtifactResponse
  pResponseStatus_ =
    ListServiceActionsForProvisioningArtifactResponse'
      { nextPageToken =
          Lude.Nothing,
        serviceActionSummaries = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafparsNextPageToken :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse (Lude.Maybe Lude.Text)
lsafparsNextPageToken = Lens.lens (nextPageToken :: ListServiceActionsForProvisioningArtifactResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListServiceActionsForProvisioningArtifactResponse)
{-# DEPRECATED lsafparsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object containing information about the self-service actions associated with the provisioning artifact.
--
-- /Note:/ Consider using 'serviceActionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafparsServiceActionSummaries :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse (Lude.Maybe [ServiceActionSummary])
lsafparsServiceActionSummaries = Lens.lens (serviceActionSummaries :: ListServiceActionsForProvisioningArtifactResponse -> Lude.Maybe [ServiceActionSummary]) (\s a -> s {serviceActionSummaries = a} :: ListServiceActionsForProvisioningArtifactResponse)
{-# DEPRECATED lsafparsServiceActionSummaries "Use generic-lens or generic-optics with 'serviceActionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafparsResponseStatus :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse Lude.Int
lsafparsResponseStatus = Lens.lens (responseStatus :: ListServiceActionsForProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServiceActionsForProvisioningArtifactResponse)
{-# DEPRECATED lsafparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
