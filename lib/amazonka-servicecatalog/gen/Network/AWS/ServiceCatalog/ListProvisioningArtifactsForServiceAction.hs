{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the specified self-service action.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
  ( -- * Creating a request
    ListProvisioningArtifactsForServiceAction (..),
    mkListProvisioningArtifactsForServiceAction,

    -- ** Request lenses
    lpafsaAcceptLanguage,
    lpafsaPageToken,
    lpafsaPageSize,
    lpafsaServiceActionId,

    -- * Destructuring the response
    ListProvisioningArtifactsForServiceActionResponse (..),
    mkListProvisioningArtifactsForServiceActionResponse,

    -- ** Response lenses
    lpafsarsNextPageToken,
    lpafsarsProvisioningArtifactViews,
    lpafsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListProvisioningArtifactsForServiceAction' smart constructor.
data ListProvisioningArtifactsForServiceAction = ListProvisioningArtifactsForServiceAction'
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
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    serviceActionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisioningArtifactsForServiceAction' with the minimum fields required to make a request.
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
-- * 'serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
mkListProvisioningArtifactsForServiceAction ::
  -- | 'serviceActionId'
  Lude.Text ->
  ListProvisioningArtifactsForServiceAction
mkListProvisioningArtifactsForServiceAction pServiceActionId_ =
  ListProvisioningArtifactsForServiceAction'
    { acceptLanguage =
        Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      serviceActionId = pServiceActionId_
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
lpafsaAcceptLanguage :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Lude.Maybe Lude.Text)
lpafsaAcceptLanguage = Lens.lens (acceptLanguage :: ListProvisioningArtifactsForServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListProvisioningArtifactsForServiceAction)
{-# DEPRECATED lpafsaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsaPageToken :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Lude.Maybe Lude.Text)
lpafsaPageToken = Lens.lens (pageToken :: ListProvisioningArtifactsForServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListProvisioningArtifactsForServiceAction)
{-# DEPRECATED lpafsaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsaPageSize :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Lude.Maybe Lude.Natural)
lpafsaPageSize = Lens.lens (pageSize :: ListProvisioningArtifactsForServiceAction -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListProvisioningArtifactsForServiceAction)
{-# DEPRECATED lpafsaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsaServiceActionId :: Lens.Lens' ListProvisioningArtifactsForServiceAction Lude.Text
lpafsaServiceActionId = Lens.lens (serviceActionId :: ListProvisioningArtifactsForServiceAction -> Lude.Text) (\s a -> s {serviceActionId = a} :: ListProvisioningArtifactsForServiceAction)
{-# DEPRECATED lpafsaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

instance Page.AWSPager ListProvisioningArtifactsForServiceAction where
  page rq rs
    | Page.stop (rs Lens.^. lpafsarsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpafsarsProvisioningArtifactViews) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpafsaPageToken Lens..~ rs Lens.^. lpafsarsNextPageToken

instance Lude.AWSRequest ListProvisioningArtifactsForServiceAction where
  type
    Rs ListProvisioningArtifactsForServiceAction =
      ListProvisioningArtifactsForServiceActionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsForServiceActionResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProvisioningArtifactViews" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProvisioningArtifactsForServiceAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListProvisioningArtifactsForServiceAction" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProvisioningArtifactsForServiceAction where
  toJSON ListProvisioningArtifactsForServiceAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("ServiceActionId" Lude..= serviceActionId)
          ]
      )

instance Lude.ToPath ListProvisioningArtifactsForServiceAction where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProvisioningArtifactsForServiceAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProvisioningArtifactsForServiceActionResponse' smart constructor.
data ListProvisioningArtifactsForServiceActionResponse = ListProvisioningArtifactsForServiceActionResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects with information about product views and provisioning artifacts.
    provisioningArtifactViews :: Lude.Maybe [ProvisioningArtifactView],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisioningArtifactsForServiceActionResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'provisioningArtifactViews' - An array of objects with information about product views and provisioning artifacts.
-- * 'responseStatus' - The response status code.
mkListProvisioningArtifactsForServiceActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProvisioningArtifactsForServiceActionResponse
mkListProvisioningArtifactsForServiceActionResponse
  pResponseStatus_ =
    ListProvisioningArtifactsForServiceActionResponse'
      { nextPageToken =
          Lude.Nothing,
        provisioningArtifactViews = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsarsNextPageToken :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Lude.Maybe Lude.Text)
lpafsarsNextPageToken = Lens.lens (nextPageToken :: ListProvisioningArtifactsForServiceActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListProvisioningArtifactsForServiceActionResponse)
{-# DEPRECATED lpafsarsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects with information about product views and provisioning artifacts.
--
-- /Note:/ Consider using 'provisioningArtifactViews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsarsProvisioningArtifactViews :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Lude.Maybe [ProvisioningArtifactView])
lpafsarsProvisioningArtifactViews = Lens.lens (provisioningArtifactViews :: ListProvisioningArtifactsForServiceActionResponse -> Lude.Maybe [ProvisioningArtifactView]) (\s a -> s {provisioningArtifactViews = a} :: ListProvisioningArtifactsForServiceActionResponse)
{-# DEPRECATED lpafsarsProvisioningArtifactViews "Use generic-lens or generic-optics with 'provisioningArtifactViews' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsarsResponseStatus :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse Lude.Int
lpafsarsResponseStatus = Lens.lens (responseStatus :: ListProvisioningArtifactsForServiceActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProvisioningArtifactsForServiceActionResponse)
{-# DEPRECATED lpafsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
