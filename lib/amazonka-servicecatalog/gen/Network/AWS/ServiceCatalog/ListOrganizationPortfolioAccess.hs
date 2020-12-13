{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organization nodes that have access to the specified portfolio. This API can only be called by the management account in the organization or by a delegated admin.
--
-- If a delegated admin is de-registered, they can no longer perform this operation.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
  ( -- * Creating a request
    ListOrganizationPortfolioAccess (..),
    mkListOrganizationPortfolioAccess,

    -- ** Request lenses
    lopaPortfolioId,
    lopaOrganizationNodeType,
    lopaAcceptLanguage,
    lopaPageToken,
    lopaPageSize,

    -- * Destructuring the response
    ListOrganizationPortfolioAccessResponse (..),
    mkListOrganizationPortfolioAccessResponse,

    -- ** Response lenses
    loparsNextPageToken,
    loparsOrganizationNodes,
    loparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListOrganizationPortfolioAccess' smart constructor.
data ListOrganizationPortfolioAccess = ListOrganizationPortfolioAccess'
  { -- | The portfolio identifier. For example, @port-2abcdext3y5fk@ .
    portfolioId :: Lude.Text,
    -- | The organization node type that will be returned in the output.
    --
    --
    --     * @ORGANIZATION@ - Organization that has access to the portfolio.
    --
    --
    --     * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.
    --
    --
    --     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
    organizationNodeType :: OrganizationNodeType,
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
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizationPortfolioAccess' with the minimum fields required to make a request.
--
-- * 'portfolioId' - The portfolio identifier. For example, @port-2abcdext3y5fk@ .
-- * 'organizationNodeType' - The organization node type that will be returned in the output.
--
--
--     * @ORGANIZATION@ - Organization that has access to the portfolio.
--
--
--     * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.
--
--
--     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
--
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
mkListOrganizationPortfolioAccess ::
  -- | 'portfolioId'
  Lude.Text ->
  -- | 'organizationNodeType'
  OrganizationNodeType ->
  ListOrganizationPortfolioAccess
mkListOrganizationPortfolioAccess
  pPortfolioId_
  pOrganizationNodeType_ =
    ListOrganizationPortfolioAccess'
      { portfolioId = pPortfolioId_,
        organizationNodeType = pOrganizationNodeType_,
        acceptLanguage = Lude.Nothing,
        pageToken = Lude.Nothing,
        pageSize = Lude.Nothing
      }

-- | The portfolio identifier. For example, @port-2abcdext3y5fk@ .
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaPortfolioId :: Lens.Lens' ListOrganizationPortfolioAccess Lude.Text
lopaPortfolioId = Lens.lens (portfolioId :: ListOrganizationPortfolioAccess -> Lude.Text) (\s a -> s {portfolioId = a} :: ListOrganizationPortfolioAccess)
{-# DEPRECATED lopaPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The organization node type that will be returned in the output.
--
--
--     * @ORGANIZATION@ - Organization that has access to the portfolio.
--
--
--     * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.
--
--
--     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
--
--
--
-- /Note:/ Consider using 'organizationNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaOrganizationNodeType :: Lens.Lens' ListOrganizationPortfolioAccess OrganizationNodeType
lopaOrganizationNodeType = Lens.lens (organizationNodeType :: ListOrganizationPortfolioAccess -> OrganizationNodeType) (\s a -> s {organizationNodeType = a} :: ListOrganizationPortfolioAccess)
{-# DEPRECATED lopaOrganizationNodeType "Use generic-lens or generic-optics with 'organizationNodeType' instead." #-}

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
lopaAcceptLanguage :: Lens.Lens' ListOrganizationPortfolioAccess (Lude.Maybe Lude.Text)
lopaAcceptLanguage = Lens.lens (acceptLanguage :: ListOrganizationPortfolioAccess -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListOrganizationPortfolioAccess)
{-# DEPRECATED lopaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaPageToken :: Lens.Lens' ListOrganizationPortfolioAccess (Lude.Maybe Lude.Text)
lopaPageToken = Lens.lens (pageToken :: ListOrganizationPortfolioAccess -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListOrganizationPortfolioAccess)
{-# DEPRECATED lopaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaPageSize :: Lens.Lens' ListOrganizationPortfolioAccess (Lude.Maybe Lude.Natural)
lopaPageSize = Lens.lens (pageSize :: ListOrganizationPortfolioAccess -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListOrganizationPortfolioAccess)
{-# DEPRECATED lopaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListOrganizationPortfolioAccess where
  page rq rs
    | Page.stop (rs Lens.^. loparsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loparsOrganizationNodes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lopaPageToken Lens..~ rs Lens.^. loparsNextPageToken

instance Lude.AWSRequest ListOrganizationPortfolioAccess where
  type
    Rs ListOrganizationPortfolioAccess =
      ListOrganizationPortfolioAccessResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOrganizationPortfolioAccessResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "OrganizationNodes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOrganizationPortfolioAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListOrganizationPortfolioAccess" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOrganizationPortfolioAccess where
  toJSON ListOrganizationPortfolioAccess' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PortfolioId" Lude..= portfolioId),
            Lude.Just ("OrganizationNodeType" Lude..= organizationNodeType),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListOrganizationPortfolioAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOrganizationPortfolioAccess where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListOrganizationPortfolioAccessResponse' smart constructor.
data ListOrganizationPortfolioAccessResponse = ListOrganizationPortfolioAccessResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Displays information about the organization nodes.
    organizationNodes :: Lude.Maybe [OrganizationNode],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizationPortfolioAccessResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'organizationNodes' - Displays information about the organization nodes.
-- * 'responseStatus' - The response status code.
mkListOrganizationPortfolioAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOrganizationPortfolioAccessResponse
mkListOrganizationPortfolioAccessResponse pResponseStatus_ =
  ListOrganizationPortfolioAccessResponse'
    { nextPageToken =
        Lude.Nothing,
      organizationNodes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loparsNextPageToken :: Lens.Lens' ListOrganizationPortfolioAccessResponse (Lude.Maybe Lude.Text)
loparsNextPageToken = Lens.lens (nextPageToken :: ListOrganizationPortfolioAccessResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListOrganizationPortfolioAccessResponse)
{-# DEPRECATED loparsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Displays information about the organization nodes.
--
-- /Note:/ Consider using 'organizationNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loparsOrganizationNodes :: Lens.Lens' ListOrganizationPortfolioAccessResponse (Lude.Maybe [OrganizationNode])
loparsOrganizationNodes = Lens.lens (organizationNodes :: ListOrganizationPortfolioAccessResponse -> Lude.Maybe [OrganizationNode]) (\s a -> s {organizationNodes = a} :: ListOrganizationPortfolioAccessResponse)
{-# DEPRECATED loparsOrganizationNodes "Use generic-lens or generic-optics with 'organizationNodes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loparsResponseStatus :: Lens.Lens' ListOrganizationPortfolioAccessResponse Lude.Int
loparsResponseStatus = Lens.lens (responseStatus :: ListOrganizationPortfolioAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOrganizationPortfolioAccessResponse)
{-# DEPRECATED loparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
