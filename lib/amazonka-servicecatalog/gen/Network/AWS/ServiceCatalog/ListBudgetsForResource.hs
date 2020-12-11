{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListBudgetsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the budgets associated to the specified resource.
module Network.AWS.ServiceCatalog.ListBudgetsForResource
  ( -- * Creating a request
    ListBudgetsForResource (..),
    mkListBudgetsForResource,

    -- ** Request lenses
    lbfrAcceptLanguage,
    lbfrPageToken,
    lbfrPageSize,
    lbfrResourceId,

    -- * Destructuring the response
    ListBudgetsForResourceResponse (..),
    mkListBudgetsForResourceResponse,

    -- ** Response lenses
    lbfrrsNextPageToken,
    lbfrrsBudgets,
    lbfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListBudgetsForResource' smart constructor.
data ListBudgetsForResource = ListBudgetsForResource'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBudgetsForResource' with the minimum fields required to make a request.
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
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'resourceId' - The resource identifier.
mkListBudgetsForResource ::
  -- | 'resourceId'
  Lude.Text ->
  ListBudgetsForResource
mkListBudgetsForResource pResourceId_ =
  ListBudgetsForResource'
    { acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      resourceId = pResourceId_
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
lbfrAcceptLanguage :: Lens.Lens' ListBudgetsForResource (Lude.Maybe Lude.Text)
lbfrAcceptLanguage = Lens.lens (acceptLanguage :: ListBudgetsForResource -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListBudgetsForResource)
{-# DEPRECATED lbfrAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrPageToken :: Lens.Lens' ListBudgetsForResource (Lude.Maybe Lude.Text)
lbfrPageToken = Lens.lens (pageToken :: ListBudgetsForResource -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListBudgetsForResource)
{-# DEPRECATED lbfrPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrPageSize :: Lens.Lens' ListBudgetsForResource (Lude.Maybe Lude.Natural)
lbfrPageSize = Lens.lens (pageSize :: ListBudgetsForResource -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListBudgetsForResource)
{-# DEPRECATED lbfrPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrResourceId :: Lens.Lens' ListBudgetsForResource Lude.Text
lbfrResourceId = Lens.lens (resourceId :: ListBudgetsForResource -> Lude.Text) (\s a -> s {resourceId = a} :: ListBudgetsForResource)
{-# DEPRECATED lbfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest ListBudgetsForResource where
  type Rs ListBudgetsForResource = ListBudgetsForResourceResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBudgetsForResourceResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Budgets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBudgetsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListBudgetsForResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBudgetsForResource where
  toJSON ListBudgetsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath ListBudgetsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBudgetsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBudgetsForResourceResponse' smart constructor.
data ListBudgetsForResourceResponse = ListBudgetsForResourceResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    budgets ::
      Lude.Maybe [BudgetDetail],
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

-- | Creates a value of 'ListBudgetsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'budgets' - Information about the associated budgets.
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'responseStatus' - The response status code.
mkListBudgetsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBudgetsForResourceResponse
mkListBudgetsForResourceResponse pResponseStatus_ =
  ListBudgetsForResourceResponse'
    { nextPageToken = Lude.Nothing,
      budgets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrrsNextPageToken :: Lens.Lens' ListBudgetsForResourceResponse (Lude.Maybe Lude.Text)
lbfrrsNextPageToken = Lens.lens (nextPageToken :: ListBudgetsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListBudgetsForResourceResponse)
{-# DEPRECATED lbfrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrrsBudgets :: Lens.Lens' ListBudgetsForResourceResponse (Lude.Maybe [BudgetDetail])
lbfrrsBudgets = Lens.lens (budgets :: ListBudgetsForResourceResponse -> Lude.Maybe [BudgetDetail]) (\s a -> s {budgets = a} :: ListBudgetsForResourceResponse)
{-# DEPRECATED lbfrrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrrsResponseStatus :: Lens.Lens' ListBudgetsForResourceResponse Lude.Int
lbfrrsResponseStatus = Lens.lens (responseStatus :: ListBudgetsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBudgetsForResourceResponse)
{-# DEPRECATED lbfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
