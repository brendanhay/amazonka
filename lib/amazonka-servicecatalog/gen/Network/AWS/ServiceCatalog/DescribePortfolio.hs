{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DescribePortfolio
  ( -- * Creating a request
    DescribePortfolio (..),
    mkDescribePortfolio,

    -- ** Request lenses
    dpgAcceptLanguage,
    dpgId,

    -- * Destructuring the response
    DescribePortfolioResponse (..),
    mkDescribePortfolioResponse,

    -- ** Response lenses
    dpfrsPortfolioDetail,
    dpfrsTagOptions,
    dpfrsBudgets,
    dpfrsTags,
    dpfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribePortfolio' smart constructor.
data DescribePortfolio = DescribePortfolio'
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
    -- | The portfolio identifier.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePortfolio' with the minimum fields required to make a request.
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
-- * 'id' - The portfolio identifier.
mkDescribePortfolio ::
  -- | 'id'
  Lude.Text ->
  DescribePortfolio
mkDescribePortfolio pId_ =
  DescribePortfolio' {acceptLanguage = Lude.Nothing, id = pId_}

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
dpgAcceptLanguage :: Lens.Lens' DescribePortfolio (Lude.Maybe Lude.Text)
dpgAcceptLanguage = Lens.lens (acceptLanguage :: DescribePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribePortfolio)
{-# DEPRECATED dpgAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgId :: Lens.Lens' DescribePortfolio Lude.Text
dpgId = Lens.lens (id :: DescribePortfolio -> Lude.Text) (\s a -> s {id = a} :: DescribePortfolio)
{-# DEPRECATED dpgId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribePortfolio where
  type Rs DescribePortfolio = DescribePortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePortfolioResponse'
            Lude.<$> (x Lude..?> "PortfolioDetail")
            Lude.<*> (x Lude..?> "TagOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Budgets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribePortfolio" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePortfolio where
  toJSON DescribePortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DescribePortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePortfolioResponse' smart constructor.
data DescribePortfolioResponse = DescribePortfolioResponse'
  { -- | Information about the portfolio.
    portfolioDetail :: Lude.Maybe PortfolioDetail,
    -- | Information about the TagOptions associated with the portfolio.
    tagOptions :: Lude.Maybe [TagOptionDetail],
    -- | Information about the associated budgets.
    budgets :: Lude.Maybe [BudgetDetail],
    -- | Information about the tags associated with the portfolio.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePortfolioResponse' with the minimum fields required to make a request.
--
-- * 'portfolioDetail' - Information about the portfolio.
-- * 'tagOptions' - Information about the TagOptions associated with the portfolio.
-- * 'budgets' - Information about the associated budgets.
-- * 'tags' - Information about the tags associated with the portfolio.
-- * 'responseStatus' - The response status code.
mkDescribePortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePortfolioResponse
mkDescribePortfolioResponse pResponseStatus_ =
  DescribePortfolioResponse'
    { portfolioDetail = Lude.Nothing,
      tagOptions = Lude.Nothing,
      budgets = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfrsPortfolioDetail :: Lens.Lens' DescribePortfolioResponse (Lude.Maybe PortfolioDetail)
dpfrsPortfolioDetail = Lens.lens (portfolioDetail :: DescribePortfolioResponse -> Lude.Maybe PortfolioDetail) (\s a -> s {portfolioDetail = a} :: DescribePortfolioResponse)
{-# DEPRECATED dpfrsPortfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead." #-}

-- | Information about the TagOptions associated with the portfolio.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfrsTagOptions :: Lens.Lens' DescribePortfolioResponse (Lude.Maybe [TagOptionDetail])
dpfrsTagOptions = Lens.lens (tagOptions :: DescribePortfolioResponse -> Lude.Maybe [TagOptionDetail]) (\s a -> s {tagOptions = a} :: DescribePortfolioResponse)
{-# DEPRECATED dpfrsTagOptions "Use generic-lens or generic-optics with 'tagOptions' instead." #-}

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfrsBudgets :: Lens.Lens' DescribePortfolioResponse (Lude.Maybe [BudgetDetail])
dpfrsBudgets = Lens.lens (budgets :: DescribePortfolioResponse -> Lude.Maybe [BudgetDetail]) (\s a -> s {budgets = a} :: DescribePortfolioResponse)
{-# DEPRECATED dpfrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfrsTags :: Lens.Lens' DescribePortfolioResponse (Lude.Maybe [Tag])
dpfrsTags = Lens.lens (tags :: DescribePortfolioResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribePortfolioResponse)
{-# DEPRECATED dpfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfrsResponseStatus :: Lens.Lens' DescribePortfolioResponse Lude.Int
dpfrsResponseStatus = Lens.lens (responseStatus :: DescribePortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePortfolioResponse)
{-# DEPRECATED dpfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
