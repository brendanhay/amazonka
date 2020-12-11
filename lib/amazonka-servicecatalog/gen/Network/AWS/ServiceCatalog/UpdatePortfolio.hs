{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdatePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified portfolio.
--
-- You cannot update a product that was shared with you.
module Network.AWS.ServiceCatalog.UpdatePortfolio
  ( -- * Creating a request
    UpdatePortfolio (..),
    mkUpdatePortfolio,

    -- ** Request lenses
    uRemoveTags,
    uAcceptLanguage,
    uDisplayName,
    uAddTags,
    uDescription,
    uProviderName,
    uId,

    -- * Destructuring the response
    UpdatePortfolioResponse (..),
    mkUpdatePortfolioResponse,

    -- ** Response lenses
    uprsPortfolioDetail,
    uprsTags,
    uprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdatePortfolio' smart constructor.
data UpdatePortfolio = UpdatePortfolio'
  { removeTags ::
      Lude.Maybe [Lude.Text],
    acceptLanguage :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    addTags :: Lude.Maybe [Tag],
    description :: Lude.Maybe Lude.Text,
    providerName :: Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePortfolio' with the minimum fields required to make a request.
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
-- * 'addTags' - The tags to add.
-- * 'description' - The updated description of the portfolio.
-- * 'displayName' - The name to use for display purposes.
-- * 'id' - The portfolio identifier.
-- * 'providerName' - The updated name of the portfolio provider.
-- * 'removeTags' - The tags to remove.
mkUpdatePortfolio ::
  -- | 'id'
  Lude.Text ->
  UpdatePortfolio
mkUpdatePortfolio pId_ =
  UpdatePortfolio'
    { removeTags = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      displayName = Lude.Nothing,
      addTags = Lude.Nothing,
      description = Lude.Nothing,
      providerName = Lude.Nothing,
      id = pId_
    }

-- | The tags to remove.
--
-- /Note:/ Consider using 'removeTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRemoveTags :: Lens.Lens' UpdatePortfolio (Lude.Maybe [Lude.Text])
uRemoveTags = Lens.lens (removeTags :: UpdatePortfolio -> Lude.Maybe [Lude.Text]) (\s a -> s {removeTags = a} :: UpdatePortfolio)
{-# DEPRECATED uRemoveTags "Use generic-lens or generic-optics with 'removeTags' instead." #-}

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
uAcceptLanguage :: Lens.Lens' UpdatePortfolio (Lude.Maybe Lude.Text)
uAcceptLanguage = Lens.lens (acceptLanguage :: UpdatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdatePortfolio)
{-# DEPRECATED uAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisplayName :: Lens.Lens' UpdatePortfolio (Lude.Maybe Lude.Text)
uDisplayName = Lens.lens (displayName :: UpdatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdatePortfolio)
{-# DEPRECATED uDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The tags to add.
--
-- /Note:/ Consider using 'addTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAddTags :: Lens.Lens' UpdatePortfolio (Lude.Maybe [Tag])
uAddTags = Lens.lens (addTags :: UpdatePortfolio -> Lude.Maybe [Tag]) (\s a -> s {addTags = a} :: UpdatePortfolio)
{-# DEPRECATED uAddTags "Use generic-lens or generic-optics with 'addTags' instead." #-}

-- | The updated description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDescription :: Lens.Lens' UpdatePortfolio (Lude.Maybe Lude.Text)
uDescription = Lens.lens (description :: UpdatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdatePortfolio)
{-# DEPRECATED uDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uProviderName :: Lens.Lens' UpdatePortfolio (Lude.Maybe Lude.Text)
uProviderName = Lens.lens (providerName :: UpdatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: UpdatePortfolio)
{-# DEPRECATED uProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' UpdatePortfolio Lude.Text
uId = Lens.lens (id :: UpdatePortfolio -> Lude.Text) (\s a -> s {id = a} :: UpdatePortfolio)
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdatePortfolio where
  type Rs UpdatePortfolio = UpdatePortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePortfolioResponse'
            Lude.<$> (x Lude..?> "PortfolioDetail")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.UpdatePortfolio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePortfolio where
  toJSON UpdatePortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RemoveTags" Lude..=) Lude.<$> removeTags,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("AddTags" Lude..=) Lude.<$> addTags,
            ("Description" Lude..=) Lude.<$> description,
            ("ProviderName" Lude..=) Lude.<$> providerName,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath UpdatePortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePortfolioResponse' smart constructor.
data UpdatePortfolioResponse = UpdatePortfolioResponse'
  { portfolioDetail ::
      Lude.Maybe PortfolioDetail,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'UpdatePortfolioResponse' with the minimum fields required to make a request.
--
-- * 'portfolioDetail' - Information about the portfolio.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Information about the tags associated with the portfolio.
mkUpdatePortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePortfolioResponse
mkUpdatePortfolioResponse pResponseStatus_ =
  UpdatePortfolioResponse'
    { portfolioDetail = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPortfolioDetail :: Lens.Lens' UpdatePortfolioResponse (Lude.Maybe PortfolioDetail)
uprsPortfolioDetail = Lens.lens (portfolioDetail :: UpdatePortfolioResponse -> Lude.Maybe PortfolioDetail) (\s a -> s {portfolioDetail = a} :: UpdatePortfolioResponse)
{-# DEPRECATED uprsPortfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead." #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsTags :: Lens.Lens' UpdatePortfolioResponse (Lude.Maybe [Tag])
uprsTags = Lens.lens (tags :: UpdatePortfolioResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdatePortfolioResponse)
{-# DEPRECATED uprsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePortfolioResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePortfolioResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
