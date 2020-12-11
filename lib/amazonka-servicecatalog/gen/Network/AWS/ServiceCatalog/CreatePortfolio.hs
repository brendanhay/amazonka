{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreatePortfolio
  ( -- * Creating a request
    CreatePortfolio (..),
    mkCreatePortfolio,

    -- ** Request lenses
    creAcceptLanguage,
    creDescription,
    creTags,
    creDisplayName,
    creProviderName,
    creIdempotencyToken,

    -- * Destructuring the response
    CreatePortfolioResponse (..),
    mkCreatePortfolioResponse,

    -- ** Response lenses
    crsPortfolioDetail,
    crsTags,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreatePortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    displayName :: Lude.Text,
    providerName :: Lude.Text,
    idempotencyToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePortfolio' with the minimum fields required to make a request.
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
-- * 'description' - The description of the portfolio.
-- * 'displayName' - The name to use for display purposes.
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'providerName' - The name of the portfolio provider.
-- * 'tags' - One or more tags.
mkCreatePortfolio ::
  -- | 'displayName'
  Lude.Text ->
  -- | 'providerName'
  Lude.Text ->
  -- | 'idempotencyToken'
  Lude.Text ->
  CreatePortfolio
mkCreatePortfolio pDisplayName_ pProviderName_ pIdempotencyToken_ =
  CreatePortfolio'
    { acceptLanguage = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      displayName = pDisplayName_,
      providerName = pProviderName_,
      idempotencyToken = pIdempotencyToken_
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
creAcceptLanguage :: Lens.Lens' CreatePortfolio (Lude.Maybe Lude.Text)
creAcceptLanguage = Lens.lens (acceptLanguage :: CreatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreatePortfolio)
{-# DEPRECATED creAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creDescription :: Lens.Lens' CreatePortfolio (Lude.Maybe Lude.Text)
creDescription = Lens.lens (description :: CreatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePortfolio)
{-# DEPRECATED creDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creTags :: Lens.Lens' CreatePortfolio (Lude.Maybe [Tag])
creTags = Lens.lens (tags :: CreatePortfolio -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePortfolio)
{-# DEPRECATED creTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creDisplayName :: Lens.Lens' CreatePortfolio Lude.Text
creDisplayName = Lens.lens (displayName :: CreatePortfolio -> Lude.Text) (\s a -> s {displayName = a} :: CreatePortfolio)
{-# DEPRECATED creDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creProviderName :: Lens.Lens' CreatePortfolio Lude.Text
creProviderName = Lens.lens (providerName :: CreatePortfolio -> Lude.Text) (\s a -> s {providerName = a} :: CreatePortfolio)
{-# DEPRECATED creProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creIdempotencyToken :: Lens.Lens' CreatePortfolio Lude.Text
creIdempotencyToken = Lens.lens (idempotencyToken :: CreatePortfolio -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CreatePortfolio)
{-# DEPRECATED creIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

instance Lude.AWSRequest CreatePortfolio where
  type Rs CreatePortfolio = CreatePortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePortfolioResponse'
            Lude.<$> (x Lude..?> "PortfolioDetail")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.CreatePortfolio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePortfolio where
  toJSON CreatePortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("DisplayName" Lude..= displayName),
            Lude.Just ("ProviderName" Lude..= providerName),
            Lude.Just ("IdempotencyToken" Lude..= idempotencyToken)
          ]
      )

instance Lude.ToPath CreatePortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
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

-- | Creates a value of 'CreatePortfolioResponse' with the minimum fields required to make a request.
--
-- * 'portfolioDetail' - Information about the portfolio.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Information about the tags associated with the portfolio.
mkCreatePortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePortfolioResponse
mkCreatePortfolioResponse pResponseStatus_ =
  CreatePortfolioResponse'
    { portfolioDetail = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsPortfolioDetail :: Lens.Lens' CreatePortfolioResponse (Lude.Maybe PortfolioDetail)
crsPortfolioDetail = Lens.lens (portfolioDetail :: CreatePortfolioResponse -> Lude.Maybe PortfolioDetail) (\s a -> s {portfolioDetail = a} :: CreatePortfolioResponse)
{-# DEPRECATED crsPortfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead." #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsTags :: Lens.Lens' CreatePortfolioResponse (Lude.Maybe [Tag])
crsTags = Lens.lens (tags :: CreatePortfolioResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePortfolioResponse)
{-# DEPRECATED crsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreatePortfolioResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreatePortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePortfolioResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
