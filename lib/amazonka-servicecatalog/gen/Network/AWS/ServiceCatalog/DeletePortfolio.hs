{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified portfolio.
--
-- You cannot delete a portfolio if it was shared with you or if it has associated products, users, constraints, or shared accounts.
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeletePortfolio
  ( -- * Creating a request
    DeletePortfolio (..),
    mkDeletePortfolio,

    -- ** Request lenses
    dpfAcceptLanguage,
    dpfId,

    -- * Destructuring the response
    DeletePortfolioResponse (..),
    mkDeletePortfolioResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeletePortfolio' smart constructor.
data DeletePortfolio = DeletePortfolio'
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

-- | Creates a value of 'DeletePortfolio' with the minimum fields required to make a request.
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
mkDeletePortfolio ::
  -- | 'id'
  Lude.Text ->
  DeletePortfolio
mkDeletePortfolio pId_ =
  DeletePortfolio' {acceptLanguage = Lude.Nothing, id = pId_}

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
dpfAcceptLanguage :: Lens.Lens' DeletePortfolio (Lude.Maybe Lude.Text)
dpfAcceptLanguage = Lens.lens (acceptLanguage :: DeletePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DeletePortfolio)
{-# DEPRECATED dpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfId :: Lens.Lens' DeletePortfolio Lude.Text
dpfId = Lens.lens (id :: DeletePortfolio -> Lude.Text) (\s a -> s {id = a} :: DeletePortfolio)
{-# DEPRECATED dpfId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeletePortfolio where
  type Rs DeletePortfolio = DeletePortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePortfolioResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.DeletePortfolio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePortfolio where
  toJSON DeletePortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DeletePortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePortfolioResponse' smart constructor.
newtype DeletePortfolioResponse = DeletePortfolioResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePortfolioResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeletePortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePortfolioResponse
mkDeletePortfolioResponse pResponseStatus_ =
  DeletePortfolioResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeletePortfolioResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeletePortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePortfolioResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
