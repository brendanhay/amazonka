{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified product.
--
-- You cannot delete a product if it was shared with you or is associated with a portfolio.
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeleteProduct
  ( -- * Creating a request
    DeleteProduct (..),
    mkDeleteProduct,

    -- ** Request lenses
    dpAcceptLanguage,
    dpId,

    -- * Destructuring the response
    DeleteProductResponse (..),
    mkDeleteProductResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeleteProduct' smart constructor.
data DeleteProduct = DeleteProduct'
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
    -- | The product identifier.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProduct' with the minimum fields required to make a request.
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
-- * 'id' - The product identifier.
mkDeleteProduct ::
  -- | 'id'
  Lude.Text ->
  DeleteProduct
mkDeleteProduct pId_ =
  DeleteProduct' {acceptLanguage = Lude.Nothing, id = pId_}

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
dpAcceptLanguage :: Lens.Lens' DeleteProduct (Lude.Maybe Lude.Text)
dpAcceptLanguage = Lens.lens (acceptLanguage :: DeleteProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DeleteProduct)
{-# DEPRECATED dpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpId :: Lens.Lens' DeleteProduct Lude.Text
dpId = Lens.lens (id :: DeleteProduct -> Lude.Text) (\s a -> s {id = a} :: DeleteProduct)
{-# DEPRECATED dpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteProduct where
  type Rs DeleteProduct = DeleteProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProductResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.DeleteProduct" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProduct where
  toJSON DeleteProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DeleteProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProductResponse' smart constructor.
newtype DeleteProductResponse = DeleteProductResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProductResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProductResponse
mkDeleteProductResponse pResponseStatus_ =
  DeleteProductResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteProductResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProductResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
