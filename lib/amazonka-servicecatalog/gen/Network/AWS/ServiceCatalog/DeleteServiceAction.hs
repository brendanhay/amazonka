{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a self-service action.
module Network.AWS.ServiceCatalog.DeleteServiceAction
  ( -- * Creating a request
    DeleteServiceAction (..),
    mkDeleteServiceAction,

    -- ** Request lenses
    dsaAcceptLanguage,
    dsaId,

    -- * Destructuring the response
    DeleteServiceActionResponse (..),
    mkDeleteServiceActionResponse,

    -- ** Response lenses
    dsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeleteServiceAction' smart constructor.
data DeleteServiceAction = DeleteServiceAction'
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
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceAction' with the minimum fields required to make a request.
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
-- * 'id' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
mkDeleteServiceAction ::
  -- | 'id'
  Lude.Text ->
  DeleteServiceAction
mkDeleteServiceAction pId_ =
  DeleteServiceAction' {acceptLanguage = Lude.Nothing, id = pId_}

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
dsaAcceptLanguage :: Lens.Lens' DeleteServiceAction (Lude.Maybe Lude.Text)
dsaAcceptLanguage = Lens.lens (acceptLanguage :: DeleteServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DeleteServiceAction)
{-# DEPRECATED dsaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaId :: Lens.Lens' DeleteServiceAction Lude.Text
dsaId = Lens.lens (id :: DeleteServiceAction -> Lude.Text) (\s a -> s {id = a} :: DeleteServiceAction)
{-# DEPRECATED dsaId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteServiceAction where
  type Rs DeleteServiceAction = DeleteServiceActionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteServiceActionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteServiceAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DeleteServiceAction" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteServiceAction where
  toJSON DeleteServiceAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DeleteServiceAction where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteServiceAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteServiceActionResponse' smart constructor.
newtype DeleteServiceActionResponse = DeleteServiceActionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceActionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteServiceActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteServiceActionResponse
mkDeleteServiceActionResponse pResponseStatus_ =
  DeleteServiceActionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsResponseStatus :: Lens.Lens' DeleteServiceActionResponse Lude.Int
dsarsResponseStatus = Lens.lens (responseStatus :: DeleteServiceActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteServiceActionResponse)
{-# DEPRECATED dsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
