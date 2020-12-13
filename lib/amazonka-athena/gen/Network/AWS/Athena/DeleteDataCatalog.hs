{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.DeleteDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data catalog.
module Network.AWS.Athena.DeleteDataCatalog
  ( -- * Creating a request
    DeleteDataCatalog (..),
    mkDeleteDataCatalog,

    -- ** Request lenses
    ddcName,

    -- * Destructuring the response
    DeleteDataCatalogResponse (..),
    mkDeleteDataCatalogResponse,

    -- ** Response lenses
    ddcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDataCatalog' smart constructor.
newtype DeleteDataCatalog = DeleteDataCatalog'
  { -- | The name of the data catalog to delete.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataCatalog' with the minimum fields required to make a request.
--
-- * 'name' - The name of the data catalog to delete.
mkDeleteDataCatalog ::
  -- | 'name'
  Lude.Text ->
  DeleteDataCatalog
mkDeleteDataCatalog pName_ = DeleteDataCatalog' {name = pName_}

-- | The name of the data catalog to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcName :: Lens.Lens' DeleteDataCatalog Lude.Text
ddcName = Lens.lens (name :: DeleteDataCatalog -> Lude.Text) (\s a -> s {name = a} :: DeleteDataCatalog)
{-# DEPRECATED ddcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteDataCatalog where
  type Rs DeleteDataCatalog = DeleteDataCatalogResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDataCatalogResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDataCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.DeleteDataCatalog" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDataCatalog where
  toJSON DeleteDataCatalog' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteDataCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDataCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDataCatalogResponse' smart constructor.
newtype DeleteDataCatalogResponse = DeleteDataCatalogResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataCatalogResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDataCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDataCatalogResponse
mkDeleteDataCatalogResponse pResponseStatus_ =
  DeleteDataCatalogResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsResponseStatus :: Lens.Lens' DeleteDataCatalogResponse Lude.Int
ddcrsResponseStatus = Lens.lens (responseStatus :: DeleteDataCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDataCatalogResponse)
{-# DEPRECATED ddcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
