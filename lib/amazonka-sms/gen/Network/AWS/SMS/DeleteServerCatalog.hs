{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteServerCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all servers from your server catalog.
module Network.AWS.SMS.DeleteServerCatalog
  ( -- * Creating a request
    DeleteServerCatalog (..),
    mkDeleteServerCatalog,

    -- * Destructuring the response
    DeleteServerCatalogResponse (..),
    mkDeleteServerCatalogResponse,

    -- ** Response lenses
    dscrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDeleteServerCatalog' smart constructor.
data DeleteServerCatalog = DeleteServerCatalog'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServerCatalog' with the minimum fields required to make a request.
mkDeleteServerCatalog ::
  DeleteServerCatalog
mkDeleteServerCatalog = DeleteServerCatalog'

instance Lude.AWSRequest DeleteServerCatalog where
  type Rs DeleteServerCatalog = DeleteServerCatalogResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteServerCatalogResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteServerCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteServerCatalog" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteServerCatalog where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DeleteServerCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteServerCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteServerCatalogResponse' smart constructor.
newtype DeleteServerCatalogResponse = DeleteServerCatalogResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServerCatalogResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteServerCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteServerCatalogResponse
mkDeleteServerCatalogResponse pResponseStatus_ =
  DeleteServerCatalogResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DeleteServerCatalogResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DeleteServerCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteServerCatalogResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
