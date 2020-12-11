{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection from the Data Catalog.
module Network.AWS.Glue.DeleteConnection
  ( -- * Creating a request
    DeleteConnection (..),
    mkDeleteConnection,

    -- ** Request lenses
    dcCatalogId,
    dcConnectionName,

    -- * Destructuring the response
    DeleteConnectionResponse (..),
    mkDeleteConnectionResponse,

    -- ** Response lenses
    dcrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { catalogId ::
      Lude.Maybe Lude.Text,
    connectionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
-- * 'connectionName' - The name of the connection to delete.
mkDeleteConnection ::
  -- | 'connectionName'
  Lude.Text ->
  DeleteConnection
mkDeleteConnection pConnectionName_ =
  DeleteConnection'
    { catalogId = Lude.Nothing,
      connectionName = pConnectionName_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCatalogId :: Lens.Lens' DeleteConnection (Lude.Maybe Lude.Text)
dcCatalogId = Lens.lens (catalogId :: DeleteConnection -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteConnection)
{-# DEPRECATED dcCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the connection to delete.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectionName :: Lens.Lens' DeleteConnection Lude.Text
dcConnectionName = Lens.lens (connectionName :: DeleteConnection -> Lude.Text) (\s a -> s {connectionName = a} :: DeleteConnection)
{-# DEPRECATED dcConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

instance Lude.AWSRequest DeleteConnection where
  type Rs DeleteConnection = DeleteConnectionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteConnectionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("ConnectionName" Lude..= connectionName)
          ]
      )

instance Lude.ToPath DeleteConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConnectionResponse' smart constructor.
newtype DeleteConnectionResponse = DeleteConnectionResponse'
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

-- | Creates a value of 'DeleteConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConnectionResponse
mkDeleteConnectionResponse pResponseStatus_ =
  DeleteConnectionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteConnectionResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConnectionResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
