{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @DataSource@ object.
module Network.AWS.AppSync.DeleteDataSource
  ( -- * Creating a request
    DeleteDataSource (..),
    mkDeleteDataSource,

    -- ** Request lenses
    ddsApiId,
    ddsName,

    -- * Destructuring the response
    DeleteDataSourceResponse (..),
    mkDeleteDataSourceResponse,

    -- ** Response lenses
    ddsrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { apiId :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataSource' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'name' - The name of the data source.
mkDeleteDataSource ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  DeleteDataSource
mkDeleteDataSource pApiId_ pName_ =
  DeleteDataSource' {apiId = pApiId_, name = pName_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsApiId :: Lens.Lens' DeleteDataSource Lude.Text
ddsApiId = Lens.lens (apiId :: DeleteDataSource -> Lude.Text) (\s a -> s {apiId = a} :: DeleteDataSource)
{-# DEPRECATED ddsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsName :: Lens.Lens' DeleteDataSource Lude.Text
ddsName = Lens.lens (name :: DeleteDataSource -> Lude.Text) (\s a -> s {name = a} :: DeleteDataSource)
{-# DEPRECATED ddsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteDataSource where
  type Rs DeleteDataSource = DeleteDataSourceResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDataSourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteDataSource where
  toPath DeleteDataSource' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/datasources/", Lude.toBS name]

instance Lude.ToQuery DeleteDataSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDataSourceResponse' smart constructor.
newtype DeleteDataSourceResponse = DeleteDataSourceResponse'
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

-- | Creates a value of 'DeleteDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDataSourceResponse
mkDeleteDataSourceResponse pResponseStatus_ =
  DeleteDataSourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsResponseStatus :: Lens.Lens' DeleteDataSourceResponse Lude.Int
ddsrsResponseStatus = Lens.lens (responseStatus :: DeleteDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDataSourceResponse)
{-# DEPRECATED ddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
