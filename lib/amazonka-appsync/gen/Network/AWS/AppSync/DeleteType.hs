{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Type@ object.
module Network.AWS.AppSync.DeleteType
  ( -- * Creating a request
    DeleteType (..),
    mkDeleteType,

    -- ** Request lenses
    dtApiId,
    dtTypeName,

    -- * Destructuring the response
    DeleteTypeResponse (..),
    mkDeleteTypeResponse,

    -- ** Response lenses
    dtrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteType' smart constructor.
data DeleteType = DeleteType'
  { apiId :: Lude.Text,
    typeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteType' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'typeName' - The type name.
mkDeleteType ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  DeleteType
mkDeleteType pApiId_ pTypeName_ =
  DeleteType' {apiId = pApiId_, typeName = pTypeName_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtApiId :: Lens.Lens' DeleteType Lude.Text
dtApiId = Lens.lens (apiId :: DeleteType -> Lude.Text) (\s a -> s {apiId = a} :: DeleteType)
{-# DEPRECATED dtApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTypeName :: Lens.Lens' DeleteType Lude.Text
dtTypeName = Lens.lens (typeName :: DeleteType -> Lude.Text) (\s a -> s {typeName = a} :: DeleteType)
{-# DEPRECATED dtTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Lude.AWSRequest DeleteType where
  type Rs DeleteType = DeleteTypeResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTypeResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteType where
  toPath DeleteType' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/types/", Lude.toBS typeName]

instance Lude.ToQuery DeleteType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTypeResponse' smart constructor.
newtype DeleteTypeResponse = DeleteTypeResponse'
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

-- | Creates a value of 'DeleteTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTypeResponse
mkDeleteTypeResponse pResponseStatus_ =
  DeleteTypeResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeleteTypeResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeleteTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTypeResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
