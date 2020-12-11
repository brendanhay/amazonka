{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Function@ .
module Network.AWS.AppSync.DeleteFunction
  ( -- * Creating a request
    DeleteFunction (..),
    mkDeleteFunction,

    -- ** Request lenses
    dfApiId,
    dfFunctionId,

    -- * Destructuring the response
    DeleteFunctionResponse (..),
    mkDeleteFunctionResponse,

    -- ** Response lenses
    dfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { apiId :: Lude.Text,
    functionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunction' with the minimum fields required to make a request.
--
-- * 'apiId' - The GraphQL API ID.
-- * 'functionId' - The @Function@ ID.
mkDeleteFunction ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'functionId'
  Lude.Text ->
  DeleteFunction
mkDeleteFunction pApiId_ pFunctionId_ =
  DeleteFunction' {apiId = pApiId_, functionId = pFunctionId_}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfApiId :: Lens.Lens' DeleteFunction Lude.Text
dfApiId = Lens.lens (apiId :: DeleteFunction -> Lude.Text) (\s a -> s {apiId = a} :: DeleteFunction)
{-# DEPRECATED dfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFunctionId :: Lens.Lens' DeleteFunction Lude.Text
dfFunctionId = Lens.lens (functionId :: DeleteFunction -> Lude.Text) (\s a -> s {functionId = a} :: DeleteFunction)
{-# DEPRECATED dfFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

instance Lude.AWSRequest DeleteFunction where
  type Rs DeleteFunction = DeleteFunctionResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteFunctionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/functions/", Lude.toBS functionId]

instance Lude.ToQuery DeleteFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFunctionResponse' smart constructor.
newtype DeleteFunctionResponse = DeleteFunctionResponse'
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

-- | Creates a value of 'DeleteFunctionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFunctionResponse
mkDeleteFunctionResponse pResponseStatus_ =
  DeleteFunctionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DeleteFunctionResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DeleteFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFunctionResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
