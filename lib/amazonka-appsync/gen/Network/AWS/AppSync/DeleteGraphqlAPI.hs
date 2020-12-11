{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteGraphqlAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @GraphqlApi@ object.
module Network.AWS.AppSync.DeleteGraphqlAPI
  ( -- * Creating a request
    DeleteGraphqlAPI (..),
    mkDeleteGraphqlAPI,

    -- ** Request lenses
    dgaApiId,

    -- * Destructuring the response
    DeleteGraphqlAPIResponse (..),
    mkDeleteGraphqlAPIResponse,

    -- ** Response lenses
    dgarsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGraphqlAPI' smart constructor.
newtype DeleteGraphqlAPI = DeleteGraphqlAPI' {apiId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGraphqlAPI' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
mkDeleteGraphqlAPI ::
  -- | 'apiId'
  Lude.Text ->
  DeleteGraphqlAPI
mkDeleteGraphqlAPI pApiId_ = DeleteGraphqlAPI' {apiId = pApiId_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgaApiId :: Lens.Lens' DeleteGraphqlAPI Lude.Text
dgaApiId = Lens.lens (apiId :: DeleteGraphqlAPI -> Lude.Text) (\s a -> s {apiId = a} :: DeleteGraphqlAPI)
{-# DEPRECATED dgaApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest DeleteGraphqlAPI where
  type Rs DeleteGraphqlAPI = DeleteGraphqlAPIResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteGraphqlAPIResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGraphqlAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteGraphqlAPI where
  toPath DeleteGraphqlAPI' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId]

instance Lude.ToQuery DeleteGraphqlAPI where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGraphqlAPIResponse' smart constructor.
newtype DeleteGraphqlAPIResponse = DeleteGraphqlAPIResponse'
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

-- | Creates a value of 'DeleteGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteGraphqlAPIResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGraphqlAPIResponse
mkDeleteGraphqlAPIResponse pResponseStatus_ =
  DeleteGraphqlAPIResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgarsResponseStatus :: Lens.Lens' DeleteGraphqlAPIResponse Lude.Int
dgarsResponseStatus = Lens.lens (responseStatus :: DeleteGraphqlAPIResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGraphqlAPIResponse)
{-# DEPRECATED dgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
