{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API key.
module Network.AWS.AppSync.DeleteAPIKey
  ( -- * Creating a request
    DeleteAPIKey (..),
    mkDeleteAPIKey,

    -- ** Request lenses
    dakApiId,
    dakId,

    -- * Destructuring the response
    DeleteAPIKeyResponse (..),
    mkDeleteAPIKeyResponse,

    -- ** Response lenses
    dakrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAPIKey' smart constructor.
data DeleteAPIKey = DeleteAPIKey'
  { apiId :: Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPIKey' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'id' - The ID for the API key.
mkDeleteAPIKey ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  DeleteAPIKey
mkDeleteAPIKey pApiId_ pId_ =
  DeleteAPIKey' {apiId = pApiId_, id = pId_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakApiId :: Lens.Lens' DeleteAPIKey Lude.Text
dakApiId = Lens.lens (apiId :: DeleteAPIKey -> Lude.Text) (\s a -> s {apiId = a} :: DeleteAPIKey)
{-# DEPRECATED dakApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The ID for the API key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakId :: Lens.Lens' DeleteAPIKey Lude.Text
dakId = Lens.lens (id :: DeleteAPIKey -> Lude.Text) (\s a -> s {id = a} :: DeleteAPIKey)
{-# DEPRECATED dakId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteAPIKey where
  type Rs DeleteAPIKey = DeleteAPIKeyResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAPIKeyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteAPIKey where
  toPath DeleteAPIKey' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/apikeys/", Lude.toBS id]

instance Lude.ToQuery DeleteAPIKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAPIKeyResponse' smart constructor.
newtype DeleteAPIKeyResponse = DeleteAPIKeyResponse'
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

-- | Creates a value of 'DeleteAPIKeyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAPIKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAPIKeyResponse
mkDeleteAPIKeyResponse pResponseStatus_ =
  DeleteAPIKeyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakrsResponseStatus :: Lens.Lens' DeleteAPIKeyResponse Lude.Int
dakrsResponseStatus = Lens.lens (responseStatus :: DeleteAPIKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAPIKeyResponse)
{-# DEPRECATED dakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
