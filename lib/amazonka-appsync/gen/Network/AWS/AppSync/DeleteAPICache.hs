{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @ApiCache@ object.
module Network.AWS.AppSync.DeleteAPICache
  ( -- * Creating a request
    DeleteAPICache (..),
    mkDeleteAPICache,

    -- ** Request lenses
    dacApiId,

    -- * Destructuring the response
    DeleteAPICacheResponse (..),
    mkDeleteAPICacheResponse,

    -- ** Response lenses
    dacrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteApiCache@ operation.
--
-- /See:/ 'mkDeleteAPICache' smart constructor.
newtype DeleteAPICache = DeleteAPICache'
  { -- | The API ID.
    apiId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPICache' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
mkDeleteAPICache ::
  -- | 'apiId'
  Lude.Text ->
  DeleteAPICache
mkDeleteAPICache pApiId_ = DeleteAPICache' {apiId = pApiId_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacApiId :: Lens.Lens' DeleteAPICache Lude.Text
dacApiId = Lens.lens (apiId :: DeleteAPICache -> Lude.Text) (\s a -> s {apiId = a} :: DeleteAPICache)
{-# DEPRECATED dacApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest DeleteAPICache where
  type Rs DeleteAPICache = DeleteAPICacheResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAPICacheResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAPICache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteAPICache where
  toPath DeleteAPICache' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/ApiCaches"]

instance Lude.ToQuery DeleteAPICache where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteApiCache@ operation.
--
-- /See:/ 'mkDeleteAPICacheResponse' smart constructor.
newtype DeleteAPICacheResponse = DeleteAPICacheResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPICacheResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAPICacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAPICacheResponse
mkDeleteAPICacheResponse pResponseStatus_ =
  DeleteAPICacheResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrsResponseStatus :: Lens.Lens' DeleteAPICacheResponse Lude.Int
dacrsResponseStatus = Lens.lens (responseStatus :: DeleteAPICacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAPICacheResponse)
{-# DEPRECATED dacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
