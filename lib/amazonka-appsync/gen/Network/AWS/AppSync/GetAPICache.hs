{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an @ApiCache@ object.
module Network.AWS.AppSync.GetAPICache
  ( -- * Creating a request
    GetAPICache (..),
    mkGetAPICache,

    -- ** Request lenses
    gacApiId,

    -- * Destructuring the response
    GetAPICacheResponse (..),
    mkGetAPICacheResponse,

    -- ** Response lenses
    gacrsApiCache,
    gacrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetApiCache@ operation.
--
-- /See:/ 'mkGetAPICache' smart constructor.
newtype GetAPICache = GetAPICache' {apiId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPICache' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
mkGetAPICache ::
  -- | 'apiId'
  Lude.Text ->
  GetAPICache
mkGetAPICache pApiId_ = GetAPICache' {apiId = pApiId_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacApiId :: Lens.Lens' GetAPICache Lude.Text
gacApiId = Lens.lens (apiId :: GetAPICache -> Lude.Text) (\s a -> s {apiId = a} :: GetAPICache)
{-# DEPRECATED gacApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest GetAPICache where
  type Rs GetAPICache = GetAPICacheResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAPICacheResponse'
            Lude.<$> (x Lude..?> "apiCache") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAPICache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetAPICache where
  toPath GetAPICache' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/ApiCaches"]

instance Lude.ToQuery GetAPICache where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetApiCache@ operation.
--
-- /See:/ 'mkGetAPICacheResponse' smart constructor.
data GetAPICacheResponse = GetAPICacheResponse'
  { apiCache ::
      Lude.Maybe APICache,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPICacheResponse' with the minimum fields required to make a request.
--
-- * 'apiCache' - The @ApiCache@ object.
-- * 'responseStatus' - The response status code.
mkGetAPICacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAPICacheResponse
mkGetAPICacheResponse pResponseStatus_ =
  GetAPICacheResponse'
    { apiCache = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ApiCache@ object.
--
-- /Note:/ Consider using 'apiCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrsApiCache :: Lens.Lens' GetAPICacheResponse (Lude.Maybe APICache)
gacrsApiCache = Lens.lens (apiCache :: GetAPICacheResponse -> Lude.Maybe APICache) (\s a -> s {apiCache = a} :: GetAPICacheResponse)
{-# DEPRECATED gacrsApiCache "Use generic-lens or generic-optics with 'apiCache' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrsResponseStatus :: Lens.Lens' GetAPICacheResponse Lude.Int
gacrsResponseStatus = Lens.lens (responseStatus :: GetAPICacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAPICacheResponse)
{-# DEPRECATED gacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
