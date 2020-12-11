{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.FlushAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes an @ApiCache@ object.
module Network.AWS.AppSync.FlushAPICache
  ( -- * Creating a request
    FlushAPICache (..),
    mkFlushAPICache,

    -- ** Request lenses
    facApiId,

    -- * Destructuring the response
    FlushAPICacheResponse (..),
    mkFlushAPICacheResponse,

    -- ** Response lenses
    facrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @FlushApiCache@ operation.
--
-- /See:/ 'mkFlushAPICache' smart constructor.
newtype FlushAPICache = FlushAPICache' {apiId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlushAPICache' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
mkFlushAPICache ::
  -- | 'apiId'
  Lude.Text ->
  FlushAPICache
mkFlushAPICache pApiId_ = FlushAPICache' {apiId = pApiId_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facApiId :: Lens.Lens' FlushAPICache Lude.Text
facApiId = Lens.lens (apiId :: FlushAPICache -> Lude.Text) (\s a -> s {apiId = a} :: FlushAPICache)
{-# DEPRECATED facApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest FlushAPICache where
  type Rs FlushAPICache = FlushAPICacheResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          FlushAPICacheResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders FlushAPICache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath FlushAPICache where
  toPath FlushAPICache' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/FlushCache"]

instance Lude.ToQuery FlushAPICache where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @FlushApiCache@ operation.
--
-- /See:/ 'mkFlushAPICacheResponse' smart constructor.
newtype FlushAPICacheResponse = FlushAPICacheResponse'
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

-- | Creates a value of 'FlushAPICacheResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkFlushAPICacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  FlushAPICacheResponse
mkFlushAPICacheResponse pResponseStatus_ =
  FlushAPICacheResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facrsResponseStatus :: Lens.Lens' FlushAPICacheResponse Lude.Int
facrsResponseStatus = Lens.lens (responseStatus :: FlushAPICacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: FlushAPICacheResponse)
{-# DEPRECATED facrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
