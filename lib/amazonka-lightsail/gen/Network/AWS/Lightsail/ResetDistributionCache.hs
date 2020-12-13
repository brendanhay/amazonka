{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ResetDistributionCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes currently cached content from your Amazon Lightsail content delivery network (CDN) distribution.
--
-- After resetting the cache, the next time a content request is made, your distribution pulls, serves, and caches it from the origin.
module Network.AWS.Lightsail.ResetDistributionCache
  ( -- * Creating a request
    ResetDistributionCache (..),
    mkResetDistributionCache,

    -- ** Request lenses
    rdcDistributionName,

    -- * Destructuring the response
    ResetDistributionCacheResponse (..),
    mkResetDistributionCacheResponse,

    -- ** Response lenses
    rdcrsStatus,
    rdcrsOperation,
    rdcrsCreateTime,
    rdcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetDistributionCache' smart constructor.
newtype ResetDistributionCache = ResetDistributionCache'
  { -- | The name of the distribution for which to reset cache.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    distributionName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetDistributionCache' with the minimum fields required to make a request.
--
-- * 'distributionName' - The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
mkResetDistributionCache ::
  ResetDistributionCache
mkResetDistributionCache =
  ResetDistributionCache' {distributionName = Lude.Nothing}

-- | The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcDistributionName :: Lens.Lens' ResetDistributionCache (Lude.Maybe Lude.Text)
rdcDistributionName = Lens.lens (distributionName :: ResetDistributionCache -> Lude.Maybe Lude.Text) (\s a -> s {distributionName = a} :: ResetDistributionCache)
{-# DEPRECATED rdcDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Lude.AWSRequest ResetDistributionCache where
  type Rs ResetDistributionCache = ResetDistributionCacheResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResetDistributionCacheResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "operation")
            Lude.<*> (x Lude..?> "createTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetDistributionCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.ResetDistributionCache" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResetDistributionCache where
  toJSON ResetDistributionCache' {..} =
    Lude.object
      ( Lude.catMaybes
          [("distributionName" Lude..=) Lude.<$> distributionName]
      )

instance Lude.ToPath ResetDistributionCache where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetDistributionCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResetDistributionCacheResponse' smart constructor.
data ResetDistributionCacheResponse = ResetDistributionCacheResponse'
  { -- | The status of the reset cache request.
    status :: Lude.Maybe Lude.Text,
    -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
    createTime :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetDistributionCacheResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the reset cache request.
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'createTime' - The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
-- * 'responseStatus' - The response status code.
mkResetDistributionCacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetDistributionCacheResponse
mkResetDistributionCacheResponse pResponseStatus_ =
  ResetDistributionCacheResponse'
    { status = Lude.Nothing,
      operation = Lude.Nothing,
      createTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the reset cache request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrsStatus :: Lens.Lens' ResetDistributionCacheResponse (Lude.Maybe Lude.Text)
rdcrsStatus = Lens.lens (status :: ResetDistributionCacheResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ResetDistributionCacheResponse)
{-# DEPRECATED rdcrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrsOperation :: Lens.Lens' ResetDistributionCacheResponse (Lude.Maybe Operation)
rdcrsOperation = Lens.lens (operation :: ResetDistributionCacheResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: ResetDistributionCacheResponse)
{-# DEPRECATED rdcrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrsCreateTime :: Lens.Lens' ResetDistributionCacheResponse (Lude.Maybe Lude.Timestamp)
rdcrsCreateTime = Lens.lens (createTime :: ResetDistributionCacheResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: ResetDistributionCacheResponse)
{-# DEPRECATED rdcrsCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrsResponseStatus :: Lens.Lens' ResetDistributionCacheResponse Lude.Int
rdcrsResponseStatus = Lens.lens (responseStatus :: ResetDistributionCacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetDistributionCacheResponse)
{-# DEPRECATED rdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
