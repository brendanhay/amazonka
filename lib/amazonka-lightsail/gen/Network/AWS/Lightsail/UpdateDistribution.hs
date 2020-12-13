{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Lightsail content delivery network (CDN) distribution.
--
-- Use this action to update the configuration of your existing distribution
module Network.AWS.Lightsail.UpdateDistribution
  ( -- * Creating a request
    UpdateDistribution (..),
    mkUpdateDistribution,

    -- ** Request lenses
    udOrigin,
    udCacheBehaviorSettings,
    udIsEnabled,
    udDistributionName,
    udDefaultCacheBehavior,
    udCacheBehaviors,

    -- * Destructuring the response
    UpdateDistributionResponse (..),
    mkUpdateDistributionResponse,

    -- ** Response lenses
    udrsOperation,
    udrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
  { -- | An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: Lude.Maybe InputOrigin,
    -- | An object that describes the cache behavior settings for the distribution.
    cacheBehaviorSettings :: Lude.Maybe CacheSettings,
    -- | Indicates whether to enable the distribution.
    isEnabled :: Lude.Maybe Lude.Bool,
    -- | The name of the distribution to update.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    distributionName :: Lude.Text,
    -- | An object that describes the default cache behavior for the distribution.
    defaultCacheBehavior :: Lude.Maybe CacheBehavior,
    -- | An array of objects that describe the per-path cache behavior for the distribution.
    cacheBehaviors :: Lude.Maybe [CacheBehaviorPerPath]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDistribution' with the minimum fields required to make a request.
--
-- * 'origin' - An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
-- * 'cacheBehaviorSettings' - An object that describes the cache behavior settings for the distribution.
-- * 'isEnabled' - Indicates whether to enable the distribution.
-- * 'distributionName' - The name of the distribution to update.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- * 'defaultCacheBehavior' - An object that describes the default cache behavior for the distribution.
-- * 'cacheBehaviors' - An array of objects that describe the per-path cache behavior for the distribution.
mkUpdateDistribution ::
  -- | 'distributionName'
  Lude.Text ->
  UpdateDistribution
mkUpdateDistribution pDistributionName_ =
  UpdateDistribution'
    { origin = Lude.Nothing,
      cacheBehaviorSettings = Lude.Nothing,
      isEnabled = Lude.Nothing,
      distributionName = pDistributionName_,
      defaultCacheBehavior = Lude.Nothing,
      cacheBehaviors = Lude.Nothing
    }

-- | An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udOrigin :: Lens.Lens' UpdateDistribution (Lude.Maybe InputOrigin)
udOrigin = Lens.lens (origin :: UpdateDistribution -> Lude.Maybe InputOrigin) (\s a -> s {origin = a} :: UpdateDistribution)
{-# DEPRECATED udOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | An object that describes the cache behavior settings for the distribution.
--
-- /Note:/ Consider using 'cacheBehaviorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCacheBehaviorSettings :: Lens.Lens' UpdateDistribution (Lude.Maybe CacheSettings)
udCacheBehaviorSettings = Lens.lens (cacheBehaviorSettings :: UpdateDistribution -> Lude.Maybe CacheSettings) (\s a -> s {cacheBehaviorSettings = a} :: UpdateDistribution)
{-# DEPRECATED udCacheBehaviorSettings "Use generic-lens or generic-optics with 'cacheBehaviorSettings' instead." #-}

-- | Indicates whether to enable the distribution.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udIsEnabled :: Lens.Lens' UpdateDistribution (Lude.Maybe Lude.Bool)
udIsEnabled = Lens.lens (isEnabled :: UpdateDistribution -> Lude.Maybe Lude.Bool) (\s a -> s {isEnabled = a} :: UpdateDistribution)
{-# DEPRECATED udIsEnabled "Use generic-lens or generic-optics with 'isEnabled' instead." #-}

-- | The name of the distribution to update.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDistributionName :: Lens.Lens' UpdateDistribution Lude.Text
udDistributionName = Lens.lens (distributionName :: UpdateDistribution -> Lude.Text) (\s a -> s {distributionName = a} :: UpdateDistribution)
{-# DEPRECATED udDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

-- | An object that describes the default cache behavior for the distribution.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDefaultCacheBehavior :: Lens.Lens' UpdateDistribution (Lude.Maybe CacheBehavior)
udDefaultCacheBehavior = Lens.lens (defaultCacheBehavior :: UpdateDistribution -> Lude.Maybe CacheBehavior) (\s a -> s {defaultCacheBehavior = a} :: UpdateDistribution)
{-# DEPRECATED udDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | An array of objects that describe the per-path cache behavior for the distribution.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCacheBehaviors :: Lens.Lens' UpdateDistribution (Lude.Maybe [CacheBehaviorPerPath])
udCacheBehaviors = Lens.lens (cacheBehaviors :: UpdateDistribution -> Lude.Maybe [CacheBehaviorPerPath]) (\s a -> s {cacheBehaviors = a} :: UpdateDistribution)
{-# DEPRECATED udCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

instance Lude.AWSRequest UpdateDistribution where
  type Rs UpdateDistribution = UpdateDistributionResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDistributionResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDistribution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.UpdateDistribution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDistribution where
  toJSON UpdateDistribution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("origin" Lude..=) Lude.<$> origin,
            ("cacheBehaviorSettings" Lude..=) Lude.<$> cacheBehaviorSettings,
            ("isEnabled" Lude..=) Lude.<$> isEnabled,
            Lude.Just ("distributionName" Lude..= distributionName),
            ("defaultCacheBehavior" Lude..=) Lude.<$> defaultCacheBehavior,
            ("cacheBehaviors" Lude..=) Lude.<$> cacheBehaviors
          ]
      )

instance Lude.ToPath UpdateDistribution where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDistribution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDistributionResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkUpdateDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDistributionResponse
mkUpdateDistributionResponse pResponseStatus_ =
  UpdateDistributionResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsOperation :: Lens.Lens' UpdateDistributionResponse (Lude.Maybe Operation)
udrsOperation = Lens.lens (operation :: UpdateDistributionResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: UpdateDistributionResponse)
{-# DEPRECATED udrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDistributionResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDistributionResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
