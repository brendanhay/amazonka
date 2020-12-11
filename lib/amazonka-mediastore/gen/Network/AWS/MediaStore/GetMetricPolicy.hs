{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metric policy for the specified container.
module Network.AWS.MediaStore.GetMetricPolicy
  ( -- * Creating a request
    GetMetricPolicy (..),
    mkGetMetricPolicy,

    -- ** Request lenses
    gmpContainerName,

    -- * Destructuring the response
    GetMetricPolicyResponse (..),
    mkGetMetricPolicyResponse,

    -- ** Response lenses
    gmprsResponseStatus,
    gmprsMetricPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMetricPolicy' smart constructor.
newtype GetMetricPolicy = GetMetricPolicy'
  { containerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that is associated with the metric policy.
mkGetMetricPolicy ::
  -- | 'containerName'
  Lude.Text ->
  GetMetricPolicy
mkGetMetricPolicy pContainerName_ =
  GetMetricPolicy' {containerName = pContainerName_}

-- | The name of the container that is associated with the metric policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpContainerName :: Lens.Lens' GetMetricPolicy Lude.Text
gmpContainerName = Lens.lens (containerName :: GetMetricPolicy -> Lude.Text) (\s a -> s {containerName = a} :: GetMetricPolicy)
{-# DEPRECATED gmpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest GetMetricPolicy where
  type Rs GetMetricPolicy = GetMetricPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMetricPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "MetricPolicy")
      )

instance Lude.ToHeaders GetMetricPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.GetMetricPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMetricPolicy where
  toJSON GetMetricPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath GetMetricPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMetricPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMetricPolicyResponse' smart constructor.
data GetMetricPolicyResponse = GetMetricPolicyResponse'
  { responseStatus ::
      Lude.Int,
    metricPolicy :: MetricPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricPolicyResponse' with the minimum fields required to make a request.
--
-- * 'metricPolicy' - The metric policy that is associated with the specific container.
-- * 'responseStatus' - The response status code.
mkGetMetricPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'metricPolicy'
  MetricPolicy ->
  GetMetricPolicyResponse
mkGetMetricPolicyResponse pResponseStatus_ pMetricPolicy_ =
  GetMetricPolicyResponse'
    { responseStatus = pResponseStatus_,
      metricPolicy = pMetricPolicy_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmprsResponseStatus :: Lens.Lens' GetMetricPolicyResponse Lude.Int
gmprsResponseStatus = Lens.lens (responseStatus :: GetMetricPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMetricPolicyResponse)
{-# DEPRECATED gmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The metric policy that is associated with the specific container.
--
-- /Note:/ Consider using 'metricPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmprsMetricPolicy :: Lens.Lens' GetMetricPolicyResponse MetricPolicy
gmprsMetricPolicy = Lens.lens (metricPolicy :: GetMetricPolicyResponse -> MetricPolicy) (\s a -> s {metricPolicy = a} :: GetMetricPolicyResponse)
{-# DEPRECATED gmprsMetricPolicy "Use generic-lens or generic-optics with 'metricPolicy' instead." #-}
