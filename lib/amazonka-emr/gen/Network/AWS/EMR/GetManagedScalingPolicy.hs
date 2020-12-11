{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.GetManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the attached managed scaling policy for an Amazon EMR cluster.
module Network.AWS.EMR.GetManagedScalingPolicy
  ( -- * Creating a request
    GetManagedScalingPolicy (..),
    mkGetManagedScalingPolicy,

    -- ** Request lenses
    gmspClusterId,

    -- * Destructuring the response
    GetManagedScalingPolicyResponse (..),
    mkGetManagedScalingPolicyResponse,

    -- ** Response lenses
    gmsprsManagedScalingPolicy,
    gmsprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetManagedScalingPolicy' smart constructor.
newtype GetManagedScalingPolicy = GetManagedScalingPolicy'
  { clusterId ::
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

-- | Creates a value of 'GetManagedScalingPolicy' with the minimum fields required to make a request.
--
-- * 'clusterId' - Specifies the ID of the cluster for which the managed scaling policy will be fetched.
mkGetManagedScalingPolicy ::
  -- | 'clusterId'
  Lude.Text ->
  GetManagedScalingPolicy
mkGetManagedScalingPolicy pClusterId_ =
  GetManagedScalingPolicy' {clusterId = pClusterId_}

-- | Specifies the ID of the cluster for which the managed scaling policy will be fetched.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmspClusterId :: Lens.Lens' GetManagedScalingPolicy Lude.Text
gmspClusterId = Lens.lens (clusterId :: GetManagedScalingPolicy -> Lude.Text) (\s a -> s {clusterId = a} :: GetManagedScalingPolicy)
{-# DEPRECATED gmspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest GetManagedScalingPolicy where
  type Rs GetManagedScalingPolicy = GetManagedScalingPolicyResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetManagedScalingPolicyResponse'
            Lude.<$> (x Lude..?> "ManagedScalingPolicy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetManagedScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.GetManagedScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetManagedScalingPolicy where
  toJSON GetManagedScalingPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClusterId" Lude..= clusterId)])

instance Lude.ToPath GetManagedScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetManagedScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetManagedScalingPolicyResponse' smart constructor.
data GetManagedScalingPolicyResponse = GetManagedScalingPolicyResponse'
  { managedScalingPolicy ::
      Lude.Maybe
        ManagedScalingPolicy,
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

-- | Creates a value of 'GetManagedScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'managedScalingPolicy' - Specifies the managed scaling policy that is attached to an Amazon EMR cluster.
-- * 'responseStatus' - The response status code.
mkGetManagedScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetManagedScalingPolicyResponse
mkGetManagedScalingPolicyResponse pResponseStatus_ =
  GetManagedScalingPolicyResponse'
    { managedScalingPolicy =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the managed scaling policy that is attached to an Amazon EMR cluster.
--
-- /Note:/ Consider using 'managedScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsprsManagedScalingPolicy :: Lens.Lens' GetManagedScalingPolicyResponse (Lude.Maybe ManagedScalingPolicy)
gmsprsManagedScalingPolicy = Lens.lens (managedScalingPolicy :: GetManagedScalingPolicyResponse -> Lude.Maybe ManagedScalingPolicy) (\s a -> s {managedScalingPolicy = a} :: GetManagedScalingPolicyResponse)
{-# DEPRECATED gmsprsManagedScalingPolicy "Use generic-lens or generic-optics with 'managedScalingPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsprsResponseStatus :: Lens.Lens' GetManagedScalingPolicyResponse Lude.Int
gmsprsResponseStatus = Lens.lens (responseStatus :: GetManagedScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetManagedScalingPolicyResponse)
{-# DEPRECATED gmsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
