{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.PutManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a managed scaling policy for an Amazon EMR cluster. The managed scaling policy defines the limits for resources, such as EC2 instances that can be added or terminated from a cluster. The policy only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
module Network.AWS.EMR.PutManagedScalingPolicy
  ( -- * Creating a request
    PutManagedScalingPolicy (..),
    mkPutManagedScalingPolicy,

    -- ** Request lenses
    pmspClusterId,
    pmspManagedScalingPolicy,

    -- * Destructuring the response
    PutManagedScalingPolicyResponse (..),
    mkPutManagedScalingPolicyResponse,

    -- ** Response lenses
    pmsprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutManagedScalingPolicy' smart constructor.
data PutManagedScalingPolicy = PutManagedScalingPolicy'
  { -- | Specifies the ID of an EMR cluster where the managed scaling policy is attached.
    clusterId :: Lude.Text,
    -- | Specifies the constraints for the managed scaling policy.
    managedScalingPolicy :: ManagedScalingPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutManagedScalingPolicy' with the minimum fields required to make a request.
--
-- * 'clusterId' - Specifies the ID of an EMR cluster where the managed scaling policy is attached.
-- * 'managedScalingPolicy' - Specifies the constraints for the managed scaling policy.
mkPutManagedScalingPolicy ::
  -- | 'clusterId'
  Lude.Text ->
  -- | 'managedScalingPolicy'
  ManagedScalingPolicy ->
  PutManagedScalingPolicy
mkPutManagedScalingPolicy pClusterId_ pManagedScalingPolicy_ =
  PutManagedScalingPolicy'
    { clusterId = pClusterId_,
      managedScalingPolicy = pManagedScalingPolicy_
    }

-- | Specifies the ID of an EMR cluster where the managed scaling policy is attached.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmspClusterId :: Lens.Lens' PutManagedScalingPolicy Lude.Text
pmspClusterId = Lens.lens (clusterId :: PutManagedScalingPolicy -> Lude.Text) (\s a -> s {clusterId = a} :: PutManagedScalingPolicy)
{-# DEPRECATED pmspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the constraints for the managed scaling policy.
--
-- /Note:/ Consider using 'managedScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmspManagedScalingPolicy :: Lens.Lens' PutManagedScalingPolicy ManagedScalingPolicy
pmspManagedScalingPolicy = Lens.lens (managedScalingPolicy :: PutManagedScalingPolicy -> ManagedScalingPolicy) (\s a -> s {managedScalingPolicy = a} :: PutManagedScalingPolicy)
{-# DEPRECATED pmspManagedScalingPolicy "Use generic-lens or generic-optics with 'managedScalingPolicy' instead." #-}

instance Lude.AWSRequest PutManagedScalingPolicy where
  type Rs PutManagedScalingPolicy = PutManagedScalingPolicyResponse
  request = Req.postJSON emrService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutManagedScalingPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutManagedScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.PutManagedScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutManagedScalingPolicy where
  toJSON PutManagedScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterId" Lude..= clusterId),
            Lude.Just ("ManagedScalingPolicy" Lude..= managedScalingPolicy)
          ]
      )

instance Lude.ToPath PutManagedScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutManagedScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutManagedScalingPolicyResponse' smart constructor.
newtype PutManagedScalingPolicyResponse = PutManagedScalingPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutManagedScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutManagedScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutManagedScalingPolicyResponse
mkPutManagedScalingPolicyResponse pResponseStatus_ =
  PutManagedScalingPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmsprsResponseStatus :: Lens.Lens' PutManagedScalingPolicyResponse Lude.Int
pmsprsResponseStatus = Lens.lens (responseStatus :: PutManagedScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutManagedScalingPolicyResponse)
{-# DEPRECATED pmsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
