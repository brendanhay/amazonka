{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an automatic scaling policy from a specified instance group within an EMR cluster.
module Network.AWS.EMR.RemoveAutoScalingPolicy
  ( -- * Creating a request
    RemoveAutoScalingPolicy (..),
    mkRemoveAutoScalingPolicy,

    -- ** Request lenses
    raspClusterId,
    raspInstanceGroupId,

    -- * Destructuring the response
    RemoveAutoScalingPolicyResponse (..),
    mkRemoveAutoScalingPolicyResponse,

    -- ** Response lenses
    rasprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveAutoScalingPolicy' smart constructor.
data RemoveAutoScalingPolicy = RemoveAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
    clusterId :: Lude.Text,
    -- | Specifies the ID of the instance group to which the scaling policy is applied.
    instanceGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAutoScalingPolicy' with the minimum fields required to make a request.
--
-- * 'clusterId' - Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
-- * 'instanceGroupId' - Specifies the ID of the instance group to which the scaling policy is applied.
mkRemoveAutoScalingPolicy ::
  -- | 'clusterId'
  Lude.Text ->
  -- | 'instanceGroupId'
  Lude.Text ->
  RemoveAutoScalingPolicy
mkRemoveAutoScalingPolicy pClusterId_ pInstanceGroupId_ =
  RemoveAutoScalingPolicy'
    { clusterId = pClusterId_,
      instanceGroupId = pInstanceGroupId_
    }

-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raspClusterId :: Lens.Lens' RemoveAutoScalingPolicy Lude.Text
raspClusterId = Lens.lens (clusterId :: RemoveAutoScalingPolicy -> Lude.Text) (\s a -> s {clusterId = a} :: RemoveAutoScalingPolicy)
{-# DEPRECATED raspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the ID of the instance group to which the scaling policy is applied.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raspInstanceGroupId :: Lens.Lens' RemoveAutoScalingPolicy Lude.Text
raspInstanceGroupId = Lens.lens (instanceGroupId :: RemoveAutoScalingPolicy -> Lude.Text) (\s a -> s {instanceGroupId = a} :: RemoveAutoScalingPolicy)
{-# DEPRECATED raspInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

instance Lude.AWSRequest RemoveAutoScalingPolicy where
  type Rs RemoveAutoScalingPolicy = RemoveAutoScalingPolicyResponse
  request = Req.postJSON emrService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveAutoScalingPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveAutoScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.RemoveAutoScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveAutoScalingPolicy where
  toJSON RemoveAutoScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterId" Lude..= clusterId),
            Lude.Just ("InstanceGroupId" Lude..= instanceGroupId)
          ]
      )

instance Lude.ToPath RemoveAutoScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveAutoScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveAutoScalingPolicyResponse' smart constructor.
newtype RemoveAutoScalingPolicyResponse = RemoveAutoScalingPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAutoScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveAutoScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveAutoScalingPolicyResponse
mkRemoveAutoScalingPolicyResponse pResponseStatus_ =
  RemoveAutoScalingPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasprsResponseStatus :: Lens.Lens' RemoveAutoScalingPolicyResponse Lude.Int
rasprsResponseStatus = Lens.lens (responseStatus :: RemoveAutoScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveAutoScalingPolicyResponse)
{-# DEPRECATED rasprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
