{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a managed scaling policy from a specified EMR cluster.
module Network.AWS.EMR.RemoveManagedScalingPolicy
  ( -- * Creating a request
    RemoveManagedScalingPolicy (..),
    mkRemoveManagedScalingPolicy,

    -- ** Request lenses
    rmspClusterId,

    -- * Destructuring the response
    RemoveManagedScalingPolicyResponse (..),
    mkRemoveManagedScalingPolicyResponse,

    -- ** Response lenses
    rmsprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveManagedScalingPolicy' smart constructor.
newtype RemoveManagedScalingPolicy = RemoveManagedScalingPolicy'
  { -- | Specifies the ID of the cluster from which the managed scaling policy will be removed.
    clusterId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveManagedScalingPolicy' with the minimum fields required to make a request.
--
-- * 'clusterId' - Specifies the ID of the cluster from which the managed scaling policy will be removed.
mkRemoveManagedScalingPolicy ::
  -- | 'clusterId'
  Lude.Text ->
  RemoveManagedScalingPolicy
mkRemoveManagedScalingPolicy pClusterId_ =
  RemoveManagedScalingPolicy' {clusterId = pClusterId_}

-- | Specifies the ID of the cluster from which the managed scaling policy will be removed.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmspClusterId :: Lens.Lens' RemoveManagedScalingPolicy Lude.Text
rmspClusterId = Lens.lens (clusterId :: RemoveManagedScalingPolicy -> Lude.Text) (\s a -> s {clusterId = a} :: RemoveManagedScalingPolicy)
{-# DEPRECATED rmspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest RemoveManagedScalingPolicy where
  type
    Rs RemoveManagedScalingPolicy =
      RemoveManagedScalingPolicyResponse
  request = Req.postJSON emrService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveManagedScalingPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveManagedScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.RemoveManagedScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveManagedScalingPolicy where
  toJSON RemoveManagedScalingPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClusterId" Lude..= clusterId)])

instance Lude.ToPath RemoveManagedScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveManagedScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveManagedScalingPolicyResponse' smart constructor.
newtype RemoveManagedScalingPolicyResponse = RemoveManagedScalingPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveManagedScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveManagedScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveManagedScalingPolicyResponse
mkRemoveManagedScalingPolicyResponse pResponseStatus_ =
  RemoveManagedScalingPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsprsResponseStatus :: Lens.Lens' RemoveManagedScalingPolicyResponse Lude.Int
rmsprsResponseStatus = Lens.lens (responseStatus :: RemoveManagedScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveManagedScalingPolicyResponse)
{-# DEPRECATED rmsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
