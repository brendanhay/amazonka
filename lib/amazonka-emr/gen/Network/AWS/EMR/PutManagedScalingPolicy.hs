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
    pmsprrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutManagedScalingPolicy' smart constructor.
data PutManagedScalingPolicy = PutManagedScalingPolicy'
  { -- | Specifies the ID of an EMR cluster where the managed scaling policy is attached.
    clusterId :: Types.ClusterId,
    -- | Specifies the constraints for the managed scaling policy.
    managedScalingPolicy :: Types.ManagedScalingPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutManagedScalingPolicy' value with any optional fields omitted.
mkPutManagedScalingPolicy ::
  -- | 'clusterId'
  Types.ClusterId ->
  -- | 'managedScalingPolicy'
  Types.ManagedScalingPolicy ->
  PutManagedScalingPolicy
mkPutManagedScalingPolicy clusterId managedScalingPolicy =
  PutManagedScalingPolicy' {clusterId, managedScalingPolicy}

-- | Specifies the ID of an EMR cluster where the managed scaling policy is attached.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmspClusterId :: Lens.Lens' PutManagedScalingPolicy Types.ClusterId
pmspClusterId = Lens.field @"clusterId"
{-# DEPRECATED pmspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the constraints for the managed scaling policy.
--
-- /Note:/ Consider using 'managedScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmspManagedScalingPolicy :: Lens.Lens' PutManagedScalingPolicy Types.ManagedScalingPolicy
pmspManagedScalingPolicy = Lens.field @"managedScalingPolicy"
{-# DEPRECATED pmspManagedScalingPolicy "Use generic-lens or generic-optics with 'managedScalingPolicy' instead." #-}

instance Core.FromJSON PutManagedScalingPolicy where
  toJSON PutManagedScalingPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("ManagedScalingPolicy" Core..= managedScalingPolicy)
          ]
      )

instance Core.AWSRequest PutManagedScalingPolicy where
  type Rs PutManagedScalingPolicy = PutManagedScalingPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.PutManagedScalingPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutManagedScalingPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutManagedScalingPolicyResponse' smart constructor.
newtype PutManagedScalingPolicyResponse = PutManagedScalingPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutManagedScalingPolicyResponse' value with any optional fields omitted.
mkPutManagedScalingPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutManagedScalingPolicyResponse
mkPutManagedScalingPolicyResponse responseStatus =
  PutManagedScalingPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmsprrsResponseStatus :: Lens.Lens' PutManagedScalingPolicyResponse Core.Int
pmsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pmsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
