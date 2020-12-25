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
    rmsprrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveManagedScalingPolicy' smart constructor.
newtype RemoveManagedScalingPolicy = RemoveManagedScalingPolicy'
  { -- | Specifies the ID of the cluster from which the managed scaling policy will be removed.
    clusterId :: Types.ClusterId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveManagedScalingPolicy' value with any optional fields omitted.
mkRemoveManagedScalingPolicy ::
  -- | 'clusterId'
  Types.ClusterId ->
  RemoveManagedScalingPolicy
mkRemoveManagedScalingPolicy clusterId =
  RemoveManagedScalingPolicy' {clusterId}

-- | Specifies the ID of the cluster from which the managed scaling policy will be removed.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmspClusterId :: Lens.Lens' RemoveManagedScalingPolicy Types.ClusterId
rmspClusterId = Lens.field @"clusterId"
{-# DEPRECATED rmspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Core.FromJSON RemoveManagedScalingPolicy where
  toJSON RemoveManagedScalingPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest RemoveManagedScalingPolicy where
  type
    Rs RemoveManagedScalingPolicy =
      RemoveManagedScalingPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.RemoveManagedScalingPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveManagedScalingPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveManagedScalingPolicyResponse' smart constructor.
newtype RemoveManagedScalingPolicyResponse = RemoveManagedScalingPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveManagedScalingPolicyResponse' value with any optional fields omitted.
mkRemoveManagedScalingPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveManagedScalingPolicyResponse
mkRemoveManagedScalingPolicyResponse responseStatus =
  RemoveManagedScalingPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsprrsResponseStatus :: Lens.Lens' RemoveManagedScalingPolicyResponse Core.Int
rmsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rmsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
