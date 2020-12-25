{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a specified Elastic Load Balancing instance from its layer.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DetachElasticLoadBalancer
  ( -- * Creating a request
    DetachElasticLoadBalancer (..),
    mkDetachElasticLoadBalancer,

    -- ** Request lenses
    delbElasticLoadBalancerName,
    delbLayerId,

    -- * Destructuring the response
    DetachElasticLoadBalancerResponse (..),
    mkDetachElasticLoadBalancerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachElasticLoadBalancer' smart constructor.
data DetachElasticLoadBalancer = DetachElasticLoadBalancer'
  { -- | The Elastic Load Balancing instance's name.
    elasticLoadBalancerName :: Types.String,
    -- | The ID of the layer that the Elastic Load Balancing instance is attached to.
    layerId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachElasticLoadBalancer' value with any optional fields omitted.
mkDetachElasticLoadBalancer ::
  -- | 'elasticLoadBalancerName'
  Types.String ->
  -- | 'layerId'
  Types.String ->
  DetachElasticLoadBalancer
mkDetachElasticLoadBalancer elasticLoadBalancerName layerId =
  DetachElasticLoadBalancer' {elasticLoadBalancerName, layerId}

-- | The Elastic Load Balancing instance's name.
--
-- /Note:/ Consider using 'elasticLoadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbElasticLoadBalancerName :: Lens.Lens' DetachElasticLoadBalancer Types.String
delbElasticLoadBalancerName = Lens.field @"elasticLoadBalancerName"
{-# DEPRECATED delbElasticLoadBalancerName "Use generic-lens or generic-optics with 'elasticLoadBalancerName' instead." #-}

-- | The ID of the layer that the Elastic Load Balancing instance is attached to.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbLayerId :: Lens.Lens' DetachElasticLoadBalancer Types.String
delbLayerId = Lens.field @"layerId"
{-# DEPRECATED delbLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Core.FromJSON DetachElasticLoadBalancer where
  toJSON DetachElasticLoadBalancer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ElasticLoadBalancerName" Core..= elasticLoadBalancerName),
            Core.Just ("LayerId" Core..= layerId)
          ]
      )

instance Core.AWSRequest DetachElasticLoadBalancer where
  type
    Rs DetachElasticLoadBalancer =
      DetachElasticLoadBalancerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DetachElasticLoadBalancer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DetachElasticLoadBalancerResponse'

-- | /See:/ 'mkDetachElasticLoadBalancerResponse' smart constructor.
data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachElasticLoadBalancerResponse' value with any optional fields omitted.
mkDetachElasticLoadBalancerResponse ::
  DetachElasticLoadBalancerResponse
mkDetachElasticLoadBalancerResponse =
  DetachElasticLoadBalancerResponse'
