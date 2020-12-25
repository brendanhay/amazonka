{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeElasticLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a stack's Elastic Load Balancing instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
  ( -- * Creating a request
    DescribeElasticLoadBalancers (..),
    mkDescribeElasticLoadBalancers,

    -- ** Request lenses
    delbLayerIds,
    delbStackId,

    -- * Destructuring the response
    DescribeElasticLoadBalancersResponse (..),
    mkDescribeElasticLoadBalancersResponse,

    -- ** Response lenses
    delbrrsElasticLoadBalancers,
    delbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeElasticLoadBalancers' smart constructor.
data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers'
  { -- | A list of layer IDs. The action describes the Elastic Load Balancing instances for the specified layers.
    layerIds :: Core.Maybe [Types.String],
    -- | A stack ID. The action describes the stack's Elastic Load Balancing instances.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticLoadBalancers' value with any optional fields omitted.
mkDescribeElasticLoadBalancers ::
  DescribeElasticLoadBalancers
mkDescribeElasticLoadBalancers =
  DescribeElasticLoadBalancers'
    { layerIds = Core.Nothing,
      stackId = Core.Nothing
    }

-- | A list of layer IDs. The action describes the Elastic Load Balancing instances for the specified layers.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbLayerIds :: Lens.Lens' DescribeElasticLoadBalancers (Core.Maybe [Types.String])
delbLayerIds = Lens.field @"layerIds"
{-# DEPRECATED delbLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | A stack ID. The action describes the stack's Elastic Load Balancing instances.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbStackId :: Lens.Lens' DescribeElasticLoadBalancers (Core.Maybe Types.String)
delbStackId = Lens.field @"stackId"
{-# DEPRECATED delbStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeElasticLoadBalancers where
  toJSON DescribeElasticLoadBalancers {..} =
    Core.object
      ( Core.catMaybes
          [ ("LayerIds" Core..=) Core.<$> layerIds,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeElasticLoadBalancers where
  type
    Rs DescribeElasticLoadBalancers =
      DescribeElasticLoadBalancersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeElasticLoadBalancers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticLoadBalancersResponse'
            Core.<$> (x Core..:? "ElasticLoadBalancers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeElasticLoadBalancers@ request.
--
-- /See:/ 'mkDescribeElasticLoadBalancersResponse' smart constructor.
data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse'
  { -- | A list of @ElasticLoadBalancer@ objects that describe the specified Elastic Load Balancing instances.
    elasticLoadBalancers :: Core.Maybe [Types.ElasticLoadBalancer],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticLoadBalancersResponse' value with any optional fields omitted.
mkDescribeElasticLoadBalancersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeElasticLoadBalancersResponse
mkDescribeElasticLoadBalancersResponse responseStatus =
  DescribeElasticLoadBalancersResponse'
    { elasticLoadBalancers =
        Core.Nothing,
      responseStatus
    }

-- | A list of @ElasticLoadBalancer@ objects that describe the specified Elastic Load Balancing instances.
--
-- /Note:/ Consider using 'elasticLoadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbrrsElasticLoadBalancers :: Lens.Lens' DescribeElasticLoadBalancersResponse (Core.Maybe [Types.ElasticLoadBalancer])
delbrrsElasticLoadBalancers = Lens.field @"elasticLoadBalancers"
{-# DEPRECATED delbrrsElasticLoadBalancers "Use generic-lens or generic-optics with 'elasticLoadBalancers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbrrsResponseStatus :: Lens.Lens' DescribeElasticLoadBalancersResponse Core.Int
delbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED delbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
