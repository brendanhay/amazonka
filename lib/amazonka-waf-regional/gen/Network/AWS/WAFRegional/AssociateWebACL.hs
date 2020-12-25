{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.AssociateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a web ACL with a resource, either an application load balancer or Amazon API Gateway stage.
module Network.AWS.WAFRegional.AssociateWebACL
  ( -- * Creating a request
    AssociateWebACL (..),
    mkAssociateWebACL,

    -- ** Request lenses
    awaclWebACLId,
    awaclResourceArn,

    -- * Destructuring the response
    AssociateWebACLResponse (..),
    mkAssociateWebACLResponse,

    -- ** Response lenses
    awaclrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkAssociateWebACL' smart constructor.
data AssociateWebACL = AssociateWebACL'
  { -- | A unique identifier (ID) for the web ACL.
    webACLId :: Types.ResourceId,
    -- | The ARN (Amazon Resource Name) of the resource to be protected, either an application load balancer or Amazon API Gateway stage.
    --
    -- The ARN should be in one of the following formats:
    --
    --     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
    --
    --
    --     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
    resourceArn :: Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateWebACL' value with any optional fields omitted.
mkAssociateWebACL ::
  -- | 'webACLId'
  Types.ResourceId ->
  -- | 'resourceArn'
  Types.ResourceArn ->
  AssociateWebACL
mkAssociateWebACL webACLId resourceArn =
  AssociateWebACL' {webACLId, resourceArn}

-- | A unique identifier (ID) for the web ACL.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awaclWebACLId :: Lens.Lens' AssociateWebACL Types.ResourceId
awaclWebACLId = Lens.field @"webACLId"
{-# DEPRECATED awaclWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The ARN (Amazon Resource Name) of the resource to be protected, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
--
--
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awaclResourceArn :: Lens.Lens' AssociateWebACL Types.ResourceArn
awaclResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED awaclResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON AssociateWebACL where
  toJSON AssociateWebACL {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WebACLId" Core..= webACLId),
            Core.Just ("ResourceArn" Core..= resourceArn)
          ]
      )

instance Core.AWSRequest AssociateWebACL where
  type Rs AssociateWebACL = AssociateWebACLResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.AssociateWebACL")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateWebACLResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateWebACLResponse' smart constructor.
newtype AssociateWebACLResponse = AssociateWebACLResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateWebACLResponse' value with any optional fields omitted.
mkAssociateWebACLResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateWebACLResponse
mkAssociateWebACLResponse responseStatus =
  AssociateWebACLResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awaclrrsResponseStatus :: Lens.Lens' AssociateWebACLResponse Core.Int
awaclrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED awaclrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
