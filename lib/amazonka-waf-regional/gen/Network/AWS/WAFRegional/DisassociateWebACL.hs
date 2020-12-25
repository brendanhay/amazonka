{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DisassociateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a web ACL from the specified resource, either an application load balancer or Amazon API Gateway stage.
module Network.AWS.WAFRegional.DisassociateWebACL
  ( -- * Creating a request
    DisassociateWebACL (..),
    mkDisassociateWebACL,

    -- ** Request lenses
    dwaclResourceArn,

    -- * Destructuring the response
    DisassociateWebACLResponse (..),
    mkDisassociateWebACLResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDisassociateWebACL' smart constructor.
newtype DisassociateWebACL = DisassociateWebACL'
  { -- | The ARN (Amazon Resource Name) of the resource from which the web ACL is being removed, either an application load balancer or Amazon API Gateway stage.
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
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateWebACL' value with any optional fields omitted.
mkDisassociateWebACL ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  DisassociateWebACL
mkDisassociateWebACL resourceArn = DisassociateWebACL' {resourceArn}

-- | The ARN (Amazon Resource Name) of the resource from which the web ACL is being removed, either an application load balancer or Amazon API Gateway stage.
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
dwaclResourceArn :: Lens.Lens' DisassociateWebACL Types.ResourceArn
dwaclResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED dwaclResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON DisassociateWebACL where
  toJSON DisassociateWebACL {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest DisassociateWebACL where
  type Rs DisassociateWebACL = DisassociateWebACLResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.DisassociateWebACL")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWebACLResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateWebACLResponse' smart constructor.
newtype DisassociateWebACLResponse = DisassociateWebACLResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateWebACLResponse' value with any optional fields omitted.
mkDisassociateWebACLResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateWebACLResponse
mkDisassociateWebACLResponse responseStatus =
  DisassociateWebACLResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DisassociateWebACLResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
