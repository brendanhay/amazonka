{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an internet gateway for use with a VPC. After creating the internet gateway, you attach it to a VPC using 'AttachInternetGateway' .
--
-- For more information about your VPC and internet gateway, see the <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide> .
module Network.AWS.EC2.CreateInternetGateway
  ( -- * Creating a request
    CreateInternetGateway (..),
    mkCreateInternetGateway,

    -- ** Request lenses
    cigDryRun,
    cigTagSpecifications,

    -- * Destructuring the response
    CreateInternetGatewayResponse (..),
    mkCreateInternetGatewayResponse,

    -- ** Response lenses
    cigrrsInternetGateway,
    cigrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInternetGateway' smart constructor.
data CreateInternetGateway = CreateInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to assign to the internet gateway.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInternetGateway' value with any optional fields omitted.
mkCreateInternetGateway ::
  CreateInternetGateway
mkCreateInternetGateway =
  CreateInternetGateway'
    { dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigDryRun :: Lens.Lens' CreateInternetGateway (Core.Maybe Core.Bool)
cigDryRun = Lens.field @"dryRun"
{-# DEPRECATED cigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to assign to the internet gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigTagSpecifications :: Lens.Lens' CreateInternetGateway (Core.Maybe [Types.TagSpecification])
cigTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cigTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateInternetGateway where
  type Rs CreateInternetGateway = CreateInternetGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateInternetGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInternetGatewayResponse'
            Core.<$> (x Core..@? "internetGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateInternetGatewayResponse' smart constructor.
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
  { -- | Information about the internet gateway.
    internetGateway :: Core.Maybe Types.InternetGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInternetGatewayResponse' value with any optional fields omitted.
mkCreateInternetGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInternetGatewayResponse
mkCreateInternetGatewayResponse responseStatus =
  CreateInternetGatewayResponse'
    { internetGateway = Core.Nothing,
      responseStatus
    }

-- | Information about the internet gateway.
--
-- /Note:/ Consider using 'internetGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrrsInternetGateway :: Lens.Lens' CreateInternetGatewayResponse (Core.Maybe Types.InternetGateway)
cigrrsInternetGateway = Lens.field @"internetGateway"
{-# DEPRECATED cigrrsInternetGateway "Use generic-lens or generic-optics with 'internetGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrrsResponseStatus :: Lens.Lens' CreateInternetGatewayResponse Core.Int
cigrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cigrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
