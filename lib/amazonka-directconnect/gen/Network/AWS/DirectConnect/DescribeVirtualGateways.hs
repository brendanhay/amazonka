{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual private gateways owned by the AWS account.
--
-- You can create one or more AWS Direct Connect private virtual interfaces linked to a virtual private gateway.
module Network.AWS.DirectConnect.DescribeVirtualGateways
  ( -- * Creating a request
    DescribeVirtualGateways (..),
    mkDescribeVirtualGateways,

    -- * Destructuring the response
    DescribeVirtualGatewaysResponse (..),
    mkDescribeVirtualGatewaysResponse,

    -- ** Response lenses
    dvgrrsVirtualGateways,
    dvgrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVirtualGateways' smart constructor.
data DescribeVirtualGateways = DescribeVirtualGateways'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVirtualGateways' value with any optional fields omitted.
mkDescribeVirtualGateways ::
  DescribeVirtualGateways
mkDescribeVirtualGateways = DescribeVirtualGateways'

instance Core.FromJSON DescribeVirtualGateways where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeVirtualGateways where
  type Rs DescribeVirtualGateways = DescribeVirtualGatewaysResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.DescribeVirtualGateways")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualGatewaysResponse'
            Core.<$> (x Core..:? "virtualGateways")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeVirtualGatewaysResponse' smart constructor.
data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse'
  { -- | The virtual private gateways.
    virtualGateways :: Core.Maybe [Types.VirtualGateway],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVirtualGatewaysResponse' value with any optional fields omitted.
mkDescribeVirtualGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeVirtualGatewaysResponse
mkDescribeVirtualGatewaysResponse responseStatus =
  DescribeVirtualGatewaysResponse'
    { virtualGateways = Core.Nothing,
      responseStatus
    }

-- | The virtual private gateways.
--
-- /Note:/ Consider using 'virtualGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrrsVirtualGateways :: Lens.Lens' DescribeVirtualGatewaysResponse (Core.Maybe [Types.VirtualGateway])
dvgrrsVirtualGateways = Lens.field @"virtualGateways"
{-# DEPRECATED dvgrrsVirtualGateways "Use generic-lens or generic-optics with 'virtualGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrrsResponseStatus :: Lens.Lens' DescribeVirtualGatewaysResponse Core.Int
dvgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
