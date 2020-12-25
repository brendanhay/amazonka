{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subnet. You must terminate all running instances in the subnet before you can delete the subnet.
module Network.AWS.EC2.DeleteSubnet
  ( -- * Creating a request
    DeleteSubnet (..),
    mkDeleteSubnet,

    -- ** Request lenses
    dsfSubnetId,
    dsfDryRun,

    -- * Destructuring the response
    DeleteSubnetResponse (..),
    mkDeleteSubnetResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSubnet' smart constructor.
data DeleteSubnet = DeleteSubnet'
  { -- | The ID of the subnet.
    subnetId :: Types.SubnetId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubnet' value with any optional fields omitted.
mkDeleteSubnet ::
  -- | 'subnetId'
  Types.SubnetId ->
  DeleteSubnet
mkDeleteSubnet subnetId =
  DeleteSubnet' {subnetId, dryRun = Core.Nothing}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfSubnetId :: Lens.Lens' DeleteSubnet Types.SubnetId
dsfSubnetId = Lens.field @"subnetId"
{-# DEPRECATED dsfSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfDryRun :: Lens.Lens' DeleteSubnet (Core.Maybe Core.Bool)
dsfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteSubnet where
  type Rs DeleteSubnet = DeleteSubnetResponse
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
            ( Core.pure ("Action", "DeleteSubnet")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SubnetId" subnetId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DeleteSubnetResponse'

-- | /See:/ 'mkDeleteSubnetResponse' smart constructor.
data DeleteSubnetResponse = DeleteSubnetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubnetResponse' value with any optional fields omitted.
mkDeleteSubnetResponse ::
  DeleteSubnetResponse
mkDeleteSubnetResponse = DeleteSubnetResponse'
