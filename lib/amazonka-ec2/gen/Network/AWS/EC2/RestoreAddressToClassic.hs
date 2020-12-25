{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC platform back to the EC2-Classic platform. You cannot move an Elastic IP address that was originally allocated for use in EC2-VPC. The Elastic IP address must not be associated with an instance or network interface.
module Network.AWS.EC2.RestoreAddressToClassic
  ( -- * Creating a request
    RestoreAddressToClassic (..),
    mkRestoreAddressToClassic,

    -- ** Request lenses
    ratcPublicIp,
    ratcDryRun,

    -- * Destructuring the response
    RestoreAddressToClassicResponse (..),
    mkRestoreAddressToClassicResponse,

    -- ** Response lenses
    ratcrrsPublicIp,
    ratcrrsStatus,
    ratcrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreAddressToClassic' smart constructor.
data RestoreAddressToClassic = RestoreAddressToClassic'
  { -- | The Elastic IP address.
    publicIp :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreAddressToClassic' value with any optional fields omitted.
mkRestoreAddressToClassic ::
  -- | 'publicIp'
  Types.String ->
  RestoreAddressToClassic
mkRestoreAddressToClassic publicIp =
  RestoreAddressToClassic' {publicIp, dryRun = Core.Nothing}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcPublicIp :: Lens.Lens' RestoreAddressToClassic Types.String
ratcPublicIp = Lens.field @"publicIp"
{-# DEPRECATED ratcPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcDryRun :: Lens.Lens' RestoreAddressToClassic (Core.Maybe Core.Bool)
ratcDryRun = Lens.field @"dryRun"
{-# DEPRECATED ratcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest RestoreAddressToClassic where
  type Rs RestoreAddressToClassic = RestoreAddressToClassicResponse
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
            ( Core.pure ("Action", "RestoreAddressToClassic")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PublicIp" publicIp)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreAddressToClassicResponse'
            Core.<$> (x Core..@? "publicIp")
            Core.<*> (x Core..@? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreAddressToClassicResponse' smart constructor.
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
  { -- | The Elastic IP address.
    publicIp :: Core.Maybe Types.String,
    -- | The move status for the IP address.
    status :: Core.Maybe Types.AddressStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreAddressToClassicResponse' value with any optional fields omitted.
mkRestoreAddressToClassicResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreAddressToClassicResponse
mkRestoreAddressToClassicResponse responseStatus =
  RestoreAddressToClassicResponse'
    { publicIp = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrrsPublicIp :: Lens.Lens' RestoreAddressToClassicResponse (Core.Maybe Types.String)
ratcrrsPublicIp = Lens.field @"publicIp"
{-# DEPRECATED ratcrrsPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | The move status for the IP address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrrsStatus :: Lens.Lens' RestoreAddressToClassicResponse (Core.Maybe Types.AddressStatus)
ratcrrsStatus = Lens.field @"status"
{-# DEPRECATED ratcrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrrsResponseStatus :: Lens.Lens' RestoreAddressToClassicResponse Core.Int
ratcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ratcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
