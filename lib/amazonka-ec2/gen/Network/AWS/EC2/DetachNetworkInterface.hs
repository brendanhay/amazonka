{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a network interface from an instance.
module Network.AWS.EC2.DetachNetworkInterface
  ( -- * Creating a request
    DetachNetworkInterface (..),
    mkDetachNetworkInterface,

    -- ** Request lenses
    dniAttachmentId,
    dniDryRun,
    dniForce,

    -- * Destructuring the response
    DetachNetworkInterfaceResponse (..),
    mkDetachNetworkInterfaceResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DetachNetworkInterface.
--
-- /See:/ 'mkDetachNetworkInterface' smart constructor.
data DetachNetworkInterface = DetachNetworkInterface'
  { -- | The ID of the attachment.
    attachmentId :: Types.NetworkInterfaceAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Specifies whether to force a detachment.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachNetworkInterface' value with any optional fields omitted.
mkDetachNetworkInterface ::
  -- | 'attachmentId'
  Types.NetworkInterfaceAttachmentId ->
  DetachNetworkInterface
mkDetachNetworkInterface attachmentId =
  DetachNetworkInterface'
    { attachmentId,
      dryRun = Core.Nothing,
      force = Core.Nothing
    }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniAttachmentId :: Lens.Lens' DetachNetworkInterface Types.NetworkInterfaceAttachmentId
dniAttachmentId = Lens.field @"attachmentId"
{-# DEPRECATED dniAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniDryRun :: Lens.Lens' DetachNetworkInterface (Core.Maybe Core.Bool)
dniDryRun = Lens.field @"dryRun"
{-# DEPRECATED dniDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Specifies whether to force a detachment.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniForce :: Lens.Lens' DetachNetworkInterface (Core.Maybe Core.Bool)
dniForce = Lens.field @"force"
{-# DEPRECATED dniForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.AWSRequest DetachNetworkInterface where
  type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse
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
            ( Core.pure ("Action", "DetachNetworkInterface")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AttachmentId" attachmentId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
            )
      }
  response = Response.receiveNull DetachNetworkInterfaceResponse'

-- | /See:/ 'mkDetachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachNetworkInterfaceResponse' value with any optional fields omitted.
mkDetachNetworkInterfaceResponse ::
  DetachNetworkInterfaceResponse
mkDetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
