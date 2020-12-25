{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a public virtual interface created by another AWS account.
--
-- After the virtual interface owner makes this call, the specified virtual interface is created and made available to handle traffic.
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
  ( -- * Creating a request
    ConfirmPublicVirtualInterface (..),
    mkConfirmPublicVirtualInterface,

    -- ** Request lenses
    cVirtualInterfaceId,

    -- * Destructuring the response
    ConfirmPublicVirtualInterfaceResponse (..),
    mkConfirmPublicVirtualInterfaceResponse,

    -- ** Response lenses
    crsVirtualInterfaceState,
    crsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkConfirmPublicVirtualInterface' smart constructor.
newtype ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Types.VirtualInterfaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmPublicVirtualInterface' value with any optional fields omitted.
mkConfirmPublicVirtualInterface ::
  -- | 'virtualInterfaceId'
  Types.VirtualInterfaceId ->
  ConfirmPublicVirtualInterface
mkConfirmPublicVirtualInterface virtualInterfaceId =
  ConfirmPublicVirtualInterface' {virtualInterfaceId}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVirtualInterfaceId :: Lens.Lens' ConfirmPublicVirtualInterface Types.VirtualInterfaceId
cVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# DEPRECATED cVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Core.FromJSON ConfirmPublicVirtualInterface where
  toJSON ConfirmPublicVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("virtualInterfaceId" Core..= virtualInterfaceId)]
      )

instance Core.AWSRequest ConfirmPublicVirtualInterface where
  type
    Rs ConfirmPublicVirtualInterface =
      ConfirmPublicVirtualInterfaceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.ConfirmPublicVirtualInterface")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmPublicVirtualInterfaceResponse'
            Core.<$> (x Core..:? "virtualInterfaceState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkConfirmPublicVirtualInterfaceResponse' smart constructor.
data ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'
  { -- | The state of the virtual interface. The following are the possible values:
    --
    --
    --     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
    --
    --
    --     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
    --
    --
    --     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
    --
    --
    --     * @available@ : A virtual interface that is able to forward traffic.
    --
    --
    --     * @down@ : A virtual interface that is BGP down.
    --
    --
    --     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
    --
    --
    --     * @deleted@ : A virtual interface that cannot forward traffic.
    --
    --
    --     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.
    --
    --
    --     * @unknown@ : The state of the virtual interface is not available.
    virtualInterfaceState :: Core.Maybe Types.VirtualInterfaceState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmPublicVirtualInterfaceResponse' value with any optional fields omitted.
mkConfirmPublicVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfirmPublicVirtualInterfaceResponse
mkConfirmPublicVirtualInterfaceResponse responseStatus =
  ConfirmPublicVirtualInterfaceResponse'
    { virtualInterfaceState =
        Core.Nothing,
      responseStatus
    }

-- | The state of the virtual interface. The following are the possible values:
--
--
--     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
--
--
--     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
--
--
--     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
--
--
--     * @available@ : A virtual interface that is able to forward traffic.
--
--
--     * @down@ : A virtual interface that is BGP down.
--
--
--     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
--
--
--     * @deleted@ : A virtual interface that cannot forward traffic.
--
--
--     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.
--
--
--     * @unknown@ : The state of the virtual interface is not available.
--
--
--
-- /Note:/ Consider using 'virtualInterfaceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsVirtualInterfaceState :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse (Core.Maybe Types.VirtualInterfaceState)
crsVirtualInterfaceState = Lens.field @"virtualInterfaceState"
{-# DEPRECATED crsVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
