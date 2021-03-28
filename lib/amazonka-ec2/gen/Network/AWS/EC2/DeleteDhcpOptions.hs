{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteDhcpOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of DHCP options. You must disassociate the set of DHCP options before you can delete it. You can disassociate the set of DHCP options by associating either a new set of options or the default set of options with the VPC.
module Network.AWS.EC2.DeleteDhcpOptions
    (
    -- * Creating a request
      DeleteDhcpOptions (..)
    , mkDeleteDhcpOptions
    -- ** Request lenses
    , ddosDhcpOptionsId
    , ddosDryRun

    -- * Destructuring the response
    , DeleteDhcpOptionsResponse (..)
    , mkDeleteDhcpOptionsResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDhcpOptions' smart constructor.
data DeleteDhcpOptions = DeleteDhcpOptions'
  { dhcpOptionsId :: Types.DhcpOptionsId
    -- ^ The ID of the DHCP options set.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDhcpOptions' value with any optional fields omitted.
mkDeleteDhcpOptions
    :: Types.DhcpOptionsId -- ^ 'dhcpOptionsId'
    -> DeleteDhcpOptions
mkDeleteDhcpOptions dhcpOptionsId
  = DeleteDhcpOptions'{dhcpOptionsId, dryRun = Core.Nothing}

-- | The ID of the DHCP options set.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddosDhcpOptionsId :: Lens.Lens' DeleteDhcpOptions Types.DhcpOptionsId
ddosDhcpOptionsId = Lens.field @"dhcpOptionsId"
{-# INLINEABLE ddosDhcpOptionsId #-}
{-# DEPRECATED dhcpOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddosDryRun :: Lens.Lens' DeleteDhcpOptions (Core.Maybe Core.Bool)
ddosDryRun = Lens.field @"dryRun"
{-# INLINEABLE ddosDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteDhcpOptions where
        toQuery DeleteDhcpOptions{..}
          = Core.toQueryPair "Action" ("DeleteDhcpOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "DhcpOptionsId" dhcpOptionsId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteDhcpOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDhcpOptions where
        type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDhcpOptionsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDhcpOptionsResponse' smart constructor.
data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDhcpOptionsResponse' value with any optional fields omitted.
mkDeleteDhcpOptionsResponse
    :: DeleteDhcpOptionsResponse
mkDeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse'
