{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network ACL. You can't delete the ACL if it's associated with any subnets. You can't delete the default network ACL.
module Network.AWS.EC2.DeleteNetworkAcl
    (
    -- * Creating a request
      DeleteNetworkAcl (..)
    , mkDeleteNetworkAcl
    -- ** Request lenses
    , dnaNetworkAclId
    , dnaDryRun

    -- * Destructuring the response
    , DeleteNetworkAclResponse (..)
    , mkDeleteNetworkAclResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNetworkAcl' smart constructor.
data DeleteNetworkAcl = DeleteNetworkAcl'
  { networkAclId :: Types.NetworkAclId
    -- ^ The ID of the network ACL.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkAcl' value with any optional fields omitted.
mkDeleteNetworkAcl
    :: Types.NetworkAclId -- ^ 'networkAclId'
    -> DeleteNetworkAcl
mkDeleteNetworkAcl networkAclId
  = DeleteNetworkAcl'{networkAclId, dryRun = Core.Nothing}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaNetworkAclId :: Lens.Lens' DeleteNetworkAcl Types.NetworkAclId
dnaNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE dnaNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaDryRun :: Lens.Lens' DeleteNetworkAcl (Core.Maybe Core.Bool)
dnaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dnaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteNetworkAcl where
        toQuery DeleteNetworkAcl{..}
          = Core.toQueryPair "Action" ("DeleteNetworkAcl" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkAclId" networkAclId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteNetworkAcl where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteNetworkAcl where
        type Rs DeleteNetworkAcl = DeleteNetworkAclResponse
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
        parseResponse = Response.receiveNull DeleteNetworkAclResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNetworkAclResponse' smart constructor.
data DeleteNetworkAclResponse = DeleteNetworkAclResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkAclResponse' value with any optional fields omitted.
mkDeleteNetworkAclResponse
    :: DeleteNetworkAclResponse
mkDeleteNetworkAclResponse = DeleteNetworkAclResponse'
