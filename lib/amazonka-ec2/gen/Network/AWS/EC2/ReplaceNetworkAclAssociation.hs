{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceNetworkAclAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes which network ACL a subnet is associated with. By default when you create a subnet, it's automatically associated with the default network ACL. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This is an idempotent operation.
module Network.AWS.EC2.ReplaceNetworkAclAssociation
    (
    -- * Creating a request
      ReplaceNetworkAclAssociation (..)
    , mkReplaceNetworkAclAssociation
    -- ** Request lenses
    , rnaaAssociationId
    , rnaaNetworkAclId
    , rnaaDryRun

    -- * Destructuring the response
    , ReplaceNetworkAclAssociationResponse (..)
    , mkReplaceNetworkAclAssociationResponse
    -- ** Response lenses
    , rnaarrsNewAssociationId
    , rnaarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceNetworkAclAssociation' smart constructor.
data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation'
  { associationId :: Types.AssociationId
    -- ^ The ID of the current association between the original network ACL and the subnet.
  , networkAclId :: Types.NetworkAclId
    -- ^ The ID of the new network ACL to associate with the subnet.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceNetworkAclAssociation' value with any optional fields omitted.
mkReplaceNetworkAclAssociation
    :: Types.AssociationId -- ^ 'associationId'
    -> Types.NetworkAclId -- ^ 'networkAclId'
    -> ReplaceNetworkAclAssociation
mkReplaceNetworkAclAssociation associationId networkAclId
  = ReplaceNetworkAclAssociation'{associationId, networkAclId,
                                  dryRun = Core.Nothing}

-- | The ID of the current association between the original network ACL and the subnet.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaaAssociationId :: Lens.Lens' ReplaceNetworkAclAssociation Types.AssociationId
rnaaAssociationId = Lens.field @"associationId"
{-# INLINEABLE rnaaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The ID of the new network ACL to associate with the subnet.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaaNetworkAclId :: Lens.Lens' ReplaceNetworkAclAssociation Types.NetworkAclId
rnaaNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE rnaaNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaaDryRun :: Lens.Lens' ReplaceNetworkAclAssociation (Core.Maybe Core.Bool)
rnaaDryRun = Lens.field @"dryRun"
{-# INLINEABLE rnaaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ReplaceNetworkAclAssociation where
        toQuery ReplaceNetworkAclAssociation{..}
          = Core.toQueryPair "Action"
              ("ReplaceNetworkAclAssociation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AssociationId" associationId
              Core.<> Core.toQueryPair "NetworkAclId" networkAclId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ReplaceNetworkAclAssociation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReplaceNetworkAclAssociation where
        type Rs ReplaceNetworkAclAssociation =
             ReplaceNetworkAclAssociationResponse
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
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ReplaceNetworkAclAssociationResponse' Core.<$>
                   (x Core..@? "newAssociationId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReplaceNetworkAclAssociationResponse' smart constructor.
data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse'
  { newAssociationId :: Core.Maybe Core.Text
    -- ^ The ID of the new association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceNetworkAclAssociationResponse' value with any optional fields omitted.
mkReplaceNetworkAclAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReplaceNetworkAclAssociationResponse
mkReplaceNetworkAclAssociationResponse responseStatus
  = ReplaceNetworkAclAssociationResponse'{newAssociationId =
                                            Core.Nothing,
                                          responseStatus}

-- | The ID of the new association.
--
-- /Note:/ Consider using 'newAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaarrsNewAssociationId :: Lens.Lens' ReplaceNetworkAclAssociationResponse (Core.Maybe Core.Text)
rnaarrsNewAssociationId = Lens.field @"newAssociationId"
{-# INLINEABLE rnaarrsNewAssociationId #-}
{-# DEPRECATED newAssociationId "Use generic-lens or generic-optics with 'newAssociationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaarrsResponseStatus :: Lens.Lens' ReplaceNetworkAclAssociationResponse Core.Int
rnaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rnaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
