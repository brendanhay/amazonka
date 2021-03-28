{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkAclEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ingress or egress entry (rule) from the specified network ACL.
module Network.AWS.EC2.DeleteNetworkAclEntry
    (
    -- * Creating a request
      DeleteNetworkAclEntry (..)
    , mkDeleteNetworkAclEntry
    -- ** Request lenses
    , dnaeEgress
    , dnaeNetworkAclId
    , dnaeRuleNumber
    , dnaeDryRun

    -- * Destructuring the response
    , DeleteNetworkAclEntryResponse (..)
    , mkDeleteNetworkAclEntryResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNetworkAclEntry' smart constructor.
data DeleteNetworkAclEntry = DeleteNetworkAclEntry'
  { egress :: Core.Bool
    -- ^ Indicates whether the rule is an egress rule.
  , networkAclId :: Types.NetworkAclId
    -- ^ The ID of the network ACL.
  , ruleNumber :: Core.Int
    -- ^ The rule number of the entry to delete.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkAclEntry' value with any optional fields omitted.
mkDeleteNetworkAclEntry
    :: Core.Bool -- ^ 'egress'
    -> Types.NetworkAclId -- ^ 'networkAclId'
    -> Core.Int -- ^ 'ruleNumber'
    -> DeleteNetworkAclEntry
mkDeleteNetworkAclEntry egress networkAclId ruleNumber
  = DeleteNetworkAclEntry'{egress, networkAclId, ruleNumber,
                           dryRun = Core.Nothing}

-- | Indicates whether the rule is an egress rule.
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeEgress :: Lens.Lens' DeleteNetworkAclEntry Core.Bool
dnaeEgress = Lens.field @"egress"
{-# INLINEABLE dnaeEgress #-}
{-# DEPRECATED egress "Use generic-lens or generic-optics with 'egress' instead"  #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeNetworkAclId :: Lens.Lens' DeleteNetworkAclEntry Types.NetworkAclId
dnaeNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE dnaeNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | The rule number of the entry to delete.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeRuleNumber :: Lens.Lens' DeleteNetworkAclEntry Core.Int
dnaeRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE dnaeRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeDryRun :: Lens.Lens' DeleteNetworkAclEntry (Core.Maybe Core.Bool)
dnaeDryRun = Lens.field @"dryRun"
{-# INLINEABLE dnaeDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteNetworkAclEntry where
        toQuery DeleteNetworkAclEntry{..}
          = Core.toQueryPair "Action" ("DeleteNetworkAclEntry" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Egress" egress
              Core.<> Core.toQueryPair "NetworkAclId" networkAclId
              Core.<> Core.toQueryPair "RuleNumber" ruleNumber
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteNetworkAclEntry where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteNetworkAclEntry where
        type Rs DeleteNetworkAclEntry = DeleteNetworkAclEntryResponse
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
        parseResponse = Response.receiveNull DeleteNetworkAclEntryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNetworkAclEntryResponse' smart constructor.
data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkAclEntryResponse' value with any optional fields omitted.
mkDeleteNetworkAclEntryResponse
    :: DeleteNetworkAclEntryResponse
mkDeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse'
