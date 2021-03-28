{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Updates the description of an egress (outbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.
--
-- You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
    (
    -- * Creating a request
      UpdateSecurityGroupRuleDescriptionsEgress (..)
    , mkUpdateSecurityGroupRuleDescriptionsEgress
    -- ** Request lenses
    , usgrdeIpPermissions
    , usgrdeDryRun
    , usgrdeGroupId
    , usgrdeGroupName

    -- * Destructuring the response
    , UpdateSecurityGroupRuleDescriptionsEgressResponse (..)
    , mkUpdateSecurityGroupRuleDescriptionsEgressResponse
    -- ** Response lenses
    , usgrderrsReturn
    , usgrderrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsEgress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgress = UpdateSecurityGroupRuleDescriptionsEgress'
  { ipPermissions :: [Types.IpPermission]
    -- ^ The IP permissions for the security group rule.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupId :: Core.Maybe Types.GroupId
    -- ^ The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecurityGroupRuleDescriptionsEgress' value with any optional fields omitted.
mkUpdateSecurityGroupRuleDescriptionsEgress
    :: UpdateSecurityGroupRuleDescriptionsEgress
mkUpdateSecurityGroupRuleDescriptionsEgress
  = UpdateSecurityGroupRuleDescriptionsEgress'{ipPermissions =
                                                 Core.mempty,
                                               dryRun = Core.Nothing, groupId = Core.Nothing,
                                               groupName = Core.Nothing}

-- | The IP permissions for the security group rule.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeIpPermissions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress [Types.IpPermission]
usgrdeIpPermissions = Lens.field @"ipPermissions"
{-# INLINEABLE usgrdeIpPermissions #-}
{-# DEPRECATED ipPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeDryRun :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Core.Maybe Core.Bool)
usgrdeDryRun = Lens.field @"dryRun"
{-# INLINEABLE usgrdeDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeGroupId :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Core.Maybe Types.GroupId)
usgrdeGroupId = Lens.field @"groupId"
{-# INLINEABLE usgrdeGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeGroupName :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Core.Maybe Types.GroupName)
usgrdeGroupName = Lens.field @"groupName"
{-# INLINEABLE usgrdeGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery UpdateSecurityGroupRuleDescriptionsEgress
         where
        toQuery UpdateSecurityGroupRuleDescriptionsEgress{..}
          = Core.toQueryPair "Action"
              ("UpdateSecurityGroupRuleDescriptionsEgress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "IpPermissions" ipPermissions
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "GroupId") groupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName

instance Core.ToHeaders UpdateSecurityGroupRuleDescriptionsEgress
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateSecurityGroupRuleDescriptionsEgress
         where
        type Rs UpdateSecurityGroupRuleDescriptionsEgress =
             UpdateSecurityGroupRuleDescriptionsEgressResponse
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
                 UpdateSecurityGroupRuleDescriptionsEgressResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsEgressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgressResponse = UpdateSecurityGroupRuleDescriptionsEgressResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecurityGroupRuleDescriptionsEgressResponse' value with any optional fields omitted.
mkUpdateSecurityGroupRuleDescriptionsEgressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSecurityGroupRuleDescriptionsEgressResponse
mkUpdateSecurityGroupRuleDescriptionsEgressResponse responseStatus
  = UpdateSecurityGroupRuleDescriptionsEgressResponse'{return =
                                                         Core.Nothing,
                                                       responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrderrsReturn :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse (Core.Maybe Core.Bool)
usgrderrsReturn = Lens.field @"return"
{-# INLINEABLE usgrderrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrderrsResponseStatus :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse Core.Int
usgrderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usgrderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
