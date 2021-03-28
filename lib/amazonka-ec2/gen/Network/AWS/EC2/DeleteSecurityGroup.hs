{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security group.
--
-- If you attempt to delete a security group that is associated with an instance, or is referenced by another security group, the operation fails with @InvalidGroup.InUse@ in EC2-Classic or @DependencyViolation@ in EC2-VPC.
module Network.AWS.EC2.DeleteSecurityGroup
    (
    -- * Creating a request
      DeleteSecurityGroup (..)
    , mkDeleteSecurityGroup
    -- ** Request lenses
    , dsgDryRun
    , dsgGroupId
    , dsgGroupName

    -- * Destructuring the response
    , DeleteSecurityGroupResponse (..)
    , mkDeleteSecurityGroupResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSecurityGroup' smart constructor.
data DeleteSecurityGroup = DeleteSecurityGroup'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupId :: Core.Maybe Types.SecurityGroupId
    -- ^ The ID of the security group. Required for a nondefault VPC.
  , groupName :: Core.Maybe Types.SecurityGroupName
    -- ^ [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityGroup' value with any optional fields omitted.
mkDeleteSecurityGroup
    :: DeleteSecurityGroup
mkDeleteSecurityGroup
  = DeleteSecurityGroup'{dryRun = Core.Nothing,
                         groupId = Core.Nothing, groupName = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDryRun :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Core.Bool)
dsgDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the security group. Required for a nondefault VPC.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgGroupId :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Types.SecurityGroupId)
dsgGroupId = Lens.field @"groupId"
{-# INLINEABLE dsgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgGroupName :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Types.SecurityGroupName)
dsgGroupName = Lens.field @"groupName"
{-# INLINEABLE dsgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery DeleteSecurityGroup where
        toQuery DeleteSecurityGroup{..}
          = Core.toQueryPair "Action" ("DeleteSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "GroupId") groupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName

instance Core.ToHeaders DeleteSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSecurityGroup where
        type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse
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
        parseResponse = Response.receiveNull DeleteSecurityGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityGroupResponse' value with any optional fields omitted.
mkDeleteSecurityGroupResponse
    :: DeleteSecurityGroupResponse
mkDeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
