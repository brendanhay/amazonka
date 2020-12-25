{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteSecurityGroup (..),
    mkDeleteSecurityGroup,

    -- ** Request lenses
    dsgDryRun,
    dsgGroupId,
    dsgGroupName,

    -- * Destructuring the response
    DeleteSecurityGroupResponse (..),
    mkDeleteSecurityGroupResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSecurityGroup' smart constructor.
data DeleteSecurityGroup = DeleteSecurityGroup'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the security group. Required for a nondefault VPC.
    groupId :: Core.Maybe Types.SecurityGroupId,
    -- | [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
    groupName :: Core.Maybe Types.SecurityGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityGroup' value with any optional fields omitted.
mkDeleteSecurityGroup ::
  DeleteSecurityGroup
mkDeleteSecurityGroup =
  DeleteSecurityGroup'
    { dryRun = Core.Nothing,
      groupId = Core.Nothing,
      groupName = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDryRun :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Core.Bool)
dsgDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the security group. Required for a nondefault VPC.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgGroupId :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Types.SecurityGroupId)
dsgGroupId = Lens.field @"groupId"
{-# DEPRECATED dsgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgGroupName :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Types.SecurityGroupName)
dsgGroupName = Lens.field @"groupName"
{-# DEPRECATED dsgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Core.AWSRequest DeleteSecurityGroup where
  type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse
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
            ( Core.pure ("Action", "DeleteSecurityGroup")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "GroupId" Core.<$> groupId)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
            )
      }
  response = Response.receiveNull DeleteSecurityGroupResponse'

-- | /See:/ 'mkDeleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityGroupResponse' value with any optional fields omitted.
mkDeleteSecurityGroupResponse ::
  DeleteSecurityGroupResponse
mkDeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
