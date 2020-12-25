{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargetSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the security groups currently in effect for a mount target. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not @deleted@ .
--
-- This operation requires permissions for the following actions:
--
--     * @elasticfilesystem:DescribeMountTargetSecurityGroups@ action on the mount target's file system.
--
--
--     * @ec2:DescribeNetworkInterfaceAttribute@ action on the mount target's network interface.
module Network.AWS.EFS.DescribeMountTargetSecurityGroups
  ( -- * Creating a request
    DescribeMountTargetSecurityGroups (..),
    mkDescribeMountTargetSecurityGroups,

    -- ** Request lenses
    dmtsgMountTargetId,

    -- * Destructuring the response
    DescribeMountTargetSecurityGroupsResponse (..),
    mkDescribeMountTargetSecurityGroupsResponse,

    -- ** Response lenses
    dmtsgrrsSecurityGroups,
    dmtsgrrsResponseStatus,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeMountTargetSecurityGroups' smart constructor.
newtype DescribeMountTargetSecurityGroups = DescribeMountTargetSecurityGroups'
  { -- | The ID of the mount target whose security groups you want to retrieve.
    mountTargetId :: Types.MountTargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMountTargetSecurityGroups' value with any optional fields omitted.
mkDescribeMountTargetSecurityGroups ::
  -- | 'mountTargetId'
  Types.MountTargetId ->
  DescribeMountTargetSecurityGroups
mkDescribeMountTargetSecurityGroups mountTargetId =
  DescribeMountTargetSecurityGroups' {mountTargetId}

-- | The ID of the mount target whose security groups you want to retrieve.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsgMountTargetId :: Lens.Lens' DescribeMountTargetSecurityGroups Types.MountTargetId
dmtsgMountTargetId = Lens.field @"mountTargetId"
{-# DEPRECATED dmtsgMountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead." #-}

instance Core.AWSRequest DescribeMountTargetSecurityGroups where
  type
    Rs DescribeMountTargetSecurityGroups =
      DescribeMountTargetSecurityGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-02-01/mount-targets/" Core.<> (Core.toText mountTargetId)
                Core.<> ("/security-groups")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMountTargetSecurityGroupsResponse'
            Core.<$> (x Core..:? "SecurityGroups" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeMountTargetSecurityGroupsResponse' smart constructor.
data DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse'
  { -- | An array of security groups.
    securityGroups :: [Types.SecurityGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMountTargetSecurityGroupsResponse' value with any optional fields omitted.
mkDescribeMountTargetSecurityGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMountTargetSecurityGroupsResponse
mkDescribeMountTargetSecurityGroupsResponse responseStatus =
  DescribeMountTargetSecurityGroupsResponse'
    { securityGroups =
        Core.mempty,
      responseStatus
    }

-- | An array of security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsgrrsSecurityGroups :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse [Types.SecurityGroup]
dmtsgrrsSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED dmtsgrrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtsgrrsResponseStatus :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse Core.Int
dmtsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmtsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
