{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.ModifyMountTargetSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the set of security groups in effect for a mount target.
--
-- When you create a mount target, Amazon EFS also creates a new network interface. For more information, see 'CreateMountTarget' . This operation replaces the security groups in effect for the network interface associated with a mount target, with the @SecurityGroups@ provided in the request. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not @deleted@ . 
-- The operation requires permissions for the following actions:
--
--     * @elasticfilesystem:ModifyMountTargetSecurityGroups@ action on the mount target's file system. 
--
--
--     * @ec2:ModifyNetworkInterfaceAttribute@ action on the mount target's network interface. 
--
--
module Network.AWS.EFS.ModifyMountTargetSecurityGroups
    (
    -- * Creating a request
      ModifyMountTargetSecurityGroups (..)
    , mkModifyMountTargetSecurityGroups
    -- ** Request lenses
    , mmtsgMountTargetId
    , mmtsgSecurityGroups

    -- * Destructuring the response
    , ModifyMountTargetSecurityGroupsResponse (..)
    , mkModifyMountTargetSecurityGroupsResponse
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyMountTargetSecurityGroups' smart constructor.
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'
  { mountTargetId :: Types.MountTargetId
    -- ^ The ID of the mount target whose security groups you want to modify.
  , securityGroups :: Core.Maybe [Types.SecurityGroup]
    -- ^ An array of up to five VPC security group IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyMountTargetSecurityGroups' value with any optional fields omitted.
mkModifyMountTargetSecurityGroups
    :: Types.MountTargetId -- ^ 'mountTargetId'
    -> ModifyMountTargetSecurityGroups
mkModifyMountTargetSecurityGroups mountTargetId
  = ModifyMountTargetSecurityGroups'{mountTargetId,
                                     securityGroups = Core.Nothing}

-- | The ID of the mount target whose security groups you want to modify.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmtsgMountTargetId :: Lens.Lens' ModifyMountTargetSecurityGroups Types.MountTargetId
mmtsgMountTargetId = Lens.field @"mountTargetId"
{-# INLINEABLE mmtsgMountTargetId #-}
{-# DEPRECATED mountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead"  #-}

-- | An array of up to five VPC security group IDs.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmtsgSecurityGroups :: Lens.Lens' ModifyMountTargetSecurityGroups (Core.Maybe [Types.SecurityGroup])
mmtsgSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE mmtsgSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

instance Core.ToQuery ModifyMountTargetSecurityGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyMountTargetSecurityGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ModifyMountTargetSecurityGroups where
        toJSON ModifyMountTargetSecurityGroups{..}
          = Core.object
              (Core.catMaybes
                 [("SecurityGroups" Core..=) Core.<$> securityGroups])

instance Core.AWSRequest ModifyMountTargetSecurityGroups where
        type Rs ModifyMountTargetSecurityGroups =
             ModifyMountTargetSecurityGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2015-02-01/mount-targets/" Core.<> Core.toText mountTargetId
                             Core.<> "/security-groups",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull ModifyMountTargetSecurityGroupsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse = ModifyMountTargetSecurityGroupsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyMountTargetSecurityGroupsResponse' value with any optional fields omitted.
mkModifyMountTargetSecurityGroupsResponse
    :: ModifyMountTargetSecurityGroupsResponse
mkModifyMountTargetSecurityGroupsResponse
  = ModifyMountTargetSecurityGroupsResponse'
