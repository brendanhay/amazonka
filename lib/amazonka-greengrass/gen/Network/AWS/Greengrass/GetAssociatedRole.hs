{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetAssociatedRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the role associated with a particular group.
module Network.AWS.Greengrass.GetAssociatedRole
    (
    -- * Creating a request
      GetAssociatedRole (..)
    , mkGetAssociatedRole
    -- ** Request lenses
    , garGroupId

    -- * Destructuring the response
    , GetAssociatedRoleResponse (..)
    , mkGetAssociatedRoleResponse
    -- ** Response lenses
    , garrrsAssociatedAt
    , garrrsRoleArn
    , garrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAssociatedRole' smart constructor.
newtype GetAssociatedRole = GetAssociatedRole'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssociatedRole' value with any optional fields omitted.
mkGetAssociatedRole
    :: Core.Text -- ^ 'groupId'
    -> GetAssociatedRole
mkGetAssociatedRole groupId = GetAssociatedRole'{groupId}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garGroupId :: Lens.Lens' GetAssociatedRole Core.Text
garGroupId = Lens.field @"groupId"
{-# INLINEABLE garGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

instance Core.ToQuery GetAssociatedRole where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAssociatedRole where
        toHeaders GetAssociatedRole{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetAssociatedRole where
        type Rs GetAssociatedRole = GetAssociatedRoleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<> "/role",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAssociatedRoleResponse' Core.<$>
                   (x Core..:? "AssociatedAt") Core.<*> x Core..:? "RoleArn" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAssociatedRoleResponse' smart constructor.
data GetAssociatedRoleResponse = GetAssociatedRoleResponse'
  { associatedAt :: Core.Maybe Core.Text
    -- ^ The time when the role was associated with the group.
  , roleArn :: Core.Maybe Core.Text
    -- ^ The ARN of the role that is associated with the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssociatedRoleResponse' value with any optional fields omitted.
mkGetAssociatedRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAssociatedRoleResponse
mkGetAssociatedRoleResponse responseStatus
  = GetAssociatedRoleResponse'{associatedAt = Core.Nothing,
                               roleArn = Core.Nothing, responseStatus}

-- | The time when the role was associated with the group.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsAssociatedAt :: Lens.Lens' GetAssociatedRoleResponse (Core.Maybe Core.Text)
garrrsAssociatedAt = Lens.field @"associatedAt"
{-# INLINEABLE garrrsAssociatedAt #-}
{-# DEPRECATED associatedAt "Use generic-lens or generic-optics with 'associatedAt' instead"  #-}

-- | The ARN of the role that is associated with the group.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsRoleArn :: Lens.Lens' GetAssociatedRoleResponse (Core.Maybe Core.Text)
garrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE garrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsResponseStatus :: Lens.Lens' GetAssociatedRoleResponse Core.Int
garrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
