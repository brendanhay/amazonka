{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UpdateSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing subnet group.
module Network.AWS.DAX.UpdateSubnetGroup
    (
    -- * Creating a request
      UpdateSubnetGroup (..)
    , mkUpdateSubnetGroup
    -- ** Request lenses
    , usgSubnetGroupName
    , usgDescription
    , usgSubnetIds

    -- * Destructuring the response
    , UpdateSubnetGroupResponse (..)
    , mkUpdateSubnetGroupResponse
    -- ** Response lenses
    , usgrrsSubnetGroup
    , usgrrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSubnetGroup' smart constructor.
data UpdateSubnetGroup = UpdateSubnetGroup'
  { subnetGroupName :: Core.Text
    -- ^ The name of the subnet group.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the subnet group.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ A list of subnet IDs in the subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubnetGroup' value with any optional fields omitted.
mkUpdateSubnetGroup
    :: Core.Text -- ^ 'subnetGroupName'
    -> UpdateSubnetGroup
mkUpdateSubnetGroup subnetGroupName
  = UpdateSubnetGroup'{subnetGroupName, description = Core.Nothing,
                       subnetIds = Core.Nothing}

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSubnetGroupName :: Lens.Lens' UpdateSubnetGroup Core.Text
usgSubnetGroupName = Lens.field @"subnetGroupName"
{-# INLINEABLE usgSubnetGroupName #-}
{-# DEPRECATED subnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead"  #-}

-- | A description of the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgDescription :: Lens.Lens' UpdateSubnetGroup (Core.Maybe Core.Text)
usgDescription = Lens.field @"description"
{-# INLINEABLE usgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of subnet IDs in the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSubnetIds :: Lens.Lens' UpdateSubnetGroup (Core.Maybe [Core.Text])
usgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE usgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.ToQuery UpdateSubnetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSubnetGroup where
        toHeaders UpdateSubnetGroup{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.UpdateSubnetGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSubnetGroup where
        toJSON UpdateSubnetGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SubnetGroupName" Core..= subnetGroupName),
                  ("Description" Core..=) Core.<$> description,
                  ("SubnetIds" Core..=) Core.<$> subnetIds])

instance Core.AWSRequest UpdateSubnetGroup where
        type Rs UpdateSubnetGroup = UpdateSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSubnetGroupResponse' Core.<$>
                   (x Core..:? "SubnetGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSubnetGroupResponse' smart constructor.
data UpdateSubnetGroupResponse = UpdateSubnetGroupResponse'
  { subnetGroup :: Core.Maybe Types.SubnetGroup
    -- ^ The subnet group that has been modified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubnetGroupResponse' value with any optional fields omitted.
mkUpdateSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSubnetGroupResponse
mkUpdateSubnetGroupResponse responseStatus
  = UpdateSubnetGroupResponse'{subnetGroup = Core.Nothing,
                               responseStatus}

-- | The subnet group that has been modified.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrrsSubnetGroup :: Lens.Lens' UpdateSubnetGroupResponse (Core.Maybe Types.SubnetGroup)
usgrrsSubnetGroup = Lens.field @"subnetGroup"
{-# INLINEABLE usgrrsSubnetGroup #-}
{-# DEPRECATED subnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrrsResponseStatus :: Lens.Lens' UpdateSubnetGroupResponse Core.Int
usgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
