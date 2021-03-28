{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Input Security Group
module Network.AWS.MediaLive.DeleteInputSecurityGroup
    (
    -- * Creating a request
      DeleteInputSecurityGroup (..)
    , mkDeleteInputSecurityGroup
    -- ** Request lenses
    , dInputSecurityGroupId

    -- * Destructuring the response
    , DeleteInputSecurityGroupResponse (..)
    , mkDeleteInputSecurityGroupResponse
    -- ** Response lenses
    , disgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteInputSecurityGroupRequest
--
-- /See:/ 'mkDeleteInputSecurityGroup' smart constructor.
newtype DeleteInputSecurityGroup = DeleteInputSecurityGroup'
  { inputSecurityGroupId :: Core.Text
    -- ^ The Input Security Group to delete
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInputSecurityGroup' value with any optional fields omitted.
mkDeleteInputSecurityGroup
    :: Core.Text -- ^ 'inputSecurityGroupId'
    -> DeleteInputSecurityGroup
mkDeleteInputSecurityGroup inputSecurityGroupId
  = DeleteInputSecurityGroup'{inputSecurityGroupId}

-- | The Input Security Group to delete
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInputSecurityGroupId :: Lens.Lens' DeleteInputSecurityGroup Core.Text
dInputSecurityGroupId = Lens.field @"inputSecurityGroupId"
{-# INLINEABLE dInputSecurityGroupId #-}
{-# DEPRECATED inputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead"  #-}

instance Core.ToQuery DeleteInputSecurityGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInputSecurityGroup where
        toHeaders DeleteInputSecurityGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteInputSecurityGroup where
        type Rs DeleteInputSecurityGroup = DeleteInputSecurityGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/prod/inputSecurityGroups/" Core.<>
                             Core.toText inputSecurityGroupId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteInputSecurityGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DeleteInputSecurityGroupResponse
--
-- /See:/ 'mkDeleteInputSecurityGroupResponse' smart constructor.
newtype DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInputSecurityGroupResponse' value with any optional fields omitted.
mkDeleteInputSecurityGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInputSecurityGroupResponse
mkDeleteInputSecurityGroupResponse responseStatus
  = DeleteInputSecurityGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrrsResponseStatus :: Lens.Lens' DeleteInputSecurityGroupResponse Core.Int
disgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE disgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
