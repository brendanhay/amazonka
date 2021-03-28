{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DeleteUserHierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing user hierarchy group. It must not be associated with any agents or have any active child groups.
module Network.AWS.Connect.DeleteUserHierarchyGroup
    (
    -- * Creating a request
      DeleteUserHierarchyGroup (..)
    , mkDeleteUserHierarchyGroup
    -- ** Request lenses
    , duhgfHierarchyGroupId
    , duhgfInstanceId

    -- * Destructuring the response
    , DeleteUserHierarchyGroupResponse (..)
    , mkDeleteUserHierarchyGroupResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserHierarchyGroup' smart constructor.
data DeleteUserHierarchyGroup = DeleteUserHierarchyGroup'
  { hierarchyGroupId :: Types.HierarchyGroupId
    -- ^ The identifier of the hierarchy group.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserHierarchyGroup' value with any optional fields omitted.
mkDeleteUserHierarchyGroup
    :: Types.HierarchyGroupId -- ^ 'hierarchyGroupId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> DeleteUserHierarchyGroup
mkDeleteUserHierarchyGroup hierarchyGroupId instanceId
  = DeleteUserHierarchyGroup'{hierarchyGroupId, instanceId}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhgfHierarchyGroupId :: Lens.Lens' DeleteUserHierarchyGroup Types.HierarchyGroupId
duhgfHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# INLINEABLE duhgfHierarchyGroupId #-}
{-# DEPRECATED hierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhgfInstanceId :: Lens.Lens' DeleteUserHierarchyGroup Types.InstanceId
duhgfInstanceId = Lens.field @"instanceId"
{-# INLINEABLE duhgfInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery DeleteUserHierarchyGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserHierarchyGroup where
        toHeaders DeleteUserHierarchyGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteUserHierarchyGroup where
        type Rs DeleteUserHierarchyGroup = DeleteUserHierarchyGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/user-hierarchy-groups/" Core.<> Core.toText instanceId Core.<>
                             "/"
                             Core.<> Core.toText hierarchyGroupId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteUserHierarchyGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserHierarchyGroupResponse' smart constructor.
data DeleteUserHierarchyGroupResponse = DeleteUserHierarchyGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserHierarchyGroupResponse' value with any optional fields omitted.
mkDeleteUserHierarchyGroupResponse
    :: DeleteUserHierarchyGroupResponse
mkDeleteUserHierarchyGroupResponse
  = DeleteUserHierarchyGroupResponse'
