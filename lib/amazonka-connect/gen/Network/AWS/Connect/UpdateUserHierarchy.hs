{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified hierarchy group to the specified user.
module Network.AWS.Connect.UpdateUserHierarchy
    (
    -- * Creating a request
      UpdateUserHierarchy (..)
    , mkUpdateUserHierarchy
    -- ** Request lenses
    , uuhUserId
    , uuhInstanceId
    , uuhHierarchyGroupId

    -- * Destructuring the response
    , UpdateUserHierarchyResponse (..)
    , mkUpdateUserHierarchyResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserHierarchy' smart constructor.
data UpdateUserHierarchy = UpdateUserHierarchy'
  { userId :: Types.UserId
    -- ^ The identifier of the user account.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , hierarchyGroupId :: Core.Maybe Types.HierarchyGroupId
    -- ^ The identifier of the hierarchy group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserHierarchy' value with any optional fields omitted.
mkUpdateUserHierarchy
    :: Types.UserId -- ^ 'userId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> UpdateUserHierarchy
mkUpdateUserHierarchy userId instanceId
  = UpdateUserHierarchy'{userId, instanceId,
                         hierarchyGroupId = Core.Nothing}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhUserId :: Lens.Lens' UpdateUserHierarchy Types.UserId
uuhUserId = Lens.field @"userId"
{-# INLINEABLE uuhUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhInstanceId :: Lens.Lens' UpdateUserHierarchy Types.InstanceId
uuhInstanceId = Lens.field @"instanceId"
{-# INLINEABLE uuhInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhHierarchyGroupId :: Lens.Lens' UpdateUserHierarchy (Core.Maybe Types.HierarchyGroupId)
uuhHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# INLINEABLE uuhHierarchyGroupId #-}
{-# DEPRECATED hierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead"  #-}

instance Core.ToQuery UpdateUserHierarchy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserHierarchy where
        toHeaders UpdateUserHierarchy{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserHierarchy where
        toJSON UpdateUserHierarchy{..}
          = Core.object
              (Core.catMaybes
                 [("HierarchyGroupId" Core..=) Core.<$> hierarchyGroupId])

instance Core.AWSRequest UpdateUserHierarchy where
        type Rs UpdateUserHierarchy = UpdateUserHierarchyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/users/" Core.<> Core.toText instanceId Core.<> "/" Core.<>
                             Core.toText userId
                             Core.<> "/hierarchy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateUserHierarchyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserHierarchyResponse' smart constructor.
data UpdateUserHierarchyResponse = UpdateUserHierarchyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserHierarchyResponse' value with any optional fields omitted.
mkUpdateUserHierarchyResponse
    :: UpdateUserHierarchyResponse
mkUpdateUserHierarchyResponse = UpdateUserHierarchyResponse'
