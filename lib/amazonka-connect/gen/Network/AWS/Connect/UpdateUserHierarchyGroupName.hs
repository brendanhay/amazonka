{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchyGroupName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of the user hierarchy group.
module Network.AWS.Connect.UpdateUserHierarchyGroupName
  ( -- * Creating a request
    UpdateUserHierarchyGroupName (..),
    mkUpdateUserHierarchyGroupName,

    -- ** Request lenses
    uuhgnName,
    uuhgnHierarchyGroupId,
    uuhgnInstanceId,

    -- * Destructuring the response
    UpdateUserHierarchyGroupNameResponse (..),
    mkUpdateUserHierarchyGroupNameResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserHierarchyGroupName' smart constructor.
data UpdateUserHierarchyGroupName = UpdateUserHierarchyGroupName'
  { -- | The name of the hierarchy group. Must not be more than 100 characters.
    name :: Types.HierarchyGroupName,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Types.HierarchyGroupId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserHierarchyGroupName' value with any optional fields omitted.
mkUpdateUserHierarchyGroupName ::
  -- | 'name'
  Types.HierarchyGroupName ->
  -- | 'hierarchyGroupId'
  Types.HierarchyGroupId ->
  -- | 'instanceId'
  Types.InstanceId ->
  UpdateUserHierarchyGroupName
mkUpdateUserHierarchyGroupName name hierarchyGroupId instanceId =
  UpdateUserHierarchyGroupName' {name, hierarchyGroupId, instanceId}

-- | The name of the hierarchy group. Must not be more than 100 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhgnName :: Lens.Lens' UpdateUserHierarchyGroupName Types.HierarchyGroupName
uuhgnName = Lens.field @"name"
{-# DEPRECATED uuhgnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhgnHierarchyGroupId :: Lens.Lens' UpdateUserHierarchyGroupName Types.HierarchyGroupId
uuhgnHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# DEPRECATED uuhgnHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhgnInstanceId :: Lens.Lens' UpdateUserHierarchyGroupName Types.InstanceId
uuhgnInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uuhgnInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UpdateUserHierarchyGroupName where
  toJSON UpdateUserHierarchyGroupName {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest UpdateUserHierarchyGroupName where
  type
    Rs UpdateUserHierarchyGroupName =
      UpdateUserHierarchyGroupNameResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/user-hierarchy-groups/" Core.<> (Core.toText instanceId)
                Core.<> ("/")
                Core.<> (Core.toText hierarchyGroupId)
                Core.<> ("/name")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull UpdateUserHierarchyGroupNameResponse'

-- | /See:/ 'mkUpdateUserHierarchyGroupNameResponse' smart constructor.
data UpdateUserHierarchyGroupNameResponse = UpdateUserHierarchyGroupNameResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserHierarchyGroupNameResponse' value with any optional fields omitted.
mkUpdateUserHierarchyGroupNameResponse ::
  UpdateUserHierarchyGroupNameResponse
mkUpdateUserHierarchyGroupNameResponse =
  UpdateUserHierarchyGroupNameResponse'
