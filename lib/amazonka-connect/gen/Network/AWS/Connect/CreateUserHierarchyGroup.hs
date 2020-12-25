{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateUserHierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user hierarchy group.
module Network.AWS.Connect.CreateUserHierarchyGroup
  ( -- * Creating a request
    CreateUserHierarchyGroup (..),
    mkCreateUserHierarchyGroup,

    -- ** Request lenses
    cuhgName,
    cuhgInstanceId,
    cuhgParentGroupId,

    -- * Destructuring the response
    CreateUserHierarchyGroupResponse (..),
    mkCreateUserHierarchyGroupResponse,

    -- ** Response lenses
    cuhgrrsHierarchyGroupArn,
    cuhgrrsHierarchyGroupId,
    cuhgrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUserHierarchyGroup' smart constructor.
data CreateUserHierarchyGroup = CreateUserHierarchyGroup'
  { -- | The name of the user hierarchy group. Must not be more than 100 characters.
    name :: Types.Name,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier for the parent hierarchy group. The user hierarchy is created at level one if the parent group ID is null.
    parentGroupId :: Core.Maybe Types.ParentGroupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserHierarchyGroup' value with any optional fields omitted.
mkCreateUserHierarchyGroup ::
  -- | 'name'
  Types.Name ->
  -- | 'instanceId'
  Types.InstanceId ->
  CreateUserHierarchyGroup
mkCreateUserHierarchyGroup name instanceId =
  CreateUserHierarchyGroup'
    { name,
      instanceId,
      parentGroupId = Core.Nothing
    }

-- | The name of the user hierarchy group. Must not be more than 100 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgName :: Lens.Lens' CreateUserHierarchyGroup Types.Name
cuhgName = Lens.field @"name"
{-# DEPRECATED cuhgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgInstanceId :: Lens.Lens' CreateUserHierarchyGroup Types.InstanceId
cuhgInstanceId = Lens.field @"instanceId"
{-# DEPRECATED cuhgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier for the parent hierarchy group. The user hierarchy is created at level one if the parent group ID is null.
--
-- /Note:/ Consider using 'parentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgParentGroupId :: Lens.Lens' CreateUserHierarchyGroup (Core.Maybe Types.ParentGroupId)
cuhgParentGroupId = Lens.field @"parentGroupId"
{-# DEPRECATED cuhgParentGroupId "Use generic-lens or generic-optics with 'parentGroupId' instead." #-}

instance Core.FromJSON CreateUserHierarchyGroup where
  toJSON CreateUserHierarchyGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("ParentGroupId" Core..=) Core.<$> parentGroupId
          ]
      )

instance Core.AWSRequest CreateUserHierarchyGroup where
  type Rs CreateUserHierarchyGroup = CreateUserHierarchyGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/user-hierarchy-groups/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserHierarchyGroupResponse'
            Core.<$> (x Core..:? "HierarchyGroupArn")
            Core.<*> (x Core..:? "HierarchyGroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateUserHierarchyGroupResponse' smart constructor.
data CreateUserHierarchyGroupResponse = CreateUserHierarchyGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the hierarchy group.
    hierarchyGroupArn :: Core.Maybe Types.ARN,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Core.Maybe Types.HierarchyGroupId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserHierarchyGroupResponse' value with any optional fields omitted.
mkCreateUserHierarchyGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserHierarchyGroupResponse
mkCreateUserHierarchyGroupResponse responseStatus =
  CreateUserHierarchyGroupResponse'
    { hierarchyGroupArn =
        Core.Nothing,
      hierarchyGroupId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgrrsHierarchyGroupArn :: Lens.Lens' CreateUserHierarchyGroupResponse (Core.Maybe Types.ARN)
cuhgrrsHierarchyGroupArn = Lens.field @"hierarchyGroupArn"
{-# DEPRECATED cuhgrrsHierarchyGroupArn "Use generic-lens or generic-optics with 'hierarchyGroupArn' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgrrsHierarchyGroupId :: Lens.Lens' CreateUserHierarchyGroupResponse (Core.Maybe Types.HierarchyGroupId)
cuhgrrsHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# DEPRECATED cuhgrrsHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgrrsResponseStatus :: Lens.Lens' CreateUserHierarchyGroupResponse Core.Int
cuhgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cuhgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
