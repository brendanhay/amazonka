{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group that can be used in Amazon WorkMail by calling the 'RegisterToWorkMail' operation.
module Network.AWS.WorkMail.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgOrganizationId,
    cgName,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrrsGroupId,
    cgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The organization under which the group is to be created.
    organizationId :: Types.OrganizationId,
    -- | The name of the group.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroup' value with any optional fields omitted.
mkCreateGroup ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'name'
  Types.Name ->
  CreateGroup
mkCreateGroup organizationId name =
  CreateGroup' {organizationId, name}

-- | The organization under which the group is to be created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgOrganizationId :: Lens.Lens' CreateGroup Types.OrganizationId
cgOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED cgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGroup Types.Name
cgName = Lens.field @"name"
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON CreateGroup where
  toJSON CreateGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.CreateGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Core.<$> (x Core..:? "GroupId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The identifier of the group.
    groupId :: Core.Maybe Types.WorkMailIdentifier,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupResponse' value with any optional fields omitted.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGroupResponse
mkCreateGroupResponse responseStatus =
  CreateGroupResponse' {groupId = Core.Nothing, responseStatus}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGroupId :: Lens.Lens' CreateGroupResponse (Core.Maybe Types.WorkMailIdentifier)
cgrrsGroupId = Lens.field @"groupId"
{-# DEPRECATED cgrrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsResponseStatus :: Lens.Lens' CreateGroupResponse Core.Int
cgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
