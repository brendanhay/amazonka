{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified resource group.
module Network.AWS.ResourceGroups.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggGroup,
    ggGroupName,

    -- * Destructuring the response
    GetGroupResponse (..),
    mkGetGroupResponse,

    -- ** Response lenses
    ggrrsGroup,
    ggrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The name or the ARN of the resource group to retrieve.
    group :: Core.Maybe Types.GroupString,
    -- | Don't use this parameter. Use @Group@ instead.
    groupName :: Core.Maybe Types.GroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroup' value with any optional fields omitted.
mkGetGroup ::
  GetGroup
mkGetGroup =
  GetGroup' {group = Core.Nothing, groupName = Core.Nothing}

-- | The name or the ARN of the resource group to retrieve.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroup :: Lens.Lens' GetGroup (Core.Maybe Types.GroupString)
ggGroup = Lens.field @"group"
{-# DEPRECATED ggGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup (Core.Maybe Types.GroupName)
ggGroupName = Lens.field @"groupName"
{-# DEPRECATED ggGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Core.FromJSON GetGroup where
  toJSON GetGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("Group" Core..=) Core.<$> group,
            ("GroupName" Core..=) Core.<$> groupName
          ]
      )

instance Core.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/get-group",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Core.<$> (x Core..:? "Group") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | A full description of the resource group.
    group :: Core.Maybe Types.Group,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupResponse' value with any optional fields omitted.
mkGetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGroupResponse
mkGetGroupResponse responseStatus =
  GetGroupResponse' {group = Core.Nothing, responseStatus}

-- | A full description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGroup :: Lens.Lens' GetGroupResponse (Core.Maybe Types.Group)
ggrrsGroup = Lens.field @"group"
{-# DEPRECATED ggrrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGroupResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
