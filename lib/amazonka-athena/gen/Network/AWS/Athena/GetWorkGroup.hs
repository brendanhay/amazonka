{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the workgroup with the specified name.
module Network.AWS.Athena.GetWorkGroup
  ( -- * Creating a request
    GetWorkGroup (..),
    mkGetWorkGroup,

    -- ** Request lenses
    gwgWorkGroup,

    -- * Destructuring the response
    GetWorkGroupResponse (..),
    mkGetWorkGroupResponse,

    -- ** Response lenses
    gwgrrsWorkGroup,
    gwgrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkGroup' smart constructor.
newtype GetWorkGroup = GetWorkGroup'
  { -- | The name of the workgroup.
    workGroup :: Types.WorkGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkGroup' value with any optional fields omitted.
mkGetWorkGroup ::
  -- | 'workGroup'
  Types.WorkGroupName ->
  GetWorkGroup
mkGetWorkGroup workGroup = GetWorkGroup' {workGroup}

-- | The name of the workgroup.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwgWorkGroup :: Lens.Lens' GetWorkGroup Types.WorkGroupName
gwgWorkGroup = Lens.field @"workGroup"
{-# DEPRECATED gwgWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Core.FromJSON GetWorkGroup where
  toJSON GetWorkGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkGroup" Core..= workGroup)])

instance Core.AWSRequest GetWorkGroup where
  type Rs GetWorkGroup = GetWorkGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.GetWorkGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkGroupResponse'
            Core.<$> (x Core..:? "WorkGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetWorkGroupResponse' smart constructor.
data GetWorkGroupResponse = GetWorkGroupResponse'
  { -- | Information about the workgroup.
    workGroup :: Core.Maybe Types.WorkGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetWorkGroupResponse' value with any optional fields omitted.
mkGetWorkGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetWorkGroupResponse
mkGetWorkGroupResponse responseStatus =
  GetWorkGroupResponse' {workGroup = Core.Nothing, responseStatus}

-- | Information about the workgroup.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwgrrsWorkGroup :: Lens.Lens' GetWorkGroupResponse (Core.Maybe Types.WorkGroup)
gwgrrsWorkGroup = Lens.field @"workGroup"
{-# DEPRECATED gwgrrsWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwgrrsResponseStatus :: Lens.Lens' GetWorkGroupResponse Core.Int
gwgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gwgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
