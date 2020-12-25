{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DeleteProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified protection group.
module Network.AWS.Shield.DeleteProtectionGroup
  ( -- * Creating a request
    DeleteProtectionGroup (..),
    mkDeleteProtectionGroup,

    -- ** Request lenses
    dpgProtectionGroupId,

    -- * Destructuring the response
    DeleteProtectionGroupResponse (..),
    mkDeleteProtectionGroupResponse,

    -- ** Response lenses
    dpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDeleteProtectionGroup' smart constructor.
newtype DeleteProtectionGroup = DeleteProtectionGroup'
  { -- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
    protectionGroupId :: Types.ProtectionGroupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProtectionGroup' value with any optional fields omitted.
mkDeleteProtectionGroup ::
  -- | 'protectionGroupId'
  Types.ProtectionGroupId ->
  DeleteProtectionGroup
mkDeleteProtectionGroup protectionGroupId =
  DeleteProtectionGroup' {protectionGroupId}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgProtectionGroupId :: Lens.Lens' DeleteProtectionGroup Types.ProtectionGroupId
dpgProtectionGroupId = Lens.field @"protectionGroupId"
{-# DEPRECATED dpgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

instance Core.FromJSON DeleteProtectionGroup where
  toJSON DeleteProtectionGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ProtectionGroupId" Core..= protectionGroupId)]
      )

instance Core.AWSRequest DeleteProtectionGroup where
  type Rs DeleteProtectionGroup = DeleteProtectionGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.DeleteProtectionGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProtectionGroupResponse' smart constructor.
newtype DeleteProtectionGroupResponse = DeleteProtectionGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProtectionGroupResponse' value with any optional fields omitted.
mkDeleteProtectionGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProtectionGroupResponse
mkDeleteProtectionGroupResponse responseStatus =
  DeleteProtectionGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsResponseStatus :: Lens.Lens' DeleteProtectionGroupResponse Core.Int
dpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
