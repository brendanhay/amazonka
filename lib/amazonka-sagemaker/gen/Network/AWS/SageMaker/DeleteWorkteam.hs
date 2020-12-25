{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing work team. This operation can't be undone.
module Network.AWS.SageMaker.DeleteWorkteam
  ( -- * Creating a request
    DeleteWorkteam (..),
    mkDeleteWorkteam,

    -- ** Request lenses
    dwWorkteamName,

    -- * Destructuring the response
    DeleteWorkteamResponse (..),
    mkDeleteWorkteamResponse,

    -- ** Response lenses
    dwrfrsSuccess,
    dwrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteWorkteam' smart constructor.
newtype DeleteWorkteam = DeleteWorkteam'
  { -- | The name of the work team to delete.
    workteamName :: Types.WorkteamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkteam' value with any optional fields omitted.
mkDeleteWorkteam ::
  -- | 'workteamName'
  Types.WorkteamName ->
  DeleteWorkteam
mkDeleteWorkteam workteamName = DeleteWorkteam' {workteamName}

-- | The name of the work team to delete.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwWorkteamName :: Lens.Lens' DeleteWorkteam Types.WorkteamName
dwWorkteamName = Lens.field @"workteamName"
{-# DEPRECATED dwWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

instance Core.FromJSON DeleteWorkteam where
  toJSON DeleteWorkteam {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkteamName" Core..= workteamName)])

instance Core.AWSRequest DeleteWorkteam where
  type Rs DeleteWorkteam = DeleteWorkteamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteWorkteam")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkteamResponse'
            Core.<$> (x Core..: "Success") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteWorkteamResponse' smart constructor.
data DeleteWorkteamResponse = DeleteWorkteamResponse'
  { -- | Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
    success :: Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkteamResponse' value with any optional fields omitted.
mkDeleteWorkteamResponse ::
  -- | 'success'
  Core.Bool ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteWorkteamResponse
mkDeleteWorkteamResponse success responseStatus =
  DeleteWorkteamResponse' {success, responseStatus}

-- | Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrfrsSuccess :: Lens.Lens' DeleteWorkteamResponse Core.Bool
dwrfrsSuccess = Lens.field @"success"
{-# DEPRECATED dwrfrsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrfrsResponseStatus :: Lens.Lens' DeleteWorkteamResponse Core.Int
dwrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
