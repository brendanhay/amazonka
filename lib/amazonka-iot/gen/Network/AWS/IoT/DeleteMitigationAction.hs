{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a defined mitigation action from your AWS account.
module Network.AWS.IoT.DeleteMitigationAction
  ( -- * Creating a request
    DeleteMitigationAction (..),
    mkDeleteMitigationAction,

    -- ** Request lenses
    dmaActionName,

    -- * Destructuring the response
    DeleteMitigationActionResponse (..),
    mkDeleteMitigationActionResponse,

    -- ** Response lenses
    dmarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMitigationAction' smart constructor.
newtype DeleteMitigationAction = DeleteMitigationAction'
  { -- | The name of the mitigation action that you want to delete.
    actionName :: Types.MitigationActionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMitigationAction' value with any optional fields omitted.
mkDeleteMitigationAction ::
  -- | 'actionName'
  Types.MitigationActionName ->
  DeleteMitigationAction
mkDeleteMitigationAction actionName =
  DeleteMitigationAction' {actionName}

-- | The name of the mitigation action that you want to delete.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaActionName :: Lens.Lens' DeleteMitigationAction Types.MitigationActionName
dmaActionName = Lens.field @"actionName"
{-# DEPRECATED dmaActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

instance Core.AWSRequest DeleteMitigationAction where
  type Rs DeleteMitigationAction = DeleteMitigationActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/mitigationactions/actions/" Core.<> (Core.toText actionName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMitigationActionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMitigationActionResponse' smart constructor.
newtype DeleteMitigationActionResponse = DeleteMitigationActionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMitigationActionResponse' value with any optional fields omitted.
mkDeleteMitigationActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMitigationActionResponse
mkDeleteMitigationActionResponse responseStatus =
  DeleteMitigationActionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarrsResponseStatus :: Lens.Lens' DeleteMitigationActionResponse Core.Int
dmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
