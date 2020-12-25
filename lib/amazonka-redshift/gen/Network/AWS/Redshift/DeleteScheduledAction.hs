{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled action.
module Network.AWS.Redshift.DeleteScheduledAction
  ( -- * Creating a request
    DeleteScheduledAction (..),
    mkDeleteScheduledAction,

    -- ** Request lenses
    dsaScheduledActionName,

    -- * Destructuring the response
    DeleteScheduledActionResponse (..),
    mkDeleteScheduledActionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteScheduledAction' smart constructor.
newtype DeleteScheduledAction = DeleteScheduledAction'
  { -- | The name of the scheduled action to delete.
    scheduledActionName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduledAction' value with any optional fields omitted.
mkDeleteScheduledAction ::
  -- | 'scheduledActionName'
  Types.String ->
  DeleteScheduledAction
mkDeleteScheduledAction scheduledActionName =
  DeleteScheduledAction' {scheduledActionName}

-- | The name of the scheduled action to delete.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaScheduledActionName :: Lens.Lens' DeleteScheduledAction Types.String
dsaScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED dsaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

instance Core.AWSRequest DeleteScheduledAction where
  type Rs DeleteScheduledAction = DeleteScheduledActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteScheduledAction")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ScheduledActionName" scheduledActionName)
            )
      }
  response = Response.receiveNull DeleteScheduledActionResponse'

-- | /See:/ 'mkDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduledActionResponse' value with any optional fields omitted.
mkDeleteScheduledActionResponse ::
  DeleteScheduledActionResponse
mkDeleteScheduledActionResponse = DeleteScheduledActionResponse'
