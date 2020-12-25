{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log group and permanently deletes all the archived log events associated with the log group.
module Network.AWS.CloudWatchLogs.DeleteLogGroup
  ( -- * Creating a request
    DeleteLogGroup (..),
    mkDeleteLogGroup,

    -- ** Request lenses
    dlgLogGroupName,

    -- * Destructuring the response
    DeleteLogGroupResponse (..),
    mkDeleteLogGroupResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLogGroup' smart constructor.
newtype DeleteLogGroup = DeleteLogGroup'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogGroup' value with any optional fields omitted.
mkDeleteLogGroup ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  DeleteLogGroup
mkDeleteLogGroup logGroupName = DeleteLogGroup' {logGroupName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLogGroupName :: Lens.Lens' DeleteLogGroup Types.LogGroupName
dlgLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dlgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Core.FromJSON DeleteLogGroup where
  toJSON DeleteLogGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("logGroupName" Core..= logGroupName)])

instance Core.AWSRequest DeleteLogGroup where
  type Rs DeleteLogGroup = DeleteLogGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DeleteLogGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteLogGroupResponse'

-- | /See:/ 'mkDeleteLogGroupResponse' smart constructor.
data DeleteLogGroupResponse = DeleteLogGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogGroupResponse' value with any optional fields omitted.
mkDeleteLogGroupResponse ::
  DeleteLogGroupResponse
mkDeleteLogGroupResponse = DeleteLogGroupResponse'
