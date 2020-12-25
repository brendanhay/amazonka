{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Realtime script. This operation permanently deletes the script record. If script files were uploaded, they are also deleted (files stored in an S3 bucket are not deleted).
--
-- To delete a script, specify the script ID. Before deleting a script, be sure to terminate all fleets that are deployed with the script being deleted. Fleet instances periodically check for script updates, and if the script record no longer exists, the instance will go into an error state and be unable to host game sessions.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
-- __Related operations__
--
--     * 'CreateScript'
--
--
--     * 'ListScripts'
--
--
--     * 'DescribeScript'
--
--
--     * 'UpdateScript'
--
--
--     * 'DeleteScript'
module Network.AWS.GameLift.DeleteScript
  ( -- * Creating a request
    DeleteScript (..),
    mkDeleteScript,

    -- ** Request lenses
    dsScriptId,

    -- * Destructuring the response
    DeleteScriptResponse (..),
    mkDeleteScriptResponse,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteScript' smart constructor.
newtype DeleteScript = DeleteScript'
  { -- | A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
    scriptId :: Types.ScriptIdOrArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScript' value with any optional fields omitted.
mkDeleteScript ::
  -- | 'scriptId'
  Types.ScriptIdOrArn ->
  DeleteScript
mkDeleteScript scriptId = DeleteScript' {scriptId}

-- | A unique identifier for a Realtime script to delete. You can use either the script ID or ARN value.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsScriptId :: Lens.Lens' DeleteScript Types.ScriptIdOrArn
dsScriptId = Lens.field @"scriptId"
{-# DEPRECATED dsScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

instance Core.FromJSON DeleteScript where
  toJSON DeleteScript {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ScriptId" Core..= scriptId)])

instance Core.AWSRequest DeleteScript where
  type Rs DeleteScript = DeleteScriptResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DeleteScript")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteScriptResponse'

-- | /See:/ 'mkDeleteScriptResponse' smart constructor.
data DeleteScriptResponse = DeleteScriptResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScriptResponse' value with any optional fields omitted.
mkDeleteScriptResponse ::
  DeleteScriptResponse
mkDeleteScriptResponse = DeleteScriptResponse'
