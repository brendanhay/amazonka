{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a Realtime script.
--
-- To request a script record, specify the script ID. If successful, an object containing the script properties is returned.
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
module Network.AWS.GameLift.DescribeScript
  ( -- * Creating a request
    DescribeScript (..),
    mkDescribeScript,

    -- ** Request lenses
    dScriptId,

    -- * Destructuring the response
    DescribeScriptResponse (..),
    mkDescribeScriptResponse,

    -- ** Response lenses
    dsrrsScript,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScript' smart constructor.
newtype DescribeScript = DescribeScript'
  { -- | A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
    scriptId :: Types.ScriptIdOrArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScript' value with any optional fields omitted.
mkDescribeScript ::
  -- | 'scriptId'
  Types.ScriptIdOrArn ->
  DescribeScript
mkDescribeScript scriptId = DescribeScript' {scriptId}

-- | A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScriptId :: Lens.Lens' DescribeScript Types.ScriptIdOrArn
dScriptId = Lens.field @"scriptId"
{-# DEPRECATED dScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

instance Core.FromJSON DescribeScript where
  toJSON DescribeScript {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ScriptId" Core..= scriptId)])

instance Core.AWSRequest DescribeScript where
  type Rs DescribeScript = DescribeScriptResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeScript")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScriptResponse'
            Core.<$> (x Core..:? "Script") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeScriptResponse' smart constructor.
data DescribeScriptResponse = DescribeScriptResponse'
  { -- | A set of properties describing the requested script.
    script :: Core.Maybe Types.Script,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScriptResponse' value with any optional fields omitted.
mkDescribeScriptResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScriptResponse
mkDescribeScriptResponse responseStatus =
  DescribeScriptResponse' {script = Core.Nothing, responseStatus}

-- | A set of properties describing the requested script.
--
-- /Note:/ Consider using 'script' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsScript :: Lens.Lens' DescribeScriptResponse (Core.Maybe Types.Script)
dsrrsScript = Lens.field @"script"
{-# DEPRECATED dsrrsScript "Use generic-lens or generic-optics with 'script' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeScriptResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
