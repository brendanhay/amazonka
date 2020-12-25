{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateStreamingURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary URL to start an AppStream 2.0 streaming session for the specified user. A streaming URL enables application streaming to be tested without user setup.
module Network.AWS.AppStream.CreateStreamingURL
  ( -- * Creating a request
    CreateStreamingURL (..),
    mkCreateStreamingURL,

    -- ** Request lenses
    csurlStackName,
    csurlFleetName,
    csurlUserId,
    csurlApplicationId,
    csurlSessionContext,
    csurlValidity,

    -- * Destructuring the response
    CreateStreamingURLResponse (..),
    mkCreateStreamingURLResponse,

    -- ** Response lenses
    csurlrrsExpires,
    csurlrrsStreamingURL,
    csurlrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStreamingURL' smart constructor.
data CreateStreamingURL = CreateStreamingURL'
  { -- | The name of the stack.
    stackName :: Types.String,
    -- | The name of the fleet.
    fleetName :: Types.String,
    -- | The identifier of the user.
    userId :: Types.UserId,
    -- | The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
    applicationId :: Core.Maybe Types.String,
    -- | The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
    sessionContext :: Core.Maybe Types.String,
    -- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
    validity :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamingURL' value with any optional fields omitted.
mkCreateStreamingURL ::
  -- | 'stackName'
  Types.String ->
  -- | 'fleetName'
  Types.String ->
  -- | 'userId'
  Types.UserId ->
  CreateStreamingURL
mkCreateStreamingURL stackName fleetName userId =
  CreateStreamingURL'
    { stackName,
      fleetName,
      userId,
      applicationId = Core.Nothing,
      sessionContext = Core.Nothing,
      validity = Core.Nothing
    }

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlStackName :: Lens.Lens' CreateStreamingURL Types.String
csurlStackName = Lens.field @"stackName"
{-# DEPRECATED csurlStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlFleetName :: Lens.Lens' CreateStreamingURL Types.String
csurlFleetName = Lens.field @"fleetName"
{-# DEPRECATED csurlFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlUserId :: Lens.Lens' CreateStreamingURL Types.UserId
csurlUserId = Lens.field @"userId"
{-# DEPRECATED csurlUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlApplicationId :: Lens.Lens' CreateStreamingURL (Core.Maybe Types.String)
csurlApplicationId = Lens.field @"applicationId"
{-# DEPRECATED csurlApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'sessionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlSessionContext :: Lens.Lens' CreateStreamingURL (Core.Maybe Types.String)
csurlSessionContext = Lens.field @"sessionContext"
{-# DEPRECATED csurlSessionContext "Use generic-lens or generic-optics with 'sessionContext' instead." #-}

-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlValidity :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Integer)
csurlValidity = Lens.field @"validity"
{-# DEPRECATED csurlValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

instance Core.FromJSON CreateStreamingURL where
  toJSON CreateStreamingURL {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackName" Core..= stackName),
            Core.Just ("FleetName" Core..= fleetName),
            Core.Just ("UserId" Core..= userId),
            ("ApplicationId" Core..=) Core.<$> applicationId,
            ("SessionContext" Core..=) Core.<$> sessionContext,
            ("Validity" Core..=) Core.<$> validity
          ]
      )

instance Core.AWSRequest CreateStreamingURL where
  type Rs CreateStreamingURL = CreateStreamingURLResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.CreateStreamingURL")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingURLResponse'
            Core.<$> (x Core..:? "Expires")
            Core.<*> (x Core..:? "StreamingURL")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
  { -- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
    expires :: Core.Maybe Core.NominalDiffTime,
    -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateStreamingURLResponse' value with any optional fields omitted.
mkCreateStreamingURLResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStreamingURLResponse
mkCreateStreamingURLResponse responseStatus =
  CreateStreamingURLResponse'
    { expires = Core.Nothing,
      streamingURL = Core.Nothing,
      responseStatus
    }

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlrrsExpires :: Lens.Lens' CreateStreamingURLResponse (Core.Maybe Core.NominalDiffTime)
csurlrrsExpires = Lens.field @"expires"
{-# DEPRECATED csurlrrsExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The URL to start the AppStream 2.0 streaming session.
--
-- /Note:/ Consider using 'streamingURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlrrsStreamingURL :: Lens.Lens' CreateStreamingURLResponse (Core.Maybe Types.String)
csurlrrsStreamingURL = Lens.field @"streamingURL"
{-# DEPRECATED csurlrrsStreamingURL "Use generic-lens or generic-optics with 'streamingURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlrrsResponseStatus :: Lens.Lens' CreateStreamingURLResponse Core.Int
csurlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csurlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
