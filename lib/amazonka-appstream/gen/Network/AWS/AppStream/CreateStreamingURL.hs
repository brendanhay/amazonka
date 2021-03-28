{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStreamingURL (..)
    , mkCreateStreamingURL
    -- ** Request lenses
    , csurlStackName
    , csurlFleetName
    , csurlUserId
    , csurlApplicationId
    , csurlSessionContext
    , csurlValidity

    -- * Destructuring the response
    , CreateStreamingURLResponse (..)
    , mkCreateStreamingURLResponse
    -- ** Response lenses
    , csurlrrsExpires
    , csurlrrsStreamingURL
    , csurlrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStreamingURL' smart constructor.
data CreateStreamingURL = CreateStreamingURL'
  { stackName :: Core.Text
    -- ^ The name of the stack.
  , fleetName :: Core.Text
    -- ^ The name of the fleet.
  , userId :: Types.UserId
    -- ^ The identifier of the user.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
  , sessionContext :: Core.Maybe Core.Text
    -- ^ The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
  , validity :: Core.Maybe Core.Integer
    -- ^ The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamingURL' value with any optional fields omitted.
mkCreateStreamingURL
    :: Core.Text -- ^ 'stackName'
    -> Core.Text -- ^ 'fleetName'
    -> Types.UserId -- ^ 'userId'
    -> CreateStreamingURL
mkCreateStreamingURL stackName fleetName userId
  = CreateStreamingURL'{stackName, fleetName, userId,
                        applicationId = Core.Nothing, sessionContext = Core.Nothing,
                        validity = Core.Nothing}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlStackName :: Lens.Lens' CreateStreamingURL Core.Text
csurlStackName = Lens.field @"stackName"
{-# INLINEABLE csurlStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlFleetName :: Lens.Lens' CreateStreamingURL Core.Text
csurlFleetName = Lens.field @"fleetName"
{-# INLINEABLE csurlFleetName #-}
{-# DEPRECATED fleetName "Use generic-lens or generic-optics with 'fleetName' instead"  #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlUserId :: Lens.Lens' CreateStreamingURL Types.UserId
csurlUserId = Lens.field @"userId"
{-# INLINEABLE csurlUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlApplicationId :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Text)
csurlApplicationId = Lens.field @"applicationId"
{-# INLINEABLE csurlApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'sessionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlSessionContext :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Text)
csurlSessionContext = Lens.field @"sessionContext"
{-# INLINEABLE csurlSessionContext #-}
{-# DEPRECATED sessionContext "Use generic-lens or generic-optics with 'sessionContext' instead"  #-}

-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlValidity :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Integer)
csurlValidity = Lens.field @"validity"
{-# INLINEABLE csurlValidity #-}
{-# DEPRECATED validity "Use generic-lens or generic-optics with 'validity' instead"  #-}

instance Core.ToQuery CreateStreamingURL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStreamingURL where
        toHeaders CreateStreamingURL{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.CreateStreamingURL")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateStreamingURL where
        toJSON CreateStreamingURL{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackName" Core..= stackName),
                  Core.Just ("FleetName" Core..= fleetName),
                  Core.Just ("UserId" Core..= userId),
                  ("ApplicationId" Core..=) Core.<$> applicationId,
                  ("SessionContext" Core..=) Core.<$> sessionContext,
                  ("Validity" Core..=) Core.<$> validity])

instance Core.AWSRequest CreateStreamingURL where
        type Rs CreateStreamingURL = CreateStreamingURLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateStreamingURLResponse' Core.<$>
                   (x Core..:? "Expires") Core.<*> x Core..:? "StreamingURL" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
  { expires :: Core.Maybe Core.NominalDiffTime
    -- ^ The elapsed time, in seconds after the Unix epoch, when this URL expires.
  , streamingURL :: Core.Maybe Core.Text
    -- ^ The URL to start the AppStream 2.0 streaming session.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateStreamingURLResponse' value with any optional fields omitted.
mkCreateStreamingURLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStreamingURLResponse
mkCreateStreamingURLResponse responseStatus
  = CreateStreamingURLResponse'{expires = Core.Nothing,
                                streamingURL = Core.Nothing, responseStatus}

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlrrsExpires :: Lens.Lens' CreateStreamingURLResponse (Core.Maybe Core.NominalDiffTime)
csurlrrsExpires = Lens.field @"expires"
{-# INLINEABLE csurlrrsExpires #-}
{-# DEPRECATED expires "Use generic-lens or generic-optics with 'expires' instead"  #-}

-- | The URL to start the AppStream 2.0 streaming session.
--
-- /Note:/ Consider using 'streamingURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlrrsStreamingURL :: Lens.Lens' CreateStreamingURLResponse (Core.Maybe Core.Text)
csurlrrsStreamingURL = Lens.field @"streamingURL"
{-# INLINEABLE csurlrrsStreamingURL #-}
{-# DEPRECATED streamingURL "Use generic-lens or generic-optics with 'streamingURL' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csurlrrsResponseStatus :: Lens.Lens' CreateStreamingURLResponse Core.Int
csurlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csurlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
