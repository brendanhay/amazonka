{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ResumeSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reconnects a session to an instance after it has been disconnected. Connections can be resumed for disconnected sessions, but not terminated sessions.
module Network.AWS.SSM.ResumeSession
    (
    -- * Creating a request
      ResumeSession (..)
    , mkResumeSession
    -- ** Request lenses
    , rsSessionId

    -- * Destructuring the response
    , ResumeSessionResponse (..)
    , mkResumeSessionResponse
    -- ** Response lenses
    , rsrrsSessionId
    , rsrrsStreamUrl
    , rsrrsTokenValue
    , rsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkResumeSession' smart constructor.
newtype ResumeSession = ResumeSession'
  { sessionId :: Types.SessionId
    -- ^ The ID of the disconnected session to resume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeSession' value with any optional fields omitted.
mkResumeSession
    :: Types.SessionId -- ^ 'sessionId'
    -> ResumeSession
mkResumeSession sessionId = ResumeSession'{sessionId}

-- | The ID of the disconnected session to resume.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSessionId :: Lens.Lens' ResumeSession Types.SessionId
rsSessionId = Lens.field @"sessionId"
{-# INLINEABLE rsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

instance Core.ToQuery ResumeSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResumeSession where
        toHeaders ResumeSession{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.ResumeSession") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResumeSession where
        toJSON ResumeSession{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SessionId" Core..= sessionId)])

instance Core.AWSRequest ResumeSession where
        type Rs ResumeSession = ResumeSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResumeSessionResponse' Core.<$>
                   (x Core..:? "SessionId") Core.<*> x Core..:? "StreamUrl" Core.<*>
                     x Core..:? "TokenValue"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResumeSessionResponse' smart constructor.
data ResumeSessionResponse = ResumeSessionResponse'
  { sessionId :: Core.Maybe Types.SessionId
    -- ^ The ID of the session.
  , streamUrl :: Core.Maybe Types.StreamUrl
    -- ^ A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@ .
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
  , tokenValue :: Core.Maybe Types.TokenValue
    -- ^ An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeSessionResponse' value with any optional fields omitted.
mkResumeSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResumeSessionResponse
mkResumeSessionResponse responseStatus
  = ResumeSessionResponse'{sessionId = Core.Nothing,
                           streamUrl = Core.Nothing, tokenValue = Core.Nothing,
                           responseStatus}

-- | The ID of the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsSessionId :: Lens.Lens' ResumeSessionResponse (Core.Maybe Types.SessionId)
rsrrsSessionId = Lens.field @"sessionId"
{-# INLINEABLE rsrrsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

-- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@ .
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
--
-- /Note:/ Consider using 'streamUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsStreamUrl :: Lens.Lens' ResumeSessionResponse (Core.Maybe Types.StreamUrl)
rsrrsStreamUrl = Lens.field @"streamUrl"
{-# INLINEABLE rsrrsStreamUrl #-}
{-# DEPRECATED streamUrl "Use generic-lens or generic-optics with 'streamUrl' instead"  #-}

-- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
--
-- /Note:/ Consider using 'tokenValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsTokenValue :: Lens.Lens' ResumeSessionResponse (Core.Maybe Types.TokenValue)
rsrrsTokenValue = Lens.field @"tokenValue"
{-# INLINEABLE rsrrsTokenValue #-}
{-# DEPRECATED tokenValue "Use generic-lens or generic-optics with 'tokenValue' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsResponseStatus :: Lens.Lens' ResumeSessionResponse Core.Int
rsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
