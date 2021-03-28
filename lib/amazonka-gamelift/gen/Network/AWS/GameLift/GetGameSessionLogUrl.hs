{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.GetGameSessionLogUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the location of stored game session logs for a specified game session. When a game session is terminated, Amazon GameLift automatically stores the logs in Amazon S3 and retains them for 14 days. Use this URL to download the logs.
--
--
--     * 'CreateGameSession' 
--
--
--     * 'DescribeGameSessions' 
--
--
--     * 'DescribeGameSessionDetails' 
--
--
--     * 'SearchGameSessions' 
--
--
--     * 'UpdateGameSession' 
--
--
--     * 'GetGameSessionLogUrl' 
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement' 
--
--
--     * 'DescribeGameSessionPlacement' 
--
--
--     * 'StopGameSessionPlacement' 
--
--
--
--
module Network.AWS.GameLift.GetGameSessionLogUrl
    (
    -- * Creating a request
      GetGameSessionLogUrl (..)
    , mkGetGameSessionLogUrl
    -- ** Request lenses
    , ggsluGameSessionId

    -- * Destructuring the response
    , GetGameSessionLogUrlResponse (..)
    , mkGetGameSessionLogUrlResponse
    -- ** Response lenses
    , ggslurrsPreSignedUrl
    , ggslurrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkGetGameSessionLogUrl' smart constructor.
newtype GetGameSessionLogUrl = GetGameSessionLogUrl'
  { gameSessionId :: Types.GameSessionId
    -- ^ A unique identifier for the game session to get logs for. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGameSessionLogUrl' value with any optional fields omitted.
mkGetGameSessionLogUrl
    :: Types.GameSessionId -- ^ 'gameSessionId'
    -> GetGameSessionLogUrl
mkGetGameSessionLogUrl gameSessionId
  = GetGameSessionLogUrl'{gameSessionId}

-- | A unique identifier for the game session to get logs for. 
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsluGameSessionId :: Lens.Lens' GetGameSessionLogUrl Types.GameSessionId
ggsluGameSessionId = Lens.field @"gameSessionId"
{-# INLINEABLE ggsluGameSessionId #-}
{-# DEPRECATED gameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead"  #-}

instance Core.ToQuery GetGameSessionLogUrl where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGameSessionLogUrl where
        toHeaders GetGameSessionLogUrl{..}
          = Core.pure ("X-Amz-Target", "GameLift.GetGameSessionLogUrl")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetGameSessionLogUrl where
        toJSON GetGameSessionLogUrl{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameSessionId" Core..= gameSessionId)])

instance Core.AWSRequest GetGameSessionLogUrl where
        type Rs GetGameSessionLogUrl = GetGameSessionLogUrlResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGameSessionLogUrlResponse' Core.<$>
                   (x Core..:? "PreSignedUrl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkGetGameSessionLogUrlResponse' smart constructor.
data GetGameSessionLogUrlResponse = GetGameSessionLogUrlResponse'
  { preSignedUrl :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Location of the requested game session logs, available for download. This URL is valid for 15 minutes, after which S3 will reject any download request using this URL. You can request a new URL any time within the 14-day period that the logs are retained.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGameSessionLogUrlResponse' value with any optional fields omitted.
mkGetGameSessionLogUrlResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGameSessionLogUrlResponse
mkGetGameSessionLogUrlResponse responseStatus
  = GetGameSessionLogUrlResponse'{preSignedUrl = Core.Nothing,
                                  responseStatus}

-- | Location of the requested game session logs, available for download. This URL is valid for 15 minutes, after which S3 will reject any download request using this URL. You can request a new URL any time within the 14-day period that the logs are retained.
--
-- /Note:/ Consider using 'preSignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggslurrsPreSignedUrl :: Lens.Lens' GetGameSessionLogUrlResponse (Core.Maybe Types.NonZeroAndMaxString)
ggslurrsPreSignedUrl = Lens.field @"preSignedUrl"
{-# INLINEABLE ggslurrsPreSignedUrl #-}
{-# DEPRECATED preSignedUrl "Use generic-lens or generic-optics with 'preSignedUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggslurrsResponseStatus :: Lens.Lens' GetGameSessionLogUrlResponse Core.Int
ggslurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggslurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
