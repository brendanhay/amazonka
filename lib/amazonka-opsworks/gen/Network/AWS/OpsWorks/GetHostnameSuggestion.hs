{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a generated host name for the specified layer, based on the current host name theme.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.GetHostnameSuggestion
    (
    -- * Creating a request
      GetHostnameSuggestion (..)
    , mkGetHostnameSuggestion
    -- ** Request lenses
    , ghsLayerId

    -- * Destructuring the response
    , GetHostnameSuggestionResponse (..)
    , mkGetHostnameSuggestionResponse
    -- ** Response lenses
    , ghsrrsHostname
    , ghsrrsLayerId
    , ghsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetHostnameSuggestion' smart constructor.
newtype GetHostnameSuggestion = GetHostnameSuggestion'
  { layerId :: Core.Text
    -- ^ The layer ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostnameSuggestion' value with any optional fields omitted.
mkGetHostnameSuggestion
    :: Core.Text -- ^ 'layerId'
    -> GetHostnameSuggestion
mkGetHostnameSuggestion layerId = GetHostnameSuggestion'{layerId}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsLayerId :: Lens.Lens' GetHostnameSuggestion Core.Text
ghsLayerId = Lens.field @"layerId"
{-# INLINEABLE ghsLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

instance Core.ToQuery GetHostnameSuggestion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetHostnameSuggestion where
        toHeaders GetHostnameSuggestion{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.GetHostnameSuggestion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetHostnameSuggestion where
        toJSON GetHostnameSuggestion{..}
          = Core.object
              (Core.catMaybes [Core.Just ("LayerId" Core..= layerId)])

instance Core.AWSRequest GetHostnameSuggestion where
        type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetHostnameSuggestionResponse' Core.<$>
                   (x Core..:? "Hostname") Core.<*> x Core..:? "LayerId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @GetHostnameSuggestion@ request.
--
-- /See:/ 'mkGetHostnameSuggestionResponse' smart constructor.
data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse'
  { hostname :: Core.Maybe Core.Text
    -- ^ The generated host name.
  , layerId :: Core.Maybe Core.Text
    -- ^ The layer ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostnameSuggestionResponse' value with any optional fields omitted.
mkGetHostnameSuggestionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetHostnameSuggestionResponse
mkGetHostnameSuggestionResponse responseStatus
  = GetHostnameSuggestionResponse'{hostname = Core.Nothing,
                                   layerId = Core.Nothing, responseStatus}

-- | The generated host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsrrsHostname :: Lens.Lens' GetHostnameSuggestionResponse (Core.Maybe Core.Text)
ghsrrsHostname = Lens.field @"hostname"
{-# INLINEABLE ghsrrsHostname #-}
{-# DEPRECATED hostname "Use generic-lens or generic-optics with 'hostname' instead"  #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsrrsLayerId :: Lens.Lens' GetHostnameSuggestionResponse (Core.Maybe Core.Text)
ghsrrsLayerId = Lens.field @"layerId"
{-# INLINEABLE ghsrrsLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsrrsResponseStatus :: Lens.Lens' GetHostnameSuggestionResponse Core.Int
ghsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ghsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
