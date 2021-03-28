{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specific conference provider.
module Network.AWS.AlexaBusiness.GetConferenceProvider
    (
    -- * Creating a request
      GetConferenceProvider (..)
    , mkGetConferenceProvider
    -- ** Request lenses
    , gcpConferenceProviderArn

    -- * Destructuring the response
    , GetConferenceProviderResponse (..)
    , mkGetConferenceProviderResponse
    -- ** Response lenses
    , grsConferenceProvider
    , grsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConferenceProvider' smart constructor.
newtype GetConferenceProvider = GetConferenceProvider'
  { conferenceProviderArn :: Types.Arn
    -- ^ The ARN of the newly created conference provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetConferenceProvider' value with any optional fields omitted.
mkGetConferenceProvider
    :: Types.Arn -- ^ 'conferenceProviderArn'
    -> GetConferenceProvider
mkGetConferenceProvider conferenceProviderArn
  = GetConferenceProvider'{conferenceProviderArn}

-- | The ARN of the newly created conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpConferenceProviderArn :: Lens.Lens' GetConferenceProvider Types.Arn
gcpConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# INLINEABLE gcpConferenceProviderArn #-}
{-# DEPRECATED conferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead"  #-}

instance Core.ToQuery GetConferenceProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetConferenceProvider where
        toHeaders GetConferenceProvider{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.GetConferenceProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetConferenceProvider where
        toJSON GetConferenceProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConferenceProviderArn" Core..= conferenceProviderArn)])

instance Core.AWSRequest GetConferenceProvider where
        type Rs GetConferenceProvider = GetConferenceProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetConferenceProviderResponse' Core.<$>
                   (x Core..:? "ConferenceProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConferenceProviderResponse' smart constructor.
data GetConferenceProviderResponse = GetConferenceProviderResponse'
  { conferenceProvider :: Core.Maybe Types.ConferenceProvider
    -- ^ The conference provider.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConferenceProviderResponse' value with any optional fields omitted.
mkGetConferenceProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConferenceProviderResponse
mkGetConferenceProviderResponse responseStatus
  = GetConferenceProviderResponse'{conferenceProvider = Core.Nothing,
                                   responseStatus}

-- | The conference provider.
--
-- /Note:/ Consider using 'conferenceProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsConferenceProvider :: Lens.Lens' GetConferenceProviderResponse (Core.Maybe Types.ConferenceProvider)
grsConferenceProvider = Lens.field @"conferenceProvider"
{-# INLINEABLE grsConferenceProvider #-}
{-# DEPRECATED conferenceProvider "Use generic-lens or generic-optics with 'conferenceProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetConferenceProviderResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
