{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing conference provider's settings.
module Network.AWS.AlexaBusiness.UpdateConferenceProvider
    (
    -- * Creating a request
      UpdateConferenceProvider (..)
    , mkUpdateConferenceProvider
    -- ** Request lenses
    , ucpConferenceProviderArn
    , ucpConferenceProviderType
    , ucpMeetingSetting
    , ucpIPDialIn
    , ucpPSTNDialIn

    -- * Destructuring the response
    , UpdateConferenceProviderResponse (..)
    , mkUpdateConferenceProviderResponse
    -- ** Response lenses
    , ucprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateConferenceProvider' smart constructor.
data UpdateConferenceProvider = UpdateConferenceProvider'
  { conferenceProviderArn :: Types.ConferenceProviderArn
    -- ^ The ARN of the conference provider.
  , conferenceProviderType :: Types.ConferenceProviderType
    -- ^ The type of the conference provider.
  , meetingSetting :: Types.MeetingSetting
    -- ^ The meeting settings for the conference provider.
  , iPDialIn :: Core.Maybe Types.IPDialIn
    -- ^ The IP endpoint and protocol for calling.
  , pSTNDialIn :: Core.Maybe Types.PSTNDialIn
    -- ^ The information for PSTN conferencing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConferenceProvider' value with any optional fields omitted.
mkUpdateConferenceProvider
    :: Types.ConferenceProviderArn -- ^ 'conferenceProviderArn'
    -> Types.ConferenceProviderType -- ^ 'conferenceProviderType'
    -> Types.MeetingSetting -- ^ 'meetingSetting'
    -> UpdateConferenceProvider
mkUpdateConferenceProvider conferenceProviderArn
  conferenceProviderType meetingSetting
  = UpdateConferenceProvider'{conferenceProviderArn,
                              conferenceProviderType, meetingSetting, iPDialIn = Core.Nothing,
                              pSTNDialIn = Core.Nothing}

-- | The ARN of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpConferenceProviderArn :: Lens.Lens' UpdateConferenceProvider Types.ConferenceProviderArn
ucpConferenceProviderArn = Lens.field @"conferenceProviderArn"
{-# INLINEABLE ucpConferenceProviderArn #-}
{-# DEPRECATED conferenceProviderArn "Use generic-lens or generic-optics with 'conferenceProviderArn' instead"  #-}

-- | The type of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpConferenceProviderType :: Lens.Lens' UpdateConferenceProvider Types.ConferenceProviderType
ucpConferenceProviderType = Lens.field @"conferenceProviderType"
{-# INLINEABLE ucpConferenceProviderType #-}
{-# DEPRECATED conferenceProviderType "Use generic-lens or generic-optics with 'conferenceProviderType' instead"  #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpMeetingSetting :: Lens.Lens' UpdateConferenceProvider Types.MeetingSetting
ucpMeetingSetting = Lens.field @"meetingSetting"
{-# INLINEABLE ucpMeetingSetting #-}
{-# DEPRECATED meetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead"  #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'iPDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpIPDialIn :: Lens.Lens' UpdateConferenceProvider (Core.Maybe Types.IPDialIn)
ucpIPDialIn = Lens.field @"iPDialIn"
{-# INLINEABLE ucpIPDialIn #-}
{-# DEPRECATED iPDialIn "Use generic-lens or generic-optics with 'iPDialIn' instead"  #-}

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpPSTNDialIn :: Lens.Lens' UpdateConferenceProvider (Core.Maybe Types.PSTNDialIn)
ucpPSTNDialIn = Lens.field @"pSTNDialIn"
{-# INLINEABLE ucpPSTNDialIn #-}
{-# DEPRECATED pSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead"  #-}

instance Core.ToQuery UpdateConferenceProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateConferenceProvider where
        toHeaders UpdateConferenceProvider{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.UpdateConferenceProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateConferenceProvider where
        toJSON UpdateConferenceProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConferenceProviderArn" Core..= conferenceProviderArn),
                  Core.Just
                    ("ConferenceProviderType" Core..= conferenceProviderType),
                  Core.Just ("MeetingSetting" Core..= meetingSetting),
                  ("IPDialIn" Core..=) Core.<$> iPDialIn,
                  ("PSTNDialIn" Core..=) Core.<$> pSTNDialIn])

instance Core.AWSRequest UpdateConferenceProvider where
        type Rs UpdateConferenceProvider = UpdateConferenceProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateConferenceProviderResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateConferenceProviderResponse' smart constructor.
newtype UpdateConferenceProviderResponse = UpdateConferenceProviderResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConferenceProviderResponse' value with any optional fields omitted.
mkUpdateConferenceProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateConferenceProviderResponse
mkUpdateConferenceProviderResponse responseStatus
  = UpdateConferenceProviderResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsResponseStatus :: Lens.Lens' UpdateConferenceProviderResponse Core.Int
ucprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
