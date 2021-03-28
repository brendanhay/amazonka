{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the conference preferences on a specific conference provider at the account level.
module Network.AWS.AlexaBusiness.PutConferencePreference
    (
    -- * Creating a request
      PutConferencePreference (..)
    , mkPutConferencePreference
    -- ** Request lenses
    , pcpConferencePreference

    -- * Destructuring the response
    , PutConferencePreferenceResponse (..)
    , mkPutConferencePreferenceResponse
    -- ** Response lenses
    , pcprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutConferencePreference' smart constructor.
newtype PutConferencePreference = PutConferencePreference'
  { conferencePreference :: Types.ConferencePreference
    -- ^ The conference preference of a specific conference provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConferencePreference' value with any optional fields omitted.
mkPutConferencePreference
    :: Types.ConferencePreference -- ^ 'conferencePreference'
    -> PutConferencePreference
mkPutConferencePreference conferencePreference
  = PutConferencePreference'{conferencePreference}

-- | The conference preference of a specific conference provider.
--
-- /Note:/ Consider using 'conferencePreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConferencePreference :: Lens.Lens' PutConferencePreference Types.ConferencePreference
pcpConferencePreference = Lens.field @"conferencePreference"
{-# INLINEABLE pcpConferencePreference #-}
{-# DEPRECATED conferencePreference "Use generic-lens or generic-optics with 'conferencePreference' instead"  #-}

instance Core.ToQuery PutConferencePreference where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutConferencePreference where
        toHeaders PutConferencePreference{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.PutConferencePreference")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutConferencePreference where
        toJSON PutConferencePreference{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConferencePreference" Core..= conferencePreference)])

instance Core.AWSRequest PutConferencePreference where
        type Rs PutConferencePreference = PutConferencePreferenceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutConferencePreferenceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutConferencePreferenceResponse' smart constructor.
newtype PutConferencePreferenceResponse = PutConferencePreferenceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConferencePreferenceResponse' value with any optional fields omitted.
mkPutConferencePreferenceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutConferencePreferenceResponse
mkPutConferencePreferenceResponse responseStatus
  = PutConferencePreferenceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsResponseStatus :: Lens.Lens' PutConferencePreferenceResponse Core.Int
pcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
