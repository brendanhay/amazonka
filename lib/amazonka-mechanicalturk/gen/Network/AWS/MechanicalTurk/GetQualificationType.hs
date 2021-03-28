{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationType@ operation retrieves information about a Qualification type using its ID. 
module Network.AWS.MechanicalTurk.GetQualificationType
    (
    -- * Creating a request
      GetQualificationType (..)
    , mkGetQualificationType
    -- ** Request lenses
    , gqtQualificationTypeId

    -- * Destructuring the response
    , GetQualificationTypeResponse (..)
    , mkGetQualificationTypeResponse
    -- ** Response lenses
    , gqtrrsQualificationType
    , gqtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetQualificationType' smart constructor.
newtype GetQualificationType = GetQualificationType'
  { qualificationTypeId :: Types.QualificationTypeId
    -- ^ The ID of the QualificationType.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetQualificationType' value with any optional fields omitted.
mkGetQualificationType
    :: Types.QualificationTypeId -- ^ 'qualificationTypeId'
    -> GetQualificationType
mkGetQualificationType qualificationTypeId
  = GetQualificationType'{qualificationTypeId}

-- | The ID of the QualificationType.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqtQualificationTypeId :: Lens.Lens' GetQualificationType Types.QualificationTypeId
gqtQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE gqtQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

instance Core.ToQuery GetQualificationType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetQualificationType where
        toHeaders GetQualificationType{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.GetQualificationType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetQualificationType where
        toJSON GetQualificationType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("QualificationTypeId" Core..= qualificationTypeId)])

instance Core.AWSRequest GetQualificationType where
        type Rs GetQualificationType = GetQualificationTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetQualificationTypeResponse' Core.<$>
                   (x Core..:? "QualificationType") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetQualificationTypeResponse' smart constructor.
data GetQualificationTypeResponse = GetQualificationTypeResponse'
  { qualificationType :: Core.Maybe Types.QualificationType
    -- ^ The returned Qualification Type
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetQualificationTypeResponse' value with any optional fields omitted.
mkGetQualificationTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetQualificationTypeResponse
mkGetQualificationTypeResponse responseStatus
  = GetQualificationTypeResponse'{qualificationType = Core.Nothing,
                                  responseStatus}

-- | The returned Qualification Type
--
-- /Note:/ Consider using 'qualificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqtrrsQualificationType :: Lens.Lens' GetQualificationTypeResponse (Core.Maybe Types.QualificationType)
gqtrrsQualificationType = Lens.field @"qualificationType"
{-# INLINEABLE gqtrrsQualificationType #-}
{-# DEPRECATED qualificationType "Use generic-lens or generic-optics with 'qualificationType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqtrrsResponseStatus :: Lens.Lens' GetQualificationTypeResponse Core.Int
gqtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gqtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
