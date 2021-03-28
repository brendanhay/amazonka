{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteQualificationType@ deletes a Qualification type and deletes any HIT types that are associated with the Qualification type. 
--
-- This operation does not revoke Qualifications already assigned to Workers because the Qualifications might be needed for active HITs. If there are any pending requests for the Qualification type, Amazon Mechanical Turk rejects those requests. After you delete a Qualification type, you can no longer use it to create HITs or HIT types.
module Network.AWS.MechanicalTurk.DeleteQualificationType
    (
    -- * Creating a request
      DeleteQualificationType (..)
    , mkDeleteQualificationType
    -- ** Request lenses
    , dqtQualificationTypeId

    -- * Destructuring the response
    , DeleteQualificationTypeResponse (..)
    , mkDeleteQualificationTypeResponse
    -- ** Response lenses
    , dqtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteQualificationType' smart constructor.
newtype DeleteQualificationType = DeleteQualificationType'
  { qualificationTypeId :: Types.EntityId
    -- ^ The ID of the QualificationType to dispose.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQualificationType' value with any optional fields omitted.
mkDeleteQualificationType
    :: Types.EntityId -- ^ 'qualificationTypeId'
    -> DeleteQualificationType
mkDeleteQualificationType qualificationTypeId
  = DeleteQualificationType'{qualificationTypeId}

-- | The ID of the QualificationType to dispose.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqtQualificationTypeId :: Lens.Lens' DeleteQualificationType Types.EntityId
dqtQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE dqtQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

instance Core.ToQuery DeleteQualificationType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteQualificationType where
        toHeaders DeleteQualificationType{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.DeleteQualificationType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteQualificationType where
        toJSON DeleteQualificationType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("QualificationTypeId" Core..= qualificationTypeId)])

instance Core.AWSRequest DeleteQualificationType where
        type Rs DeleteQualificationType = DeleteQualificationTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteQualificationTypeResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteQualificationTypeResponse' smart constructor.
newtype DeleteQualificationTypeResponse = DeleteQualificationTypeResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQualificationTypeResponse' value with any optional fields omitted.
mkDeleteQualificationTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteQualificationTypeResponse
mkDeleteQualificationTypeResponse responseStatus
  = DeleteQualificationTypeResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqtrrsResponseStatus :: Lens.Lens' DeleteQualificationTypeResponse Core.Int
dqtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
