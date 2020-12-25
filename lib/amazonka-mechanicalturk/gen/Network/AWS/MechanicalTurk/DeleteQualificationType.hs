{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteQualificationType (..),
    mkDeleteQualificationType,

    -- ** Request lenses
    dqtQualificationTypeId,

    -- * Destructuring the response
    DeleteQualificationTypeResponse (..),
    mkDeleteQualificationTypeResponse,

    -- ** Response lenses
    dqtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteQualificationType' smart constructor.
newtype DeleteQualificationType = DeleteQualificationType'
  { -- | The ID of the QualificationType to dispose.
    qualificationTypeId :: Types.EntityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQualificationType' value with any optional fields omitted.
mkDeleteQualificationType ::
  -- | 'qualificationTypeId'
  Types.EntityId ->
  DeleteQualificationType
mkDeleteQualificationType qualificationTypeId =
  DeleteQualificationType' {qualificationTypeId}

-- | The ID of the QualificationType to dispose.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqtQualificationTypeId :: Lens.Lens' DeleteQualificationType Types.EntityId
dqtQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED dqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

instance Core.FromJSON DeleteQualificationType where
  toJSON DeleteQualificationType {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("QualificationTypeId" Core..= qualificationTypeId)]
      )

instance Core.AWSRequest DeleteQualificationType where
  type Rs DeleteQualificationType = DeleteQualificationTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.DeleteQualificationType"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQualificationTypeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteQualificationTypeResponse' smart constructor.
newtype DeleteQualificationTypeResponse = DeleteQualificationTypeResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQualificationTypeResponse' value with any optional fields omitted.
mkDeleteQualificationTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteQualificationTypeResponse
mkDeleteQualificationTypeResponse responseStatus =
  DeleteQualificationTypeResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqtrrsResponseStatus :: Lens.Lens' DeleteQualificationTypeResponse Core.Int
dqtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dqtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
