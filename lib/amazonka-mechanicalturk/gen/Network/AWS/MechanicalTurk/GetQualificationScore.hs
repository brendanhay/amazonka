{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetQualificationScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationScore@ operation returns the value of a Worker's Qualification for a given Qualification type.
--
-- To get a Worker's Qualification, you must know the Worker's ID. The Worker's ID is included in the assignment data returned by the @ListAssignmentsForHIT@ operation.
-- Only the owner of a Qualification type can query the value of a Worker's Qualification of that type.
module Network.AWS.MechanicalTurk.GetQualificationScore
  ( -- * Creating a request
    GetQualificationScore (..),
    mkGetQualificationScore,

    -- ** Request lenses
    gqsQualificationTypeId,
    gqsWorkerId,

    -- * Destructuring the response
    GetQualificationScoreResponse (..),
    mkGetQualificationScoreResponse,

    -- ** Response lenses
    gqsrrsQualification,
    gqsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetQualificationScore' smart constructor.
data GetQualificationScore = GetQualificationScore'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Types.QualificationTypeId,
    -- | The ID of the Worker whose Qualification is being updated.
    workerId :: Types.WorkerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQualificationScore' value with any optional fields omitted.
mkGetQualificationScore ::
  -- | 'qualificationTypeId'
  Types.QualificationTypeId ->
  -- | 'workerId'
  Types.WorkerId ->
  GetQualificationScore
mkGetQualificationScore qualificationTypeId workerId =
  GetQualificationScore' {qualificationTypeId, workerId}

-- | The ID of the QualificationType.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsQualificationTypeId :: Lens.Lens' GetQualificationScore Types.QualificationTypeId
gqsQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED gqsQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The ID of the Worker whose Qualification is being updated.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsWorkerId :: Lens.Lens' GetQualificationScore Types.WorkerId
gqsWorkerId = Lens.field @"workerId"
{-# DEPRECATED gqsWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Core.FromJSON GetQualificationScore where
  toJSON GetQualificationScore {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
            Core.Just ("WorkerId" Core..= workerId)
          ]
      )

instance Core.AWSRequest GetQualificationScore where
  type Rs GetQualificationScore = GetQualificationScoreResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.GetQualificationScore"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQualificationScoreResponse'
            Core.<$> (x Core..:? "Qualification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetQualificationScoreResponse' smart constructor.
data GetQualificationScoreResponse = GetQualificationScoreResponse'
  { -- | The Qualification data structure of the Qualification assigned to a user, including the Qualification type and the value (score).
    qualification :: Core.Maybe Types.Qualification,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetQualificationScoreResponse' value with any optional fields omitted.
mkGetQualificationScoreResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetQualificationScoreResponse
mkGetQualificationScoreResponse responseStatus =
  GetQualificationScoreResponse'
    { qualification = Core.Nothing,
      responseStatus
    }

-- | The Qualification data structure of the Qualification assigned to a user, including the Qualification type and the value (score).
--
-- /Note:/ Consider using 'qualification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsrrsQualification :: Lens.Lens' GetQualificationScoreResponse (Core.Maybe Types.Qualification)
gqsrrsQualification = Lens.field @"qualification"
{-# DEPRECATED gqsrrsQualification "Use generic-lens or generic-optics with 'qualification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsrrsResponseStatus :: Lens.Lens' GetQualificationScoreResponse Core.Int
gqsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gqsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
