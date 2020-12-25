{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersWithQualificationType@ operation returns all of the Workers that have been associated with a given Qualification type.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
  ( -- * Creating a request
    ListWorkersWithQualificationType (..),
    mkListWorkersWithQualificationType,

    -- ** Request lenses
    lwwqtQualificationTypeId,
    lwwqtMaxResults,
    lwwqtNextToken,
    lwwqtStatus,

    -- * Destructuring the response
    ListWorkersWithQualificationTypeResponse (..),
    mkListWorkersWithQualificationTypeResponse,

    -- ** Response lenses
    lwwqtrrsNextToken,
    lwwqtrrsNumResults,
    lwwqtrrsQualifications,
    lwwqtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListWorkersWithQualificationType' smart constructor.
data ListWorkersWithQualificationType = ListWorkersWithQualificationType'
  { -- | The ID of the Qualification type of the Qualifications to return.
    qualificationTypeId :: Types.QualificationTypeId,
    -- | Limit the number of results returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | Pagination Token
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The status of the Qualifications to return. Can be @Granted | Revoked@ .
    status :: Core.Maybe Types.QualificationStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkersWithQualificationType' value with any optional fields omitted.
mkListWorkersWithQualificationType ::
  -- | 'qualificationTypeId'
  Types.QualificationTypeId ->
  ListWorkersWithQualificationType
mkListWorkersWithQualificationType qualificationTypeId =
  ListWorkersWithQualificationType'
    { qualificationTypeId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The ID of the Qualification type of the Qualifications to return.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtQualificationTypeId :: Lens.Lens' ListWorkersWithQualificationType Types.QualificationTypeId
lwwqtQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED lwwqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtMaxResults :: Lens.Lens' ListWorkersWithQualificationType (Core.Maybe Core.Natural)
lwwqtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lwwqtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination Token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtNextToken :: Lens.Lens' ListWorkersWithQualificationType (Core.Maybe Types.PaginationToken)
lwwqtNextToken = Lens.field @"nextToken"
{-# DEPRECATED lwwqtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The status of the Qualifications to return. Can be @Granted | Revoked@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtStatus :: Lens.Lens' ListWorkersWithQualificationType (Core.Maybe Types.QualificationStatus)
lwwqtStatus = Lens.field @"status"
{-# DEPRECATED lwwqtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ListWorkersWithQualificationType where
  toJSON ListWorkersWithQualificationType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest ListWorkersWithQualificationType where
  type
    Rs ListWorkersWithQualificationType =
      ListWorkersWithQualificationTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.ListWorkersWithQualificationType"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkersWithQualificationTypeResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "NumResults")
            Core.<*> (x Core..:? "Qualifications")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListWorkersWithQualificationType where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"qualifications" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListWorkersWithQualificationTypeResponse' smart constructor.
data ListWorkersWithQualificationTypeResponse = ListWorkersWithQualificationTypeResponse'
  { nextToken :: Core.Maybe Types.PaginationToken,
    -- | The number of Qualifications on this page in the filtered results list, equivalent to the number of Qualifications being returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The list of Qualification elements returned by this call.
    qualifications :: Core.Maybe [Types.Qualification],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListWorkersWithQualificationTypeResponse' value with any optional fields omitted.
mkListWorkersWithQualificationTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListWorkersWithQualificationTypeResponse
mkListWorkersWithQualificationTypeResponse responseStatus =
  ListWorkersWithQualificationTypeResponse'
    { nextToken =
        Core.Nothing,
      numResults = Core.Nothing,
      qualifications = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrrsNextToken :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Core.Maybe Types.PaginationToken)
lwwqtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lwwqtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of Qualifications on this page in the filtered results list, equivalent to the number of Qualifications being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrrsNumResults :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Core.Maybe Core.Int)
lwwqtrrsNumResults = Lens.field @"numResults"
{-# DEPRECATED lwwqtrrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The list of Qualification elements returned by this call.
--
-- /Note:/ Consider using 'qualifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrrsQualifications :: Lens.Lens' ListWorkersWithQualificationTypeResponse (Core.Maybe [Types.Qualification])
lwwqtrrsQualifications = Lens.field @"qualifications"
{-# DEPRECATED lwwqtrrsQualifications "Use generic-lens or generic-optics with 'qualifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwwqtrrsResponseStatus :: Lens.Lens' ListWorkersWithQualificationTypeResponse Core.Int
lwwqtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lwwqtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
