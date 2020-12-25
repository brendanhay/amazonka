{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListQualificationTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationTypes@ operation returns a list of Qualification types, filtered by an optional search term.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationTypes
  ( -- * Creating a request
    ListQualificationTypes (..),
    mkListQualificationTypes,

    -- ** Request lenses
    lqtMustBeRequestable,
    lqtMaxResults,
    lqtMustBeOwnedByCaller,
    lqtNextToken,
    lqtQuery,

    -- * Destructuring the response
    ListQualificationTypesResponse (..),
    mkListQualificationTypesResponse,

    -- ** Response lenses
    lqtrrsNextToken,
    lqtrrsNumResults,
    lqtrrsQualificationTypes,
    lqtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListQualificationTypes' smart constructor.
data ListQualificationTypes = ListQualificationTypes'
  { -- | Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
    mustBeRequestable :: Core.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
    mustBeOwnedByCaller :: Core.Maybe Core.Bool,
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | A text query against all of the searchable attributes of Qualification types.
    query :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQualificationTypes' value with any optional fields omitted.
mkListQualificationTypes ::
  -- | 'mustBeRequestable'
  Core.Bool ->
  ListQualificationTypes
mkListQualificationTypes mustBeRequestable =
  ListQualificationTypes'
    { mustBeRequestable,
      maxResults = Core.Nothing,
      mustBeOwnedByCaller = Core.Nothing,
      nextToken = Core.Nothing,
      query = Core.Nothing
    }

-- | Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
--
-- /Note:/ Consider using 'mustBeRequestable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtMustBeRequestable :: Lens.Lens' ListQualificationTypes Core.Bool
lqtMustBeRequestable = Lens.field @"mustBeRequestable"
{-# DEPRECATED lqtMustBeRequestable "Use generic-lens or generic-optics with 'mustBeRequestable' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtMaxResults :: Lens.Lens' ListQualificationTypes (Core.Maybe Core.Natural)
lqtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lqtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
--
-- /Note:/ Consider using 'mustBeOwnedByCaller' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtMustBeOwnedByCaller :: Lens.Lens' ListQualificationTypes (Core.Maybe Core.Bool)
lqtMustBeOwnedByCaller = Lens.field @"mustBeOwnedByCaller"
{-# DEPRECATED lqtMustBeOwnedByCaller "Use generic-lens or generic-optics with 'mustBeOwnedByCaller' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtNextToken :: Lens.Lens' ListQualificationTypes (Core.Maybe Types.PaginationToken)
lqtNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A text query against all of the searchable attributes of Qualification types.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtQuery :: Lens.Lens' ListQualificationTypes (Core.Maybe Types.String)
lqtQuery = Lens.field @"query"
{-# DEPRECATED lqtQuery "Use generic-lens or generic-optics with 'query' instead." #-}

instance Core.FromJSON ListQualificationTypes where
  toJSON ListQualificationTypes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MustBeRequestable" Core..= mustBeRequestable),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("MustBeOwnedByCaller" Core..=) Core.<$> mustBeOwnedByCaller,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Query" Core..=) Core.<$> query
          ]
      )

instance Core.AWSRequest ListQualificationTypes where
  type Rs ListQualificationTypes = ListQualificationTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.ListQualificationTypes"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQualificationTypesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "NumResults")
            Core.<*> (x Core..:? "QualificationTypes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListQualificationTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"qualificationTypes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListQualificationTypesResponse' smart constructor.
data ListQualificationTypesResponse = ListQualificationTypesResponse'
  { nextToken :: Core.Maybe Types.PaginationToken,
    -- | The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
    numResults :: Core.Maybe Core.Int,
    -- | The list of QualificationType elements returned by the query.
    qualificationTypes :: Core.Maybe [Types.QualificationType],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListQualificationTypesResponse' value with any optional fields omitted.
mkListQualificationTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListQualificationTypesResponse
mkListQualificationTypesResponse responseStatus =
  ListQualificationTypesResponse'
    { nextToken = Core.Nothing,
      numResults = Core.Nothing,
      qualificationTypes = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsNextToken :: Lens.Lens' ListQualificationTypesResponse (Core.Maybe Types.PaginationToken)
lqtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsNumResults :: Lens.Lens' ListQualificationTypesResponse (Core.Maybe Core.Int)
lqtrrsNumResults = Lens.field @"numResults"
{-# DEPRECATED lqtrrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The list of QualificationType elements returned by the query.
--
-- /Note:/ Consider using 'qualificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsQualificationTypes :: Lens.Lens' ListQualificationTypesResponse (Core.Maybe [Types.QualificationType])
lqtrrsQualificationTypes = Lens.field @"qualificationTypes"
{-# DEPRECATED lqtrrsQualificationTypes "Use generic-lens or generic-optics with 'qualificationTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsResponseStatus :: Lens.Lens' ListQualificationTypesResponse Core.Int
lqtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lqtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
