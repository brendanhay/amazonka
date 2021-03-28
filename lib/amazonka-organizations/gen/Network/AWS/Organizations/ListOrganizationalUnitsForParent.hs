{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListOrganizationalUnitsForParent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organizational units (OUs) in a parent organizational unit or root.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListOrganizationalUnitsForParent
    (
    -- * Creating a request
      ListOrganizationalUnitsForParent (..)
    , mkListOrganizationalUnitsForParent
    -- ** Request lenses
    , loufpParentId
    , loufpMaxResults
    , loufpNextToken

    -- * Destructuring the response
    , ListOrganizationalUnitsForParentResponse (..)
    , mkListOrganizationalUnitsForParentResponse
    -- ** Response lenses
    , loufprrsNextToken
    , loufprrsOrganizationalUnits
    , loufprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOrganizationalUnitsForParent' smart constructor.
data ListOrganizationalUnitsForParent = ListOrganizationalUnitsForParent'
  { parentId :: Types.ParentId
    -- ^ The unique identifier (ID) of the root or OU whose child OUs you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOrganizationalUnitsForParent' value with any optional fields omitted.
mkListOrganizationalUnitsForParent
    :: Types.ParentId -- ^ 'parentId'
    -> ListOrganizationalUnitsForParent
mkListOrganizationalUnitsForParent parentId
  = ListOrganizationalUnitsForParent'{parentId,
                                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The unique identifier (ID) of the root or OU whose child OUs you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufpParentId :: Lens.Lens' ListOrganizationalUnitsForParent Types.ParentId
loufpParentId = Lens.field @"parentId"
{-# INLINEABLE loufpParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufpMaxResults :: Lens.Lens' ListOrganizationalUnitsForParent (Core.Maybe Core.Natural)
loufpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE loufpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufpNextToken :: Lens.Lens' ListOrganizationalUnitsForParent (Core.Maybe Types.NextToken)
loufpNextToken = Lens.field @"nextToken"
{-# INLINEABLE loufpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListOrganizationalUnitsForParent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListOrganizationalUnitsForParent where
        toHeaders ListOrganizationalUnitsForParent{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.ListOrganizationalUnitsForParent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListOrganizationalUnitsForParent where
        toJSON ListOrganizationalUnitsForParent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParentId" Core..= parentId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListOrganizationalUnitsForParent where
        type Rs ListOrganizationalUnitsForParent =
             ListOrganizationalUnitsForParentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOrganizationalUnitsForParentResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "OrganizationalUnits"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOrganizationalUnitsForParent where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"organizationalUnits" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListOrganizationalUnitsForParentResponse' smart constructor.
data ListOrganizationalUnitsForParentResponse = ListOrganizationalUnitsForParentResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , organizationalUnits :: Core.Maybe [Types.OrganizationalUnit]
    -- ^ A list of the OUs in the specified root or parent OU.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOrganizationalUnitsForParentResponse' value with any optional fields omitted.
mkListOrganizationalUnitsForParentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOrganizationalUnitsForParentResponse
mkListOrganizationalUnitsForParentResponse responseStatus
  = ListOrganizationalUnitsForParentResponse'{nextToken =
                                                Core.Nothing,
                                              organizationalUnits = Core.Nothing, responseStatus}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufprrsNextToken :: Lens.Lens' ListOrganizationalUnitsForParentResponse (Core.Maybe Types.NextToken)
loufprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE loufprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of the OUs in the specified root or parent OU.
--
-- /Note:/ Consider using 'organizationalUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufprrsOrganizationalUnits :: Lens.Lens' ListOrganizationalUnitsForParentResponse (Core.Maybe [Types.OrganizationalUnit])
loufprrsOrganizationalUnits = Lens.field @"organizationalUnits"
{-# INLINEABLE loufprrsOrganizationalUnits #-}
{-# DEPRECATED organizationalUnits "Use generic-lens or generic-optics with 'organizationalUnits' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufprrsResponseStatus :: Lens.Lens' ListOrganizationalUnitsForParentResponse Core.Int
loufprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE loufprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
