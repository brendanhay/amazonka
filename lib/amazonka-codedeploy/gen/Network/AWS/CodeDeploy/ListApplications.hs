{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applications registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplications
    (
    -- * Creating a request
      ListApplications (..)
    , mkListApplications
    -- ** Request lenses
    , laNextToken

    -- * Destructuring the response
    , ListApplicationsResponse (..)
    , mkListApplicationsResponse
    -- ** Response lenses
    , larrsApplications
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListApplications@ operation.
--
-- /See:/ 'mkListApplications' smart constructor.
newtype ListApplications = ListApplications'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplications' value with any optional fields omitted.
mkListApplications
    :: ListApplications
mkListApplications = ListApplications'{nextToken = Core.Nothing}

-- | An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApplications (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListApplications where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListApplications where
        toHeaders ListApplications{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.ListApplications")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListApplications where
        toJSON ListApplications{..}
          = Core.object
              (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListApplications where
        type Rs ListApplications = ListApplicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListApplicationsResponse' Core.<$>
                   (x Core..:? "applications") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListApplications where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"applications" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the output of a ListApplications operation.
--
-- /See:/ 'mkListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { applications :: Core.Maybe [Types.ApplicationName]
    -- ^ A list of application names.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationsResponse' value with any optional fields omitted.
mkListApplicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListApplicationsResponse
mkListApplicationsResponse responseStatus
  = ListApplicationsResponse'{applications = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | A list of application names.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsApplications :: Lens.Lens' ListApplicationsResponse (Core.Maybe [Types.ApplicationName])
larrsApplications = Lens.field @"applications"
{-# INLINEABLE larrsApplications #-}
{-# DEPRECATED applications "Use generic-lens or generic-optics with 'applications' instead"  #-}

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListApplicationsResponse (Core.Maybe Types.NextToken)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListApplicationsResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
