{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListChangeSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the ID and status of each active change set for a stack. For example, AWS CloudFormation lists change sets that are in the @CREATE_IN_PROGRESS@ or @CREATE_PENDING@ state.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListChangeSets
    (
    -- * Creating a request
      ListChangeSets (..)
    , mkListChangeSets
    -- ** Request lenses
    , lcsStackName
    , lcsNextToken

    -- * Destructuring the response
    , ListChangeSetsResponse (..)
    , mkListChangeSetsResponse
    -- ** Response lenses
    , lcsrrsNextToken
    , lcsrrsSummaries
    , lcsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'ListChangeSets' action.
--
-- /See:/ 'mkListChangeSets' smart constructor.
data ListChangeSets = ListChangeSets'
  { stackName :: Types.StackName
    -- ^ The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListChangeSets' value with any optional fields omitted.
mkListChangeSets
    :: Types.StackName -- ^ 'stackName'
    -> ListChangeSets
mkListChangeSets stackName
  = ListChangeSets'{stackName, nextToken = Core.Nothing}

-- | The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsStackName :: Lens.Lens' ListChangeSets Types.StackName
lcsStackName = Lens.field @"stackName"
{-# INLINEABLE lcsStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListChangeSets (Core.Maybe Types.NextToken)
lcsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListChangeSets where
        toQuery ListChangeSets{..}
          = Core.toQueryPair "Action" ("ListChangeSets" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackName" stackName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListChangeSets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListChangeSets where
        type Rs ListChangeSets = ListChangeSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListChangeSetsResult"
              (\ s h x ->
                 ListChangeSetsResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "Summaries" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListChangeSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"summaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The output for the 'ListChangeSets' action.
--
-- /See:/ 'mkListChangeSetsResponse' smart constructor.
data ListChangeSetsResponse = ListChangeSetsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
  , summaries :: Core.Maybe [Types.ChangeSetSummary]
    -- ^ A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListChangeSetsResponse' value with any optional fields omitted.
mkListChangeSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListChangeSetsResponse
mkListChangeSetsResponse responseStatus
  = ListChangeSetsResponse'{nextToken = Core.Nothing,
                            summaries = Core.Nothing, responseStatus}

-- | If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsNextToken :: Lens.Lens' ListChangeSetsResponse (Core.Maybe Types.NextToken)
lcsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsSummaries :: Lens.Lens' ListChangeSetsResponse (Core.Maybe [Types.ChangeSetSummary])
lcsrrsSummaries = Lens.field @"summaries"
{-# INLINEABLE lcsrrsSummaries #-}
{-# DEPRECATED summaries "Use generic-lens or generic-optics with 'summaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsResponseStatus :: Lens.Lens' ListChangeSetsResponse Core.Int
lcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
