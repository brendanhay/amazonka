{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description for the specified stack; if no stack name was specified, then it returns the description for all the stacks created.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStacks
    (
    -- * Creating a request
      DescribeStacks (..)
    , mkDescribeStacks
    -- ** Request lenses
    , dNextToken
    , dStackName

    -- * Destructuring the response
    , DescribeStacksResponse (..)
    , mkDescribeStacksResponse
    -- ** Response lenses
    , dsrrsNextToken
    , dsrrsStacks
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for 'DescribeStacks' action.
--
-- /See:/ 'mkDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A string that identifies the next page of stacks that you want to retrieve.
  , stackName :: Core.Maybe Types.StackName
    -- ^ The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStacks' value with any optional fields omitted.
mkDescribeStacks
    :: DescribeStacks
mkDescribeStacks
  = DescribeStacks'{nextToken = Core.Nothing,
                    stackName = Core.Nothing}

-- | A string that identifies the next page of stacks that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeStacks (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackName :: Lens.Lens' DescribeStacks (Core.Maybe Types.StackName)
dStackName = Lens.field @"stackName"
{-# INLINEABLE dStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

instance Core.ToQuery DescribeStacks where
        toQuery DescribeStacks{..}
          = Core.toQueryPair "Action" ("DescribeStacks" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackName") stackName

instance Core.ToHeaders DescribeStacks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeStacks where
        type Rs DescribeStacks = DescribeStacksResponse
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
          = Response.receiveXMLWrapper "DescribeStacksResult"
              (\ s h x ->
                 DescribeStacksResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "Stacks" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeStacks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"stacks" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The output for a 'DescribeStacks' action.
--
-- /See:/ 'mkDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
  , stacks :: Core.Maybe [Types.Stack]
    -- ^ A list of stack structures.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStacksResponse' value with any optional fields omitted.
mkDescribeStacksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStacksResponse
mkDescribeStacksResponse responseStatus
  = DescribeStacksResponse'{nextToken = Core.Nothing,
                            stacks = Core.Nothing, responseStatus}

-- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsNextToken :: Lens.Lens' DescribeStacksResponse (Core.Maybe Types.NextToken)
dsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of stack structures.
--
-- /Note:/ Consider using 'stacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStacks :: Lens.Lens' DescribeStacksResponse (Core.Maybe [Types.Stack])
dsrrsStacks = Lens.field @"stacks"
{-# INLINEABLE dsrrsStacks #-}
{-# DEPRECATED stacks "Use generic-lens or generic-optics with 'stacks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStacksResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
