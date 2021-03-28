{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed view of a Remediation Execution for a set of resources including state, timestamps for when steps for the remediation execution occur, and any error messages for steps that have failed. When you specify the limit and the next token, you receive a paginated response.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRemediationExecutionStatus
    (
    -- * Creating a request
      DescribeRemediationExecutionStatus (..)
    , mkDescribeRemediationExecutionStatus
    -- ** Request lenses
    , dresConfigRuleName
    , dresLimit
    , dresNextToken
    , dresResourceKeys

    -- * Destructuring the response
    , DescribeRemediationExecutionStatusResponse (..)
    , mkDescribeRemediationExecutionStatusResponse
    -- ** Response lenses
    , dresrrsNextToken
    , dresrrsRemediationExecutionStatuses
    , dresrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { configRuleName :: Types.ConfigRuleName
    -- ^ A list of AWS Config rule names.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , resourceKeys :: Core.Maybe (Core.NonEmpty Types.ResourceKey)
    -- ^ A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRemediationExecutionStatus' value with any optional fields omitted.
mkDescribeRemediationExecutionStatus
    :: Types.ConfigRuleName -- ^ 'configRuleName'
    -> DescribeRemediationExecutionStatus
mkDescribeRemediationExecutionStatus configRuleName
  = DescribeRemediationExecutionStatus'{configRuleName,
                                        limit = Core.Nothing, nextToken = Core.Nothing,
                                        resourceKeys = Core.Nothing}

-- | A list of AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresConfigRuleName :: Lens.Lens' DescribeRemediationExecutionStatus Types.ConfigRuleName
dresConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE dresConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

-- | The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresLimit :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe Core.Natural)
dresLimit = Lens.field @"limit"
{-# INLINEABLE dresLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresNextToken :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe Core.Text)
dresNextToken = Lens.field @"nextToken"
{-# INLINEABLE dresNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID. 
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresResourceKeys :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe (Core.NonEmpty Types.ResourceKey))
dresResourceKeys = Lens.field @"resourceKeys"
{-# INLINEABLE dresResourceKeys #-}
{-# DEPRECATED resourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead"  #-}

instance Core.ToQuery DescribeRemediationExecutionStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRemediationExecutionStatus where
        toHeaders DescribeRemediationExecutionStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeRemediationExecutionStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRemediationExecutionStatus where
        toJSON DescribeRemediationExecutionStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigRuleName" Core..= configRuleName),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceKeys" Core..=) Core.<$> resourceKeys])

instance Core.AWSRequest DescribeRemediationExecutionStatus where
        type Rs DescribeRemediationExecutionStatus =
             DescribeRemediationExecutionStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRemediationExecutionStatusResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "RemediationExecutionStatuses"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeRemediationExecutionStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"remediationExecutionStatuses" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , remediationExecutionStatuses :: Core.Maybe [Types.RemediationExecutionStatus]
    -- ^ Returns a list of remediation execution statuses objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRemediationExecutionStatusResponse' value with any optional fields omitted.
mkDescribeRemediationExecutionStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRemediationExecutionStatusResponse
mkDescribeRemediationExecutionStatusResponse responseStatus
  = DescribeRemediationExecutionStatusResponse'{nextToken =
                                                  Core.Nothing,
                                                remediationExecutionStatuses = Core.Nothing,
                                                responseStatus}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrrsNextToken :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Core.Maybe Core.Text)
dresrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dresrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns a list of remediation execution statuses objects.
--
-- /Note:/ Consider using 'remediationExecutionStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrrsRemediationExecutionStatuses :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Core.Maybe [Types.RemediationExecutionStatus])
dresrrsRemediationExecutionStatuses = Lens.field @"remediationExecutionStatuses"
{-# INLINEABLE dresrrsRemediationExecutionStatuses #-}
{-# DEPRECATED remediationExecutionStatuses "Use generic-lens or generic-optics with 'remediationExecutionStatuses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrrsResponseStatus :: Lens.Lens' DescribeRemediationExecutionStatusResponse Core.Int
dresrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dresrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
