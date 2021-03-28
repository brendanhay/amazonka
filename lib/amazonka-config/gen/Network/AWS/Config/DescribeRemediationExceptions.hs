{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation exceptions. A detailed view of a remediation exception for a set of resources that includes an explanation of an exception and the time when the exception will be deleted. When you specify the limit and the next token, you receive a paginated response. 
module Network.AWS.Config.DescribeRemediationExceptions
    (
    -- * Creating a request
      DescribeRemediationExceptions (..)
    , mkDescribeRemediationExceptions
    -- ** Request lenses
    , dreConfigRuleName
    , dreLimit
    , dreNextToken
    , dreResourceKeys

    -- * Destructuring the response
    , DescribeRemediationExceptionsResponse (..)
    , mkDescribeRemediationExceptionsResponse
    -- ** Response lenses
    , drerrsNextToken
    , drerrsRemediationExceptions
    , drerrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRemediationExceptions' smart constructor.
data DescribeRemediationExceptions = DescribeRemediationExceptions'
  { configRuleName :: Types.ConfigRuleName
    -- ^ The name of the AWS Config rule.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of RemediationExceptionResourceKey returned on each page. The default is 25. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  , resourceKeys :: Core.Maybe (Core.NonEmpty Types.RemediationExceptionResourceKey)
    -- ^ An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRemediationExceptions' value with any optional fields omitted.
mkDescribeRemediationExceptions
    :: Types.ConfigRuleName -- ^ 'configRuleName'
    -> DescribeRemediationExceptions
mkDescribeRemediationExceptions configRuleName
  = DescribeRemediationExceptions'{configRuleName,
                                   limit = Core.Nothing, nextToken = Core.Nothing,
                                   resourceKeys = Core.Nothing}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreConfigRuleName :: Lens.Lens' DescribeRemediationExceptions Types.ConfigRuleName
dreConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE dreConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

-- | The maximum number of RemediationExceptionResourceKey returned on each page. The default is 25. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreLimit :: Lens.Lens' DescribeRemediationExceptions (Core.Maybe Core.Natural)
dreLimit = Lens.field @"limit"
{-# INLINEABLE dreLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreNextToken :: Lens.Lens' DescribeRemediationExceptions (Core.Maybe Core.Text)
dreNextToken = Lens.field @"nextToken"
{-# INLINEABLE dreNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys. 
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreResourceKeys :: Lens.Lens' DescribeRemediationExceptions (Core.Maybe (Core.NonEmpty Types.RemediationExceptionResourceKey))
dreResourceKeys = Lens.field @"resourceKeys"
{-# INLINEABLE dreResourceKeys #-}
{-# DEPRECATED resourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead"  #-}

instance Core.ToQuery DescribeRemediationExceptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRemediationExceptions where
        toHeaders DescribeRemediationExceptions{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeRemediationExceptions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRemediationExceptions where
        toJSON DescribeRemediationExceptions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigRuleName" Core..= configRuleName),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceKeys" Core..=) Core.<$> resourceKeys])

instance Core.AWSRequest DescribeRemediationExceptions where
        type Rs DescribeRemediationExceptions =
             DescribeRemediationExceptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRemediationExceptionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "RemediationExceptions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRemediationExceptionsResponse' smart constructor.
data DescribeRemediationExceptionsResponse = DescribeRemediationExceptionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  , remediationExceptions :: Core.Maybe [Types.RemediationException]
    -- ^ Returns a list of remediation exception objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRemediationExceptionsResponse' value with any optional fields omitted.
mkDescribeRemediationExceptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRemediationExceptionsResponse
mkDescribeRemediationExceptionsResponse responseStatus
  = DescribeRemediationExceptionsResponse'{nextToken = Core.Nothing,
                                           remediationExceptions = Core.Nothing, responseStatus}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerrsNextToken :: Lens.Lens' DescribeRemediationExceptionsResponse (Core.Maybe Core.Text)
drerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns a list of remediation exception objects.
--
-- /Note:/ Consider using 'remediationExceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerrsRemediationExceptions :: Lens.Lens' DescribeRemediationExceptionsResponse (Core.Maybe [Types.RemediationException])
drerrsRemediationExceptions = Lens.field @"remediationExceptions"
{-# INLINEABLE drerrsRemediationExceptions #-}
{-# DEPRECATED remediationExceptions "Use generic-lens or generic-optics with 'remediationExceptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerrsResponseStatus :: Lens.Lens' DescribeRemediationExceptionsResponse Core.Int
drerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
