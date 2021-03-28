{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeComplianceByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified AWS resources are compliant. If a resource is noncompliant, this action returns the number of AWS Config rules that the resource does not comply with.
--
-- A resource is compliant if it complies with all the AWS Config rules that evaluate it. It is noncompliant if it does not comply with one or more of these rules.
-- If AWS Config has no current evaluation results for the resource, it returns @INSUFFICIENT_DATA@ . This result might indicate one of the following conditions about the rules that evaluate the resource:
--
--     * AWS Config has never invoked an evaluation for the rule. To check whether it has, use the @DescribeConfigRuleEvaluationStatus@ action to get the @LastSuccessfulInvocationTime@ and @LastFailedInvocationTime@ .
--
--
--     * The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the @config:PutEvaluations@ permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the @config:PutEvaluations@ permission.
--
--
--     * The rule's AWS Lambda function has returned @NOT_APPLICABLE@ for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeComplianceByResource
    (
    -- * Creating a request
      DescribeComplianceByResource (..)
    , mkDescribeComplianceByResource
    -- ** Request lenses
    , dcbrComplianceTypes
    , dcbrLimit
    , dcbrNextToken
    , dcbrResourceId
    , dcbrResourceType

    -- * Destructuring the response
    , DescribeComplianceByResourceResponse (..)
    , mkDescribeComplianceByResourceResponse
    -- ** Response lenses
    , dcbrrrsComplianceByResources
    , dcbrrrsNextToken
    , dcbrrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeComplianceByResource' smart constructor.
data DescribeComplianceByResource = DescribeComplianceByResource'
  { complianceTypes :: Core.Maybe [Types.ComplianceType]
    -- ^ Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , resourceId :: Core.Maybe Types.BaseResourceId
    -- ^ The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
  , resourceType :: Core.Maybe Types.StringWithCharLimit256
    -- ^ The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComplianceByResource' value with any optional fields omitted.
mkDescribeComplianceByResource
    :: DescribeComplianceByResource
mkDescribeComplianceByResource
  = DescribeComplianceByResource'{complianceTypes = Core.Nothing,
                                  limit = Core.Nothing, nextToken = Core.Nothing,
                                  resourceId = Core.Nothing, resourceType = Core.Nothing}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrComplianceTypes :: Lens.Lens' DescribeComplianceByResource (Core.Maybe [Types.ComplianceType])
dcbrComplianceTypes = Lens.field @"complianceTypes"
{-# INLINEABLE dcbrComplianceTypes #-}
{-# DEPRECATED complianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead"  #-}

-- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrLimit :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Core.Natural)
dcbrLimit = Lens.field @"limit"
{-# INLINEABLE dcbrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrNextToken :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Types.NextToken)
dcbrNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcbrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrResourceId :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Types.BaseResourceId)
dcbrResourceId = Lens.field @"resourceId"
{-# INLINEABLE dcbrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrResourceType :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Types.StringWithCharLimit256)
dcbrResourceType = Lens.field @"resourceType"
{-# INLINEABLE dcbrResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery DescribeComplianceByResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeComplianceByResource where
        toHeaders DescribeComplianceByResource{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeComplianceByResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeComplianceByResource where
        toJSON DescribeComplianceByResource{..}
          = Core.object
              (Core.catMaybes
                 [("ComplianceTypes" Core..=) Core.<$> complianceTypes,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceId" Core..=) Core.<$> resourceId,
                  ("ResourceType" Core..=) Core.<$> resourceType])

instance Core.AWSRequest DescribeComplianceByResource where
        type Rs DescribeComplianceByResource =
             DescribeComplianceByResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeComplianceByResourceResponse' Core.<$>
                   (x Core..:? "ComplianceByResources") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeComplianceByResource where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"complianceByResources" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | 
--
-- /See:/ 'mkDescribeComplianceByResourceResponse' smart constructor.
data DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse'
  { complianceByResources :: Core.Maybe [Types.ComplianceByResource]
    -- ^ Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The string that you use in a subsequent request to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComplianceByResourceResponse' value with any optional fields omitted.
mkDescribeComplianceByResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeComplianceByResourceResponse
mkDescribeComplianceByResourceResponse responseStatus
  = DescribeComplianceByResourceResponse'{complianceByResources =
                                            Core.Nothing,
                                          nextToken = Core.Nothing, responseStatus}

-- | Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
--
-- /Note:/ Consider using 'complianceByResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrrsComplianceByResources :: Lens.Lens' DescribeComplianceByResourceResponse (Core.Maybe [Types.ComplianceByResource])
dcbrrrsComplianceByResources = Lens.field @"complianceByResources"
{-# INLINEABLE dcbrrrsComplianceByResources #-}
{-# DEPRECATED complianceByResources "Use generic-lens or generic-optics with 'complianceByResources' instead"  #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrrsNextToken :: Lens.Lens' DescribeComplianceByResourceResponse (Core.Maybe Types.NextToken)
dcbrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcbrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrrsResponseStatus :: Lens.Lens' DescribeComplianceByResourceResponse Core.Int
dcbrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcbrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
