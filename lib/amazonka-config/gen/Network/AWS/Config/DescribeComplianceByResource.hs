{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeComplianceByResource (..),
    mkDescribeComplianceByResource,

    -- ** Request lenses
    dcbrResourceId,
    dcbrResourceType,
    dcbrComplianceTypes,
    dcbrNextToken,
    dcbrLimit,

    -- * Destructuring the response
    DescribeComplianceByResourceResponse (..),
    mkDescribeComplianceByResourceResponse,

    -- ** Response lenses
    dcbrrsComplianceByResources,
    dcbrrsNextToken,
    dcbrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeComplianceByResource' smart constructor.
data DescribeComplianceByResource = DescribeComplianceByResource'
  { -- | The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
    resourceId :: Lude.Maybe Lude.Text,
    -- | The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
    resourceType :: Lude.Maybe Lude.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
    complianceTypes :: Lude.Maybe [ComplianceType],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeComplianceByResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
-- * 'resourceType' - The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
-- * 'complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
mkDescribeComplianceByResource ::
  DescribeComplianceByResource
mkDescribeComplianceByResource =
  DescribeComplianceByResource'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      complianceTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrResourceId :: Lens.Lens' DescribeComplianceByResource (Lude.Maybe Lude.Text)
dcbrResourceId = Lens.lens (resourceId :: DescribeComplianceByResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DescribeComplianceByResource)
{-# DEPRECATED dcbrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrResourceType :: Lens.Lens' DescribeComplianceByResource (Lude.Maybe Lude.Text)
dcbrResourceType = Lens.lens (resourceType :: DescribeComplianceByResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: DescribeComplianceByResource)
{-# DEPRECATED dcbrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrComplianceTypes :: Lens.Lens' DescribeComplianceByResource (Lude.Maybe [ComplianceType])
dcbrComplianceTypes = Lens.lens (complianceTypes :: DescribeComplianceByResource -> Lude.Maybe [ComplianceType]) (\s a -> s {complianceTypes = a} :: DescribeComplianceByResource)
{-# DEPRECATED dcbrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrNextToken :: Lens.Lens' DescribeComplianceByResource (Lude.Maybe Lude.Text)
dcbrNextToken = Lens.lens (nextToken :: DescribeComplianceByResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeComplianceByResource)
{-# DEPRECATED dcbrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrLimit :: Lens.Lens' DescribeComplianceByResource (Lude.Maybe Lude.Natural)
dcbrLimit = Lens.lens (limit :: DescribeComplianceByResource -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeComplianceByResource)
{-# DEPRECATED dcbrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeComplianceByResource where
  page rq rs
    | Page.stop (rs Lens.^. dcbrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcbrrsComplianceByResources) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcbrNextToken Lens..~ rs Lens.^. dcbrrsNextToken

instance Lude.AWSRequest DescribeComplianceByResource where
  type
    Rs DescribeComplianceByResource =
      DescribeComplianceByResourceResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeComplianceByResourceResponse'
            Lude.<$> (x Lude..?> "ComplianceByResources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeComplianceByResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeComplianceByResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeComplianceByResource where
  toJSON DescribeComplianceByResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceId" Lude..=) Lude.<$> resourceId,
            ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("ComplianceTypes" Lude..=) Lude.<$> complianceTypes,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeComplianceByResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeComplianceByResource where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeComplianceByResourceResponse' smart constructor.
data DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse'
  { -- | Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
    complianceByResources :: Lude.Maybe [ComplianceByResource],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeComplianceByResourceResponse' with the minimum fields required to make a request.
--
-- * 'complianceByResources' - Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeComplianceByResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeComplianceByResourceResponse
mkDescribeComplianceByResourceResponse pResponseStatus_ =
  DescribeComplianceByResourceResponse'
    { complianceByResources =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
--
-- /Note:/ Consider using 'complianceByResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrsComplianceByResources :: Lens.Lens' DescribeComplianceByResourceResponse (Lude.Maybe [ComplianceByResource])
dcbrrsComplianceByResources = Lens.lens (complianceByResources :: DescribeComplianceByResourceResponse -> Lude.Maybe [ComplianceByResource]) (\s a -> s {complianceByResources = a} :: DescribeComplianceByResourceResponse)
{-# DEPRECATED dcbrrsComplianceByResources "Use generic-lens or generic-optics with 'complianceByResources' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrsNextToken :: Lens.Lens' DescribeComplianceByResourceResponse (Lude.Maybe Lude.Text)
dcbrrsNextToken = Lens.lens (nextToken :: DescribeComplianceByResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeComplianceByResourceResponse)
{-# DEPRECATED dcbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrrsResponseStatus :: Lens.Lens' DescribeComplianceByResourceResponse Lude.Int
dcbrrsResponseStatus = Lens.lens (responseStatus :: DescribeComplianceByResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeComplianceByResourceResponse)
{-# DEPRECATED dcbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
