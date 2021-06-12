{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeComplianceByResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified AWS resources are compliant. If a
-- resource is noncompliant, this action returns the number of AWS Config
-- rules that the resource does not comply with.
--
-- A resource is compliant if it complies with all the AWS Config rules
-- that evaluate it. It is noncompliant if it does not comply with one or
-- more of these rules.
--
-- If AWS Config has no current evaluation results for the resource, it
-- returns @INSUFFICIENT_DATA@. This result might indicate one of the
-- following conditions about the rules that evaluate the resource:
--
-- -   AWS Config has never invoked an evaluation for the rule. To check
--     whether it has, use the @DescribeConfigRuleEvaluationStatus@ action
--     to get the @LastSuccessfulInvocationTime@ and
--     @LastFailedInvocationTime@.
--
-- -   The rule\'s AWS Lambda function is failing to send evaluation
--     results to AWS Config. Verify that the role that you assigned to
--     your configuration recorder includes the @config:PutEvaluations@
--     permission. If the rule is a custom rule, verify that the AWS Lambda
--     execution role includes the @config:PutEvaluations@ permission.
--
-- -   The rule\'s AWS Lambda function has returned @NOT_APPLICABLE@ for
--     all evaluation results. This can occur if the resources were deleted
--     or removed from the rule\'s scope.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeComplianceByResource
  ( -- * Creating a Request
    DescribeComplianceByResource (..),
    newDescribeComplianceByResource,

    -- * Request Lenses
    describeComplianceByResource_resourceId,
    describeComplianceByResource_nextToken,
    describeComplianceByResource_complianceTypes,
    describeComplianceByResource_resourceType,
    describeComplianceByResource_limit,

    -- * Destructuring the Response
    DescribeComplianceByResourceResponse (..),
    newDescribeComplianceByResourceResponse,

    -- * Response Lenses
    describeComplianceByResourceResponse_nextToken,
    describeComplianceByResourceResponse_complianceByResources,
    describeComplianceByResourceResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeComplianceByResource' smart constructor.
data DescribeComplianceByResource = DescribeComplianceByResource'
  { -- | The ID of the AWS resource for which you want compliance information.
    -- You can specify only one resource ID. If you specify a resource ID, you
    -- must also specify a type for @ResourceType@.
    resourceId :: Core.Maybe Core.Text,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
    -- @INSUFFICIENT_DATA@.
    complianceTypes :: Core.Maybe [ComplianceType],
    -- | The types of AWS resources for which you want compliance information
    -- (for example, @AWS::EC2::Instance@). For this action, you can specify
    -- that the resource type is an AWS account by specifying @AWS::::Account@.
    resourceType :: Core.Maybe Core.Text,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is 10. You cannot specify a number greater than 100. If you
    -- specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeComplianceByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeComplianceByResource_resourceId' - The ID of the AWS resource for which you want compliance information.
-- You can specify only one resource ID. If you specify a resource ID, you
-- must also specify a type for @ResourceType@.
--
-- 'nextToken', 'describeComplianceByResource_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceTypes', 'describeComplianceByResource_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @INSUFFICIENT_DATA@.
--
-- 'resourceType', 'describeComplianceByResource_resourceType' - The types of AWS resources for which you want compliance information
-- (for example, @AWS::EC2::Instance@). For this action, you can specify
-- that the resource type is an AWS account by specifying @AWS::::Account@.
--
-- 'limit', 'describeComplianceByResource_limit' - The maximum number of evaluation results returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
newDescribeComplianceByResource ::
  DescribeComplianceByResource
newDescribeComplianceByResource =
  DescribeComplianceByResource'
    { resourceId =
        Core.Nothing,
      nextToken = Core.Nothing,
      complianceTypes = Core.Nothing,
      resourceType = Core.Nothing,
      limit = Core.Nothing
    }

-- | The ID of the AWS resource for which you want compliance information.
-- You can specify only one resource ID. If you specify a resource ID, you
-- must also specify a type for @ResourceType@.
describeComplianceByResource_resourceId :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Core.Text)
describeComplianceByResource_resourceId = Lens.lens (\DescribeComplianceByResource' {resourceId} -> resourceId) (\s@DescribeComplianceByResource' {} a -> s {resourceId = a} :: DescribeComplianceByResource)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeComplianceByResource_nextToken :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Core.Text)
describeComplianceByResource_nextToken = Lens.lens (\DescribeComplianceByResource' {nextToken} -> nextToken) (\s@DescribeComplianceByResource' {} a -> s {nextToken = a} :: DescribeComplianceByResource)

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @INSUFFICIENT_DATA@.
describeComplianceByResource_complianceTypes :: Lens.Lens' DescribeComplianceByResource (Core.Maybe [ComplianceType])
describeComplianceByResource_complianceTypes = Lens.lens (\DescribeComplianceByResource' {complianceTypes} -> complianceTypes) (\s@DescribeComplianceByResource' {} a -> s {complianceTypes = a} :: DescribeComplianceByResource) Core.. Lens.mapping Lens._Coerce

-- | The types of AWS resources for which you want compliance information
-- (for example, @AWS::EC2::Instance@). For this action, you can specify
-- that the resource type is an AWS account by specifying @AWS::::Account@.
describeComplianceByResource_resourceType :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Core.Text)
describeComplianceByResource_resourceType = Lens.lens (\DescribeComplianceByResource' {resourceType} -> resourceType) (\s@DescribeComplianceByResource' {} a -> s {resourceType = a} :: DescribeComplianceByResource)

-- | The maximum number of evaluation results returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
describeComplianceByResource_limit :: Lens.Lens' DescribeComplianceByResource (Core.Maybe Core.Natural)
describeComplianceByResource_limit = Lens.lens (\DescribeComplianceByResource' {limit} -> limit) (\s@DescribeComplianceByResource' {} a -> s {limit = a} :: DescribeComplianceByResource)

instance Core.AWSPager DescribeComplianceByResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeComplianceByResourceResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeComplianceByResourceResponse_complianceByResources
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeComplianceByResource_nextToken
          Lens..~ rs
          Lens.^? describeComplianceByResourceResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeComplianceByResource where
  type
    AWSResponse DescribeComplianceByResource =
      DescribeComplianceByResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComplianceByResourceResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ComplianceByResources"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeComplianceByResource

instance Core.NFData DescribeComplianceByResource

instance Core.ToHeaders DescribeComplianceByResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeComplianceByResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeComplianceByResource where
  toJSON DescribeComplianceByResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceId" Core..=) Core.<$> resourceId,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
            ("ResourceType" Core..=) Core.<$> resourceType,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeComplianceByResource where
  toPath = Core.const "/"

instance Core.ToQuery DescribeComplianceByResource where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeComplianceByResourceResponse' smart constructor.
data DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Indicates whether the specified AWS resource complies with all of the
    -- AWS Config rules that evaluate it.
    complianceByResources :: Core.Maybe [ComplianceByResource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeComplianceByResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComplianceByResourceResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'complianceByResources', 'describeComplianceByResourceResponse_complianceByResources' - Indicates whether the specified AWS resource complies with all of the
-- AWS Config rules that evaluate it.
--
-- 'httpStatus', 'describeComplianceByResourceResponse_httpStatus' - The response's http status code.
newDescribeComplianceByResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeComplianceByResourceResponse
newDescribeComplianceByResourceResponse pHttpStatus_ =
  DescribeComplianceByResourceResponse'
    { nextToken =
        Core.Nothing,
      complianceByResources = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeComplianceByResourceResponse_nextToken :: Lens.Lens' DescribeComplianceByResourceResponse (Core.Maybe Core.Text)
describeComplianceByResourceResponse_nextToken = Lens.lens (\DescribeComplianceByResourceResponse' {nextToken} -> nextToken) (\s@DescribeComplianceByResourceResponse' {} a -> s {nextToken = a} :: DescribeComplianceByResourceResponse)

-- | Indicates whether the specified AWS resource complies with all of the
-- AWS Config rules that evaluate it.
describeComplianceByResourceResponse_complianceByResources :: Lens.Lens' DescribeComplianceByResourceResponse (Core.Maybe [ComplianceByResource])
describeComplianceByResourceResponse_complianceByResources = Lens.lens (\DescribeComplianceByResourceResponse' {complianceByResources} -> complianceByResources) (\s@DescribeComplianceByResourceResponse' {} a -> s {complianceByResources = a} :: DescribeComplianceByResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeComplianceByResourceResponse_httpStatus :: Lens.Lens' DescribeComplianceByResourceResponse Core.Int
describeComplianceByResourceResponse_httpStatus = Lens.lens (\DescribeComplianceByResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeComplianceByResourceResponse' {} a -> s {httpStatus = a} :: DescribeComplianceByResourceResponse)

instance
  Core.NFData
    DescribeComplianceByResourceResponse
