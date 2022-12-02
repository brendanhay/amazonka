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
-- Module      : Amazonka.Config.DescribeComplianceByResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified Amazon Web Services resources are
-- compliant. If a resource is noncompliant, this action returns the number
-- of Config rules that the resource does not comply with.
--
-- A resource is compliant if it complies with all the Config rules that
-- evaluate it. It is noncompliant if it does not comply with one or more
-- of these rules.
--
-- If Config has no current evaluation results for the resource, it returns
-- @INSUFFICIENT_DATA@. This result might indicate one of the following
-- conditions about the rules that evaluate the resource:
--
-- -   Config has never invoked an evaluation for the rule. To check
--     whether it has, use the @DescribeConfigRuleEvaluationStatus@ action
--     to get the @LastSuccessfulInvocationTime@ and
--     @LastFailedInvocationTime@.
--
-- -   The rule\'s Lambda function is failing to send evaluation results to
--     Config. Verify that the role that you assigned to your configuration
--     recorder includes the @config:PutEvaluations@ permission. If the
--     rule is a custom rule, verify that the Lambda execution role
--     includes the @config:PutEvaluations@ permission.
--
-- -   The rule\'s Lambda function has returned @NOT_APPLICABLE@ for all
--     evaluation results. This can occur if the resources were deleted or
--     removed from the rule\'s scope.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeComplianceByResource
  ( -- * Creating a Request
    DescribeComplianceByResource (..),
    newDescribeComplianceByResource,

    -- * Request Lenses
    describeComplianceByResource_resourceId,
    describeComplianceByResource_resourceType,
    describeComplianceByResource_nextToken,
    describeComplianceByResource_complianceTypes,
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeComplianceByResource' smart constructor.
data DescribeComplianceByResource = DescribeComplianceByResource'
  { -- | The ID of the Amazon Web Services resource for which you want compliance
    -- information. You can specify only one resource ID. If you specify a
    -- resource ID, you must also specify a type for @ResourceType@.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The types of Amazon Web Services resources for which you want compliance
    -- information (for example, @AWS::EC2::Instance@). For this action, you
    -- can specify that the resource type is an Amazon Web Services account by
    -- specifying @AWS::::Account@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
    -- @INSUFFICIENT_DATA@.
    complianceTypes :: Prelude.Maybe [ComplianceType],
    -- | The maximum number of evaluation results returned on each page. The
    -- default is 10. You cannot specify a number greater than 100. If you
    -- specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComplianceByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeComplianceByResource_resourceId' - The ID of the Amazon Web Services resource for which you want compliance
-- information. You can specify only one resource ID. If you specify a
-- resource ID, you must also specify a type for @ResourceType@.
--
-- 'resourceType', 'describeComplianceByResource_resourceType' - The types of Amazon Web Services resources for which you want compliance
-- information (for example, @AWS::EC2::Instance@). For this action, you
-- can specify that the resource type is an Amazon Web Services account by
-- specifying @AWS::::Account@.
--
-- 'nextToken', 'describeComplianceByResource_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceTypes', 'describeComplianceByResource_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @INSUFFICIENT_DATA@.
--
-- 'limit', 'describeComplianceByResource_limit' - The maximum number of evaluation results returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
newDescribeComplianceByResource ::
  DescribeComplianceByResource
newDescribeComplianceByResource =
  DescribeComplianceByResource'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      complianceTypes = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services resource for which you want compliance
-- information. You can specify only one resource ID. If you specify a
-- resource ID, you must also specify a type for @ResourceType@.
describeComplianceByResource_resourceId :: Lens.Lens' DescribeComplianceByResource (Prelude.Maybe Prelude.Text)
describeComplianceByResource_resourceId = Lens.lens (\DescribeComplianceByResource' {resourceId} -> resourceId) (\s@DescribeComplianceByResource' {} a -> s {resourceId = a} :: DescribeComplianceByResource)

-- | The types of Amazon Web Services resources for which you want compliance
-- information (for example, @AWS::EC2::Instance@). For this action, you
-- can specify that the resource type is an Amazon Web Services account by
-- specifying @AWS::::Account@.
describeComplianceByResource_resourceType :: Lens.Lens' DescribeComplianceByResource (Prelude.Maybe Prelude.Text)
describeComplianceByResource_resourceType = Lens.lens (\DescribeComplianceByResource' {resourceType} -> resourceType) (\s@DescribeComplianceByResource' {} a -> s {resourceType = a} :: DescribeComplianceByResource)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeComplianceByResource_nextToken :: Lens.Lens' DescribeComplianceByResource (Prelude.Maybe Prelude.Text)
describeComplianceByResource_nextToken = Lens.lens (\DescribeComplianceByResource' {nextToken} -> nextToken) (\s@DescribeComplianceByResource' {} a -> s {nextToken = a} :: DescribeComplianceByResource)

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @INSUFFICIENT_DATA@.
describeComplianceByResource_complianceTypes :: Lens.Lens' DescribeComplianceByResource (Prelude.Maybe [ComplianceType])
describeComplianceByResource_complianceTypes = Lens.lens (\DescribeComplianceByResource' {complianceTypes} -> complianceTypes) (\s@DescribeComplianceByResource' {} a -> s {complianceTypes = a} :: DescribeComplianceByResource) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of evaluation results returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
describeComplianceByResource_limit :: Lens.Lens' DescribeComplianceByResource (Prelude.Maybe Prelude.Natural)
describeComplianceByResource_limit = Lens.lens (\DescribeComplianceByResource' {limit} -> limit) (\s@DescribeComplianceByResource' {} a -> s {limit = a} :: DescribeComplianceByResource)

instance Core.AWSPager DescribeComplianceByResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeComplianceByResourceResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeComplianceByResourceResponse_complianceByResources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeComplianceByResource_nextToken
          Lens..~ rs
          Lens.^? describeComplianceByResourceResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeComplianceByResource where
  type
    AWSResponse DescribeComplianceByResource =
      DescribeComplianceByResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComplianceByResourceResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ComplianceByResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeComplianceByResource
  where
  hashWithSalt _salt DescribeComplianceByResource' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` complianceTypes
      `Prelude.hashWithSalt` limit

instance Prelude.NFData DescribeComplianceByResource where
  rnf DescribeComplianceByResource' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf complianceTypes
      `Prelude.seq` Prelude.rnf limit

instance Data.ToHeaders DescribeComplianceByResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeComplianceByResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeComplianceByResource where
  toJSON DescribeComplianceByResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceId" Data..=) Prelude.<$> resourceId,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ComplianceTypes" Data..=)
              Prelude.<$> complianceTypes,
            ("Limit" Data..=) Prelude.<$> limit
          ]
      )

instance Data.ToPath DescribeComplianceByResource where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeComplianceByResource where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeComplianceByResourceResponse' smart constructor.
data DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the specified Amazon Web Services resource complies
    -- with all of the Config rules that evaluate it.
    complianceByResources :: Prelude.Maybe [ComplianceByResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'complianceByResources', 'describeComplianceByResourceResponse_complianceByResources' - Indicates whether the specified Amazon Web Services resource complies
-- with all of the Config rules that evaluate it.
--
-- 'httpStatus', 'describeComplianceByResourceResponse_httpStatus' - The response's http status code.
newDescribeComplianceByResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComplianceByResourceResponse
newDescribeComplianceByResourceResponse pHttpStatus_ =
  DescribeComplianceByResourceResponse'
    { nextToken =
        Prelude.Nothing,
      complianceByResources =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeComplianceByResourceResponse_nextToken :: Lens.Lens' DescribeComplianceByResourceResponse (Prelude.Maybe Prelude.Text)
describeComplianceByResourceResponse_nextToken = Lens.lens (\DescribeComplianceByResourceResponse' {nextToken} -> nextToken) (\s@DescribeComplianceByResourceResponse' {} a -> s {nextToken = a} :: DescribeComplianceByResourceResponse)

-- | Indicates whether the specified Amazon Web Services resource complies
-- with all of the Config rules that evaluate it.
describeComplianceByResourceResponse_complianceByResources :: Lens.Lens' DescribeComplianceByResourceResponse (Prelude.Maybe [ComplianceByResource])
describeComplianceByResourceResponse_complianceByResources = Lens.lens (\DescribeComplianceByResourceResponse' {complianceByResources} -> complianceByResources) (\s@DescribeComplianceByResourceResponse' {} a -> s {complianceByResources = a} :: DescribeComplianceByResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeComplianceByResourceResponse_httpStatus :: Lens.Lens' DescribeComplianceByResourceResponse Prelude.Int
describeComplianceByResourceResponse_httpStatus = Lens.lens (\DescribeComplianceByResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeComplianceByResourceResponse' {} a -> s {httpStatus = a} :: DescribeComplianceByResourceResponse)

instance
  Prelude.NFData
    DescribeComplianceByResourceResponse
  where
  rnf DescribeComplianceByResourceResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf complianceByResources
      `Prelude.seq` Prelude.rnf httpStatus
