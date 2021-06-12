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
-- Module      : Network.AWS.Config.DescribeComplianceByConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified AWS Config rules are compliant. If a
-- rule is noncompliant, this action returns the number of AWS resources
-- that do not comply with the rule.
--
-- A rule is compliant if all of the evaluated resources comply with it. It
-- is noncompliant if any of these resources do not comply.
--
-- If AWS Config has no current evaluation results for the rule, it returns
-- @INSUFFICIENT_DATA@. This result might indicate one of the following
-- conditions:
--
-- -   AWS Config has never invoked an evaluation for the rule. To check
--     whether it has, use the @DescribeConfigRuleEvaluationStatus@ action
--     to get the @LastSuccessfulInvocationTime@ and
--     @LastFailedInvocationTime@.
--
-- -   The rule\'s AWS Lambda function is failing to send evaluation
--     results to AWS Config. Verify that the role you assigned to your
--     configuration recorder includes the @config:PutEvaluations@
--     permission. If the rule is a custom rule, verify that the AWS Lambda
--     execution role includes the @config:PutEvaluations@ permission.
--
-- -   The rule\'s AWS Lambda function has returned @NOT_APPLICABLE@ for
--     all evaluation results. This can occur if the resources were deleted
--     or removed from the rule\'s scope.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeComplianceByConfigRule
  ( -- * Creating a Request
    DescribeComplianceByConfigRule (..),
    newDescribeComplianceByConfigRule,

    -- * Request Lenses
    describeComplianceByConfigRule_nextToken,
    describeComplianceByConfigRule_complianceTypes,
    describeComplianceByConfigRule_configRuleNames,

    -- * Destructuring the Response
    DescribeComplianceByConfigRuleResponse (..),
    newDescribeComplianceByConfigRuleResponse,

    -- * Response Lenses
    describeComplianceByConfigRuleResponse_nextToken,
    describeComplianceByConfigRuleResponse_complianceByConfigRules,
    describeComplianceByConfigRuleResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeComplianceByConfigRule' smart constructor.
data DescribeComplianceByConfigRule = DescribeComplianceByConfigRule'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    complianceTypes :: Core.Maybe [ComplianceType],
    -- | Specify one or more AWS Config rule names to filter the results by rule.
    configRuleNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeComplianceByConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComplianceByConfigRule_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceTypes', 'describeComplianceByConfigRule_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
--
-- 'configRuleNames', 'describeComplianceByConfigRule_configRuleNames' - Specify one or more AWS Config rule names to filter the results by rule.
newDescribeComplianceByConfigRule ::
  DescribeComplianceByConfigRule
newDescribeComplianceByConfigRule =
  DescribeComplianceByConfigRule'
    { nextToken =
        Core.Nothing,
      complianceTypes = Core.Nothing,
      configRuleNames = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeComplianceByConfigRule_nextToken :: Lens.Lens' DescribeComplianceByConfigRule (Core.Maybe Core.Text)
describeComplianceByConfigRule_nextToken = Lens.lens (\DescribeComplianceByConfigRule' {nextToken} -> nextToken) (\s@DescribeComplianceByConfigRule' {} a -> s {nextToken = a} :: DescribeComplianceByConfigRule)

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
describeComplianceByConfigRule_complianceTypes :: Lens.Lens' DescribeComplianceByConfigRule (Core.Maybe [ComplianceType])
describeComplianceByConfigRule_complianceTypes = Lens.lens (\DescribeComplianceByConfigRule' {complianceTypes} -> complianceTypes) (\s@DescribeComplianceByConfigRule' {} a -> s {complianceTypes = a} :: DescribeComplianceByConfigRule) Core.. Lens.mapping Lens._Coerce

-- | Specify one or more AWS Config rule names to filter the results by rule.
describeComplianceByConfigRule_configRuleNames :: Lens.Lens' DescribeComplianceByConfigRule (Core.Maybe [Core.Text])
describeComplianceByConfigRule_configRuleNames = Lens.lens (\DescribeComplianceByConfigRule' {configRuleNames} -> configRuleNames) (\s@DescribeComplianceByConfigRule' {} a -> s {configRuleNames = a} :: DescribeComplianceByConfigRule) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeComplianceByConfigRule where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeComplianceByConfigRuleResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeComplianceByConfigRuleResponse_complianceByConfigRules
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeComplianceByConfigRule_nextToken
          Lens..~ rs
          Lens.^? describeComplianceByConfigRuleResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeComplianceByConfigRule
  where
  type
    AWSResponse DescribeComplianceByConfigRule =
      DescribeComplianceByConfigRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComplianceByConfigRuleResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ComplianceByConfigRules"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeComplianceByConfigRule

instance Core.NFData DescribeComplianceByConfigRule

instance
  Core.ToHeaders
    DescribeComplianceByConfigRule
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeComplianceByConfigRule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeComplianceByConfigRule where
  toJSON DescribeComplianceByConfigRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
            ("ConfigRuleNames" Core..=)
              Core.<$> configRuleNames
          ]
      )

instance Core.ToPath DescribeComplianceByConfigRule where
  toPath = Core.const "/"

instance Core.ToQuery DescribeComplianceByConfigRule where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeComplianceByConfigRuleResponse' smart constructor.
data DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Indicates whether each of the specified AWS Config rules is compliant.
    complianceByConfigRules :: Core.Maybe [ComplianceByConfigRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeComplianceByConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComplianceByConfigRuleResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'complianceByConfigRules', 'describeComplianceByConfigRuleResponse_complianceByConfigRules' - Indicates whether each of the specified AWS Config rules is compliant.
--
-- 'httpStatus', 'describeComplianceByConfigRuleResponse_httpStatus' - The response's http status code.
newDescribeComplianceByConfigRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeComplianceByConfigRuleResponse
newDescribeComplianceByConfigRuleResponse
  pHttpStatus_ =
    DescribeComplianceByConfigRuleResponse'
      { nextToken =
          Core.Nothing,
        complianceByConfigRules =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeComplianceByConfigRuleResponse_nextToken :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Core.Maybe Core.Text)
describeComplianceByConfigRuleResponse_nextToken = Lens.lens (\DescribeComplianceByConfigRuleResponse' {nextToken} -> nextToken) (\s@DescribeComplianceByConfigRuleResponse' {} a -> s {nextToken = a} :: DescribeComplianceByConfigRuleResponse)

-- | Indicates whether each of the specified AWS Config rules is compliant.
describeComplianceByConfigRuleResponse_complianceByConfigRules :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Core.Maybe [ComplianceByConfigRule])
describeComplianceByConfigRuleResponse_complianceByConfigRules = Lens.lens (\DescribeComplianceByConfigRuleResponse' {complianceByConfigRules} -> complianceByConfigRules) (\s@DescribeComplianceByConfigRuleResponse' {} a -> s {complianceByConfigRules = a} :: DescribeComplianceByConfigRuleResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeComplianceByConfigRuleResponse_httpStatus :: Lens.Lens' DescribeComplianceByConfigRuleResponse Core.Int
describeComplianceByConfigRuleResponse_httpStatus = Lens.lens (\DescribeComplianceByConfigRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeComplianceByConfigRuleResponse' {} a -> s {httpStatus = a} :: DescribeComplianceByConfigRuleResponse)

instance
  Core.NFData
    DescribeComplianceByConfigRuleResponse
